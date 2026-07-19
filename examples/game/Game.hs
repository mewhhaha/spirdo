{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word32)
import System.Environment (lookupEnv)
import Slop hiding (Shader)
import Spirdo.Wesl.Reflection
  ( BindingInfo
  , BindingPlan(..)
  , BindingSlotCount(..)
  , Shader
  , shaderPlan
  , shaderSpirv
  , singleGroupBindingSlotCount
  )

import Examples.Game.Camera (gameViewProjection)
import Examples.Game.Input (interactiveFrameSeconds, movementFromKeys)
import Examples.Game.Logic
  ( Crystal(..)
  , GameInput(..)
  , GameState
  , GroundPosition(..)
  , MoveDirection(..)
  , activeCrystals
  , advanceGame
  , gamePlayerPosition
  , gamePlayerHeading
  , gameScore
  , initialGame
  )
import Examples.Game.Meshes
  ( cubeVertices
  , crystalVertices
  , groundVertices
  , rotationY
  , rotationZ
  , scale
  , shipModel
  , shipVertices
  , translation
  )
import Examples.Game.Shaders (gameFragmentShader, gameVertexShader)

data GameMeshes = GameMeshes
  { ship :: !Mesh
  , crystal :: !Mesh
  , ground :: !Mesh
  , pillar :: !Mesh
  }

data GameMode
  = Interactive
  | HeadlessCapture !FilePath

main :: IO ()
main = do
  captureReadyFile <- lookupEnv "SPIRDO_CAPTURE_READY_FILE"
  let gameMode = maybe Interactive HeadlessCapture captureReadyFile
      cfg =
        defaultConfig
          { windowTitle = "Spirdo Crystal Run — WASD/arrows move, R resets"
          , windowWidth = 960
          , windowHeight = 540
          , windowResizable = True
          }
  runWindow cfg $ do
    meshes <- createGameMeshes
    startedFrames <- liftIO (newIORef 0)
    -- SDL_gpu's SPIR-V ABI assigns vertex and fragment uniforms to sets 1 and 3.
    vertexShaderCounts <- requireShaderCounts "vertex shader" 1 gameVertexShader
    vertexShader <-
      createVertexShader
        (shaderSpirv gameVertexShader)
        vertexShaderCounts
    fragmentShaderCounts <- requireShaderCounts "fragment shader" 3 gameFragmentShader
    fragmentShader <-
      createFragmentShader
        (shaderSpirv gameFragmentShader)
        fragmentShaderCounts
    pipeline <-
      graphicsPipeline
        GraphicsDesc
          { gfxVertex = vertexShader
          , gfxFragment = fragmentShader
          , gfxLayout = mesh3DLayout
          , gfxPrimitive = PrimTriangles
          , gfxTarget = TargetSwapchain
          , gfxBlend = BlendNone
          , gfxDepth = DepthTestWrite
          , gfxDepthFormat = 0
          }

    liftIO $ do
      putStrLn "Crystal Run: collect all five crystals."
      putStrLn "Move with WASD or the arrow keys; press R to reset."

    _ <- loop initialGame (gameFrame gameMode startedFrames meshes pipeline)
    pure ()

createGameMeshes :: WindowM GameMeshes
createGameMeshes = do
  ship <- createMesh3D shipVertices
  crystal <- createMesh3D crystalVertices
  ground <- createMesh3D groundVertices
  pillar <- createMesh3D cubeVertices
  pure GameMeshes { ship, crystal, ground, pillar }

requireShaderCounts :: String -> Word32 -> Shader mode iface -> WindowM ShaderCounts
requireShaderCounts label uniformGroup shader =
  case shaderCounts uniformGroup shader of
    Left err -> liftIO (ioError (userError (label <> ": " <> err)))
    Right counts -> pure counts

shaderCounts :: Word32 -> Shader mode iface -> Either String ShaderCounts
shaderCounts uniformGroup shader = do
  let bindingPlan = shaderPlan shader
      unsupportedResources =
        filter ((/= 0) . snd)
          [ ("samplers", length bindingPlan.bpSamplers)
          , ("textures", length bindingPlan.bpTextures)
          , ("storage textures", length bindingPlan.bpStorageTextures)
          , ("storage buffers", length bindingPlan.bpStorageBuffers)
          ]
  when (not (null unsupportedResources)) $
    Left ("unsupported non-uniform resources " <> show unsupportedResources)
  uniformCount <- uniformBindingCount uniformGroup bindingPlan.bpUniforms
  pure (ShaderCounts 0 0 0 uniformCount)

uniformBindingCount :: Word32 -> [BindingInfo] -> Either String Word32
uniformBindingCount expectedGroup bindings = do
  slotCount <- singleGroupBindingSlotCount bindings
  case slotCount of
    Nothing -> Right 0
    Just count
      | count.bscGroup /= expectedGroup ->
          Left
            ( "uniform descriptor group "
                <> show count.bscGroup
                <> " does not match renderer group "
                <> show expectedGroup
            )
      | count.bscSlots > fromIntegral (maxBound :: Word32) ->
          Left ("uniform slot count exceeds Word32: " <> show count.bscSlots)
      | otherwise -> Right (fromIntegral count.bscSlots)

gameFrame :: GameMode -> IORef Int -> GameMeshes -> Pipeline -> Frame -> GameState -> Loop (LoopControl GameState)
gameFrame gameMode startedFrames meshes pipeline frame state = do
  liftLoop $ liftIO $
    case gameMode of
      Interactive -> pure ()
      HeadlessCapture readyFile -> do
        startedFrame <- atomicModifyIORef' startedFrames (\count -> let next = count + 1 in (next, next))
        when (startedFrame == 2) (writeFile readyFile "ready\n")

  let moveDirection = movementFromInput frame.input
      frameSeconds =
        case gameMode of
          Interactive -> interactiveFrameSeconds frame.delta
          HeadlessCapture _ -> 0
      animationTime =
        case gameMode of
          Interactive -> frame.time
          HeadlessCapture _ -> 0
      input =
        GameInput
          { move = moveDirection
          , reset = keyPressed KeyR frame.input
          }
      nextState = advanceGame frameSeconds input state
      score = gameScore nextState
      previousScore = gameScore state
      playerPosition = gamePlayerPosition nextState
      playerHeading = gamePlayerHeading nextState
      viewProjection = gameViewProjection frame.renderSize playerPosition

  when (score /= previousScore) $
    liftLoop (liftIO (announceScore score))

  clear (rgb 0.055 0.08 0.15)
  drawModel pipeline meshes.ground viewProjection (scale 1 1 1)
  drawArenaPillars pipeline meshes.pillar viewProjection
  drawCrystals pipeline meshes.crystal viewProjection animationTime (activeCrystals nextState)
  drawShip pipeline meshes.ship viewProjection playerPosition playerHeading moveDirection
  pure (Continue nextState)

movementFromInput :: InputFrame -> MoveDirection
movementFromInput input = movementFromKeys (`keyDown` input.inputNow)

drawShip :: Pipeline -> Mesh -> M44 Float -> GroundPosition -> Float -> MoveDirection -> Loop ()
drawShip pipeline mesh viewProjection playerPosition facing direction =
  drawModel pipeline mesh viewProjection (shipModel playerPosition facing direction)

drawCrystals :: Pipeline -> Mesh -> M44 Float -> Float -> [Crystal] -> Loop ()
drawCrystals pipeline mesh viewProjection gameTime crystals =
  forM_ crystals $ \crystal -> do
    let hover = 1.0 + sin (gameTime * 2.2 + crystal.rotation) * 0.16
        model =
          translation crystal.pos.x hover crystal.pos.z
            * rotationY crystal.rotation
            * rotationZ (crystal.rotation * 0.35)
            * scale 0.72 0.72 0.72
    drawModel pipeline mesh viewProjection model

drawArenaPillars :: Pipeline -> Mesh -> M44 Float -> Loop ()
drawArenaPillars pipeline mesh viewProjection =
  forM_ pillarPositions $ \(x, z) ->
    drawModel
      pipeline
      mesh
      viewProjection
      (translation x 0.65 z * scale 0.5 1.3 0.5)
  where
    pillarPositions =
      [ (-8.4, -8.0)
      , (8.4, -8.0)
      , (-8.4, 8.0)
      , (8.4, 8.0)
      ]

drawModel :: Pipeline -> Mesh -> M44 Float -> M44 Float -> Loop ()
drawModel pipeline mesh viewProjection model =
  drawMesh pipeline mesh [vUniform 0 viewProjection, vUniform 1 model]

announceScore :: Int -> IO ()
announceScore score = do
  putStrLn ("Crystals: " <> show score <> "/5")
  when (score == 5) $
    putStrLn "All crystals collected — press R to fly again."
