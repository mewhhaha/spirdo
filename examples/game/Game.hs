{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word32)
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
import Examples.Game.Logic
  ( Crystal(..)
  , GameInput(..)
  , GameState
  , GroundPosition(..)
  , MoveDirection(..)
  , activeCrystals
  , advanceGame
  , gamePlayerPosition
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

main :: IO ()
main = do
  let cfg =
        defaultConfig
          { windowTitle = "Spirdo Crystal Run — WASD/arrows move, R resets"
          , windowWidth = 960
          , windowHeight = 540
          , windowResizable = True
          }
  runWindow cfg $ do
    meshes <- createGameMeshes
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

    _ <- loop initialGame (gameFrame meshes pipeline)
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

gameFrame :: GameMeshes -> Pipeline -> Frame -> GameState -> Loop (LoopControl GameState)
gameFrame meshes pipeline frame state = do
  let moveDirection = movementFromInput frame.input
      input =
        GameInput
          { move = moveDirection
          , reset = keyPressed KeyR frame.input
          }
      nextState = advanceGame frame.delta input state
      score = gameScore nextState
      previousScore = gameScore state
      playerPosition = gamePlayerPosition nextState
      viewProjection = gameViewProjection frame.renderSize playerPosition

  when (score /= previousScore) $
    liftLoop (liftIO (announceScore score))

  clear (rgb 0.055 0.08 0.15)
  drawModel pipeline meshes.ground viewProjection (scale 1 1 1)
  drawArenaPillars pipeline meshes.pillar viewProjection
  drawCrystals pipeline meshes.crystal viewProjection frame.time (activeCrystals nextState)
  drawShip pipeline meshes.ship viewProjection playerPosition moveDirection
  pure (Continue nextState)

movementFromInput :: InputFrame -> MoveDirection
movementFromInput input =
  MoveDirection
    { x = keyAxis KeyA KeyLeft KeyD KeyRight
    , z = keyAxis KeyW KeyUp KeyS KeyDown
    }
  where
    current = input.inputNow
    keyAxis negativeKey alternateNegativeKey positiveKey alternatePositiveKey =
      keyValue positiveKey current
        + keyValue alternatePositiveKey current
        - keyValue negativeKey current
        - keyValue alternateNegativeKey current

keyValue :: Key -> InputState -> Float
keyValue key current =
  if keyDown key current then 1 else 0

drawShip :: Pipeline -> Mesh -> M44 Float -> GroundPosition -> MoveDirection -> Loop ()
drawShip pipeline mesh viewProjection playerPosition direction =
  drawModel pipeline mesh viewProjection model
  where
    facing
      | direction.x == 0 && direction.z == 0 = 0
      | otherwise = atan2 (-direction.x) (-direction.z)
    bank = (-direction.x) * 0.16
    model =
      translation playerPosition.x 0.48 playerPosition.z
        * rotationY facing
        * rotationZ bank
        * scale 0.78 0.78 0.78

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
  drawMesh pipeline mesh [vUniform 0 (viewProjection * model)]

announceScore :: Int -> IO ()
announceScore score = do
  putStrLn ("Crystals: " <> show score <> "/5")
  when (score == 5) $
    putStrLn "All crystals collected — press R to fly again."
