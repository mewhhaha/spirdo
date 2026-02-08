{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.List (find)
import Data.Word (Word32)
import qualified Data.ByteString as BS
import Foreign.Ptr (nullPtr, wordPtrToPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Slop hiding (Shader, V4)
import Slop.SDL.Raw (GPUDevice(..), GPUSampler(..), GPUTexture(..))

import Spirdo.Wesl.Reflection
  ( BindingInfo(..)
  , BindingPlan(..)
  , Shader
  , SomeShader(..)
  , ShaderInterface(..)
  , SamplerBindingMode(..)
  , shaderSpirv
  , shaderPlan
  , shaderInterface
  )
import Spirdo.Wesl.Uniform
  ( ToUniform
  , V4(..)
  )
import Spirdo.Wesl.Inputs
  ( SamplerHandle(..)
  , TextureHandle(..)
  , StorageTextureHandle(..)
  , RequireUniform
  , UniformInput(..)
  , SamplerInput(..)
  , TextureInput(..)
  , StorageTextureInput(..)
  , ShaderInputs
  , InputsBuilder
  , inputsFor
  , inputsInterface
  , inputsSamplers
  , inputsStorageTextures
  , inputsTextures
  , orderedUniforms
  )
import qualified Spirdo.Wesl.Inputs as Inputs

import Examples.Compute (computeParticlesShader, computeShader)
import Examples.Fragments
import Examples.Vertex (vertexFullscreenShader, vertexShader)

shouldWriteSpv :: IO Bool
shouldWriteSpv = do
  env <- lookupEnv "SPIRDO_WRITE_SPV"
  pure $
    case fmap (map toLower) env of
      Just "1" -> True
      Just "true" -> True
      Just "yes" -> True
      _ -> False

spvOutputs :: [(FilePath, SomeShader)]
spvOutputs =
  [ ("fragment-0.spv", SomeShader fragmentGradientBloomShader)
  , ("fragment-1.spv", SomeShader fragmentCirclePulseShader)
  , ("fragment-2.spv", SomeShader fragmentInlineImportsShader)
  , ("fragment-3.spv", SomeShader fragmentSpectrumShiftShader)
  , ("fragment-4.spv", SomeShader fragmentSineWavesShader)
  , ("fragment-5.spv", SomeShader fragmentCheckerWarpShader)
  , ("fragment-6.spv", SomeShader fragmentRippleCausticsShader)
  , ("fragment-7.spv", SomeShader fragmentPlasmaStormShader)
  , ("fragment-8.spv", SomeShader fragmentVignetteGlowShader)
  , ("fragment-9.spv", SomeShader fragmentNoiseFlowShader)
  , ("fragment-10.spv", SomeShader fragmentSwirlVortexShader)
  , ("fragment-11.spv", SomeShader fragmentMetaballsShader)
  , ("fragment-12.spv", SomeShader fragmentKaleidoscopeShader)
  , ("compute-1.spv", SomeShader computeShader)
  , ("compute-2.spv", SomeShader computeParticlesShader)
  , ("vertex-1.spv", SomeShader vertexShader)
  , ("vertex-2.spv", SomeShader vertexFullscreenShader)
  ]

writeSpvOutputs :: IO ()
writeSpvOutputs =
  forM_ spvOutputs $ \(path, SomeShader shader) ->
    BS.writeFile path (shaderSpirv shader)

-- Uniforms shared by fragment variants.
data ParamsU = ParamsU
  { time_res :: V4 Float
  , color :: V4 Float
  } deriving (Generic)

instance ToUniform ParamsU

-- Vertex data for fullscreen quad.
newtype Vertex = Vertex (Float, Float, Float, Float)

instance Storable Vertex where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr =
    Vertex
      <$> ( (,,,)
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 4
              <*> peekByteOff ptr 8
              <*> peekByteOff ptr 12
          )
  poke ptr (Vertex (x, y, u, v)) =
    pokeByteOff ptr 0 x
      >> pokeByteOff ptr 4 y
      >> pokeByteOff ptr 8 u
      >> pokeByteOff ptr 12 v

vertexLayout :: VertexLayout
vertexLayout =
  VertexLayout
    { layoutStride = 16
    , layoutAttrs =
        [ VertexAttr 0 VertexFloat2 0
        , VertexAttr 1 VertexFloat2 8
        ]
    }

quadVertices :: [Vertex]
quadVertices = fmap Vertex
  [ (-1, -1, 0, 0)
  , (1, -1, 1, 0)
  , (1, 1, 1, 1)
  , (-1, -1, 0, 0)
  , (1, 1, 1, 1)
  , (-1, 1, 0, 1)
  ]

data Variant where
  Variant :: RequireUniform "params" iface => String -> BlendMode -> Pipeline -> Shader mode iface -> (ParamsU -> InputsBuilder mode iface) -> Variant

data FragmentVariant where
  FragmentVariant :: RequireUniform "params" iface => String -> BlendMode -> Shader mode iface -> (ParamsU -> InputsBuilder mode iface) -> FragmentVariant

data History = History
  { historyTarget :: RenderTarget
  , historySize :: (Int, Int)
  , historyValid :: Bool
  }

data DemoState = DemoState
  { stateIndex :: Int
  , stateHistory :: Maybe History
  }

main :: IO ()
main = do
  emitSpv <- shouldWriteSpv
  when emitSpv writeSpvOutputs
  let cfg =
        defaultConfig
          { windowTitle = "Spirdo Slop demo"
          , windowWidth = 960
          , windowHeight = 540
          , windowResizable = True
          }
  runWindow cfg $ do
    mesh <- createMesh vertexLayout quadVertices
    let vprep = vertexShader
    vshader <- createVertexShader (shaderSpirv vprep) (countsFromShader vprep)

    let baseVariants =
          [ FragmentVariant "Gradient Bloom" BlendNone fragmentGradientBloomShader (Inputs.uniform @"params")
          , FragmentVariant "Circle Pulse" BlendNone fragmentCirclePulseShader (Inputs.uniform @"params")
          , FragmentVariant "Inline Imports" BlendNone fragmentInlineImportsShader (Inputs.uniform @"params")
          , FragmentVariant "Spectrum Shift" BlendNone fragmentSpectrumShiftShader (Inputs.uniform @"params")
          , FragmentVariant "Sine Waves" BlendNone fragmentSineWavesShader (Inputs.uniform @"params")
          , FragmentVariant "Checker Warp" BlendNone fragmentCheckerWarpShader (Inputs.uniform @"params")
          , FragmentVariant "Ripple Caustics" BlendNone fragmentRippleCausticsShader (Inputs.uniform @"params")
          , FragmentVariant "Plasma Storm" BlendNone fragmentPlasmaStormShader (Inputs.uniform @"params")
          , FragmentVariant "Vignette Glow" BlendNone fragmentVignetteGlowShader (Inputs.uniform @"params")
          , FragmentVariant "Noise Flow" BlendNone fragmentNoiseFlowShader (Inputs.uniform @"params")
          , FragmentVariant "Swirl Vortex" BlendNone fragmentSwirlVortexShader (Inputs.uniform @"params")
          , FragmentVariant "Metaballs" BlendNone fragmentMetaballsShader (Inputs.uniform @"params")
          , FragmentVariant "Kaleidoscope" BlendNone fragmentKaleidoscopeShader (Inputs.uniform @"params")
          ]

    variants <- mapM (\(FragmentVariant name blend shader mkInputs) -> mkVariant name blend shader mkInputs vshader) baseVariants
    case variants of
      (Variant vName _ _ _ _ : _) -> liftIO (putStrLn ("Demo: " <> vName))
      [] -> pure ()

    _ <- loop (DemoState 0 Nothing) $ \frame state -> do
      let count = length variants
      let delta
            | keyPressed KeyLeft frame.input = -1
            | keyPressed KeyRight frame.input = 1
            | otherwise = 0
      let idx' = if count == 0 then 0 else (state.stateIndex + delta + count) `mod` count
      case variants !! idx' of
        Variant vName vBlend vPipeline fprep mkInputs -> do
          when (idx' /= state.stateIndex) $
            liftLoop (liftIO (putStrLn ("Demo: " <> vName)))
          let (w, h) = frame.renderSize
          let size = (w, h)
          let useTemporal = vBlend == BlendAlpha
          historyOpt <-
            if useTemporal
              then liftLoop (Just <$> ensureHistory size state.stateHistory)
              else pure (resetHistory state.stateHistory)
          let mixFactor = if useTemporal && historyValidFrom historyOpt then 0.8 else 0.0
          let params =
                ParamsU
                  { time_res = V4 frame.time (fromIntegral w) (fromIntegral h) mixFactor
                  , color = V4 1 1 1 1
                  }
          case inputsFor fprep (mkInputs params) of
            Left err -> error (vName <> ": " <> err.ieMessage)
            Right inputs -> do
              if useTemporal
                then do
                  let target = historyTargetFrom historyOpt
                  render target $ do
                    if historyValidFrom historyOpt
                      then pure ()
                      else clear (rgb 0.0 0.0 0.0)
                    drawMesh vPipeline mesh (bindingsFromInputs inputs)
                  drawRender target Nothing (fullscreenRect size)
                  pure (Continue (DemoState idx' (Just (historyUpdated historyOpt True))))
                else do
                  clear (rgb 0.06 0.07 0.1)
                  drawMesh vPipeline mesh (bindingsFromInputs inputs)
                  pure (Continue (DemoState idx' historyOpt))
    pure ()

mkVariant :: RequireUniform "params" iface => String -> BlendMode -> Shader mode iface -> (ParamsU -> InputsBuilder mode iface) -> VertexShader -> WindowM Variant
mkVariant name blend prep mkInputs vshader = do
  fshader <- createFragmentShader (shaderSpirv prep) (countsFromShader prep)
  pipeline <- graphicsPipeline
    GraphicsDesc
      { gfxVertex = vshader
      , gfxFragment = fshader
      , gfxLayout = vertexLayout
      , gfxPrimitive = PrimTriangles
      , gfxTarget = TargetSwapchain
      , gfxBlend = blend
      , gfxDepth = DepthNone
      , gfxDepthFormat = 0
      }
  pure (Variant name blend pipeline prep mkInputs)

ensureHistory :: (Int, Int) -> Maybe History -> WindowM History
ensureHistory size existing =
  case existing of
    Just h | h.historySize == size -> pure h
    Just h -> do
      destroyTarget h.historyTarget
      target <- uncurry createRenderTarget size
      pure (History target size False)
    Nothing -> do
      target <- uncurry createRenderTarget size
      pure (History target size False)

resetHistory :: Maybe History -> Maybe History
resetHistory = fmap (\h -> h { historyValid = False })

historyValidFrom :: Maybe History -> Bool
historyValidFrom = maybe False (\h -> h.historyValid)

historyTargetFrom :: Maybe History -> RenderTarget
historyTargetFrom history =
  case history of
    Just h -> h.historyTarget
    Nothing -> error "missing history render target"

historyUpdated :: Maybe History -> Bool -> History
historyUpdated history valid =
  case history of
    Just h -> h { historyValid = valid }
    Nothing -> error "missing history render target"


countsFromShader :: Shader mode iface -> ShaderCounts
countsFromShader shader =
  let bindingPlan = shaderPlan shader
      iface = shaderInterface shader
      BindingPlan
        { bpSamplers = planSamplers
        , bpTextures = planTextures
        , bpStorageTextures = planStorageTextures
        , bpStorageBuffers = planStorageBuffers
        , bpUniforms = planUniforms
        } = bindingPlan
      ShaderInterface { siSamplerMode = samplerMode } = iface
      samplerBindings =
        case samplerMode of
          SamplerCombined -> planTextures
          SamplerSeparate -> planSamplers
      samplerCount = bindingCount samplerBindings
      storageTextureCount = bindingCount planStorageTextures
      storageBufferCount = bindingCount planStorageBuffers
      uniformCount = bindingCount planUniforms
  in ShaderCounts
      samplerCount
      storageTextureCount
      storageBufferCount
      uniformCount
  where
    bindingCount :: [BindingInfo] -> Word32
    bindingCount [] = 0
    bindingCount xs = maximum (map (.biBinding) xs) + 1

bindingsFromInputs :: ShaderInputs iface -> [Binding]
bindingsFromInputs inputs =
  let iface = inputsInterface inputs
      uniforms =
        [ fUniformBytes u.uiBinding u.uiBytes
        | u <- orderedUniforms inputs
        ]
      samplers =
        case iface of
          ShaderInterface { siSamplerMode = SamplerCombined } ->
            [ fSamplerWith t.textureBinding (textureFromHandle t.textureHandle) (samplerFromHandle (resolveSampler t))
            | t <- inputsTextures inputs
            ]
          ShaderInterface { siSamplerMode = SamplerSeparate } ->
            [ fSamplerWith s.samplerBinding (textureFromHandle (TextureHandle (samplerTex s))) (samplerFromHandle s.samplerHandle)
            | s <- inputsSamplers inputs
            ]
      storage =
        [ fStorageTexture t.storageTextureBinding (storageTextureFromHandle t.storageTextureHandle)
        | t <- inputsStorageTextures inputs
        ]
  in uniforms <> samplers <> storage
  where
    resolveSampler texInput =
      case texInput.textureSampler of
        Just handle -> handle
        Nothing ->
          case findSamplerForTexture texInput.textureName (inputsSamplers inputs) of
            Just handle -> handle
            Nothing -> error ("missing sampler for texture: " <> texInput.textureName)
    samplerTex (SamplerInput samplerName _ _ _) =
      maybe 0 unTexture (findSamplerTexture samplerName (inputsTextures inputs))
    unTexture (TextureInput _ _ _ textureHandle _) =
      case textureHandle of
        TextureHandle v -> v
    findSamplerTexture name textures =
      findTexture name textures
        <|> (stripSuffix "_sampler" name >>= \base -> findTexture base textures)
        <|> (stripSuffix "Sampler" name >>= \base -> findTexture base textures)
    findSamplerForTexture name samplers =
      findSampler name samplers
        <|> findSampler (name <> "_sampler") samplers
        <|> findSampler (name <> "Sampler") samplers
    findSampler name samplers =
      (.samplerHandle) <$> find (\s -> s.samplerName == name) samplers

    findTexture name =
      foldr (\t acc -> if t.textureName == name then Just t else acc) Nothing

    stripSuffix suffix value =
      let suffixLen = length suffix
          valueLen = length value
      in if valueLen >= suffixLen && drop (valueLen - suffixLen) value == suffix
          then Just (take (valueLen - suffixLen) value)
          else Nothing

textureFromHandle :: TextureHandle -> Texture
textureFromHandle (TextureHandle value) =
  Texture
    { textureHandle = GPUTexture (wordPtrToPtr (fromIntegral value))
    , textureDevice = GPUDevice nullPtr
    , textureWidth = 0
    , textureHeight = 0
    , textureDepth = 0
  }

storageTextureFromHandle :: StorageTextureHandle -> Texture
storageTextureFromHandle (StorageTextureHandle value) =
  Texture
    { textureHandle = GPUTexture (wordPtrToPtr (fromIntegral value))
    , textureDevice = GPUDevice nullPtr
    , textureWidth = 0
    , textureHeight = 0
    , textureDepth = 0
    }

samplerFromHandle :: SamplerHandle -> Sampler
samplerFromHandle (SamplerHandle value) =
  Sampler (GPUSampler (wordPtrToPtr (fromIntegral value)))
