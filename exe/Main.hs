{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | Executable entry point.
module Main (main) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Foreign
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CBool(..), CFloat)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

import Spirdo.SDL3
import Spirdo.SDL3.Safe (withSDL, withWindow, withGPURenderer)
import Spirdo.Wesl
  ( BindingInfo(..)
  , BindingPlan(..)
  , CompiledShader(..)
  , CompileOptions(..)
  , OverrideValue(..)
  , PreparedShader(..)
  , SomeCompiledShader(..)
  , ToUniform(..)
  , V4(..)
  , compileWeslToSpirvWith
  , defaultCompileOptions
  , packUniformFrom
  , prepareShader
  , uniform
  )
import Spirdo.Wesl.Inputs (UniformInput(..))
import Examples.Compute
import Examples.Fragments
import Examples.Override (fragmentOverrideSrc)
import Examples.Vertex

data ShaderVariant where
  ShaderVariant ::
    { svName :: String
    , svPrepared :: PreparedShader iface
    , svSamplerCount :: Word32
    , svStorageTextureCount :: Word32
    , svStorageBufferCount :: Word32
    , svUniformCount :: Word32
    , svParamsUniform :: ParamsU -> Either String UniformInput
    } -> ShaderVariant

data VariantState = VariantState
  { vsName :: String
  , vsState :: Ptr SDL_GPURenderState
  , vsShader :: Ptr SDL_GPUShader
  , vsUniformCount :: Word32
  , vsParamsUniform :: ParamsU -> Either String UniformInput
  }

main :: IO ()
main = do
  let overrideVariant label value =
        case compileWeslToSpirvWith (defaultCompileOptions { overrideValues = [("mode", OVI32 value)] }) fragmentOverrideSrc of
          Left err -> error ("override variant " <> label <> ": " <> show err)
          Right shader -> mkVariantDynamic label shader
  let variants =
        [ mkVariant "Feature Mix" fragmentFeatureShader
        , mkVariant "Raymarch" fragmentRaymarchShader
        , mkVariant "Triangle" fragmentTriangleShader
        , mkVariant "Plasma" fragmentPlasmaShader
        , mkVariant "Grid" fragmentGridShader
        , mkVariant "SDF Text" fragmentSdfTextShader
        , mkVariant "Clouds" fragmentCloudShader
        , mkVariant "Bits" fragmentBitsShader
        , mkVariant "Aurora" fragmentAuroraShader
        , mkVariant "Starfield" fragmentStarfieldShader
        , mkVariant "Tunnel" fragmentTunnelShader
        , mkVariant "Voronoi" fragmentVoronoiShader
        , mkVariant "Mandelbrot" fragmentMandelbrotShader
        , mkVariant "Seascape" fragmentSeascapeShader
        , mkVariant "Protean Clouds" fragmentProteanCloudsShader
        , mkVariant "Primitives" fragmentPrimitivesShader
        , mkVariant "Grass" fragmentGrassShader
        , overrideVariant "Override Stripes" 0
        , overrideVariant "Override Rings" 1
        ]
  let computeShaders =
        [ SomeCompiledShader computeShader
        , SomeCompiledShader computeParticlesShader
        , SomeCompiledShader computeTilesShader
        ]
  let vertexShaders =
        [ SomeCompiledShader vertexShader
        , SomeCompiledShader vertexWaveShader
        , SomeCompiledShader vertexFullscreenShader
        ]
  writeFlag <- lookupEnv "SPIRDO_WRITE_SPV"
  let shouldWriteSpv =
        case fmap (map toLower) writeFlag of
          Just "1" -> True
          Just "true" -> True
          Just "yes" -> True
          Just "on" -> True
          _ -> False
  when shouldWriteSpv $ do
    forM_ (zip [0 :: Int ..] variants) $ \(ix, variant) ->
      BS.writeFile ("fragment-" <> show ix <> ".spv") (variantSpirv variant)
    forM_ (zip [0 :: Int ..] computeShaders) $ \(ix, SomeCompiledShader shader) ->
      let suffix = if ix == 0 then "" else "-" <> show ix
      in BS.writeFile ("compute" <> suffix <> ".spv") (shaderSpirv shader)
    forM_ (zip [0 :: Int ..] vertexShaders) $ \(ix, SomeCompiledShader shader) ->
      let suffix = if ix == 0 then "" else "-" <> show ix
      in BS.writeFile ("vertex" <> suffix <> ".spv") (shaderSpirv shader)

  withSDL sdl_INIT_VIDEO $
    withWindow "Spirdo SDL3" 800 600 0 $ \window ->
      withGPURenderer window $ \renderer -> do
        device <- sdlGetGPURendererDevice renderer
        when (device == nullPtr) (sdlFail "sdlGetGPURendererDevice")

        formats <- sdlGetGPUShaderFormats device
        when ((formats .&. sdl_GPU_SHADERFORMAT_SPIRV) == 0) (sdlFail "SPIR-V not supported by SDL GPU renderer")

        variantStates <- mapM (createVariantState device renderer) variants
        when (null variantStates) (sdlFail "no shaders available")

        let color = SDL_FColor 0.15 0.55 1.0 1.0
        loop renderer variantStates 0 color 0

        mapM_ (destroyVariantState device) variantStates

loop :: Ptr SDL_Renderer -> [VariantState] -> Int -> SDL_FColor -> CFloat -> IO ()
loop renderer variants idx color t = do
  (quit, delta) <- pollInput
  let count = length variants
      idx' = if count == 0 then 0 else (idx + delta + count) `mod` count
  unless quit $ do
    when (idx' /= idx) $ do
      let name = vsName (variants !! idx')
      putStrLn ("shader: " <> name)
    let current = variants !! idx'
    renderFrame renderer current color t idx'
    sdlDelay 16
    loop renderer variants idx' color (t + 0.016)

renderFrame :: Ptr SDL_Renderer -> VariantState -> SDL_FColor -> CFloat -> Int -> IO ()
renderFrame renderer variant color t mode = do
  sdlCheckBool "sdlSetRenderDrawColor" (sdlSetRenderDrawColor renderer 8 8 8 255)
  sdlCheckBool "sdlRenderClear" (sdlRenderClear renderer)

  let SDL_FColor cr cg cb ca = color
      params =
        ParamsU
          { time_res = V4 (realToFrac t) 800 600 (fromIntegral mode)
          , color = V4 (realToFrac cr) (realToFrac cg) (realToFrac cb) (realToFrac ca)
          }
  case vsParamsUniform variant params of
    Left err -> error ("uniform pack failed: " <> err)
    Right input ->
      BS.useAsCStringLen (uiBytes input) $ \(paramPtr, len) ->
        sdlCheckBool "sdlSetGPURenderStateFragmentUniforms" $
          sdlSetGPURenderStateFragmentUniforms (vsState variant) (uiBinding input) (castPtr paramPtr) (fromIntegral len)
  sdlCheckBool "sdlSetGPURenderState" (sdlSetGPURenderState renderer (vsState variant))

  let positions :: [CFloat]
      positions =
        [ 0, 0
        , 800, 0
        , 800, 600
        , 0, 0
        , 800, 600
        , 0, 600
        ]
      uvs :: [CFloat]
      uvs =
        [ 0, 0
        , 1, 0
        , 1, 1
        , 0, 0
        , 1, 1
        , 0, 1
        ]
      colors :: [SDL_FColor]
      colors =
        [ SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        ]
      strideXY = fromIntegral (2 * sizeOf (undefined :: CFloat))
      strideColor = fromIntegral (sizeOf (undefined :: SDL_FColor))
      strideUV = strideXY
  withArray positions $ \posPtr ->
    withArray colors $ \colorPtr ->
      withArray uvs $ \uvPtr ->
        sdlCheckBool "sdlRenderGeometryRaw" $
          sdlRenderGeometryRaw renderer nullPtr posPtr strideXY colorPtr strideColor uvPtr strideUV 6 nullPtr 0 0

  sdlCheckBool "sdlRenderPresent" (sdlRenderPresent renderer)

pollInput :: IO (Bool, Int)
pollInput = allocaBytes sdlEventSize $ \eventPtr -> go eventPtr False 0
  where
    go ptr quit delta = do
      has <- sdlPollEvent ptr
      if not (fromCBool has)
        then pure (quit, delta)
        else do
          evType <- peek (castPtr ptr :: Ptr Word32)
          if evType == sdl_EVENT_QUIT
            then go ptr True delta
            else if evType == sdl_EVENT_KEY_DOWN
              then do
                scancode <- peekByteOff ptr sdlKeyboardEventScancodeOffset :: IO Word32
                repeatFlag <- peekByteOff ptr sdlKeyboardEventRepeatOffset :: IO Word8
                let delta' =
                      if repeatFlag /= 0
                        then delta
                        else case scancode of
                          _ | scancode == sdl_SCANCODE_LEFT -> delta - 1
                          _ | scancode == sdl_SCANCODE_RIGHT -> delta + 1
                          _ -> delta
                go ptr quit delta'
              else go ptr quit delta

createShader :: Ptr SDL_GPUDevice -> SDL_GPUShaderStage -> Word32 -> Word32 -> Word32 -> Word32 -> BS.ByteString -> IO (Ptr SDL_GPUShader)
createShader device stage samplerCount storageTextureCount storageBufferCount uniformCount bytes =
  BS.useAsCStringLen bytes $ \(ptr, len) ->
    withCString "main" $ \entry -> do
      let info = SDL_GPUShaderCreateInfo
            { shaderCodeSize = fromIntegral len
            , shaderCode = castPtr ptr
            , shaderEntrypoint = entry
            , shaderFormat = sdl_GPU_SHADERFORMAT_SPIRV
            , shaderStage = stage
            , shaderNumSamplers = samplerCount
            , shaderNumStorageTextures = storageTextureCount
            , shaderNumStorageBuffers = storageBufferCount
            , shaderNumUniformBuffers = uniformCount
            , shaderProps = 0
            }
      with info $ \infoPtr -> do
        shader <- sdlCreateGPUShader device infoPtr
        when (shader == nullPtr) (sdlFail "sdlCreateGPUShader")
        pure shader

createRenderState :: Ptr SDL_Renderer -> Ptr SDL_GPUShader -> IO (Ptr SDL_GPURenderState)
createRenderState renderer frag = do
  let info = SDL_GPURenderStateCreateInfo
        { rsFragmentShader = frag
        , rsNumSamplerBindings = 0
        , rsSamplerBindings = nullPtr
        , rsNumStorageTextures = 0
        , rsStorageTextures = nullPtr
        , rsNumStorageBuffers = 0
        , rsStorageBuffers = nullPtr
        , rsProps = 0
        }
  with info $ \infoPtr -> do
    state <- sdlCreateGPURenderState renderer infoPtr
    when (state == nullPtr) (sdlFail "sdlCreateGPURenderState")
    pure state

bindingCountInfo :: [BindingInfo] -> Word32
bindingCountInfo infos =
  let bindings = map biBinding infos
  in if null bindings then 0 else 1 + maximum bindings

mkVariant :: String -> CompiledShader iface -> ShaderVariant
mkVariant name shader =
  case prepareShader shader of
    Left err -> error ("variant " <> name <> ": " <> err)
    Right prepared ->
      let plan = psPlan prepared
          samplerCount = bindingCountInfo (bpSamplers plan)
          storageBufferCount = bindingCountInfo (bpStorageBuffers plan)
          storageTextureCount = bindingCountInfo (bpStorageTextures plan)
          uniformCount = bindingCountInfo (bpUniforms plan)
          paramsInfo =
            case [bi | bi <- bpUniforms plan, biName bi == "params"] of
              (bi:_) -> bi
              [] -> error ("variant " <> name <> ": params uniform not found")
          paramsUniform params = do
            bytes <- packUniformFrom (biType paramsInfo) params
            pure UniformInput
              { uiName = biName paramsInfo
              , uiGroup = biGroup paramsInfo
              , uiBinding = biBinding paramsInfo
              , uiBytes = bytes
              }
      in ShaderVariant name prepared samplerCount storageTextureCount storageBufferCount uniformCount paramsUniform

mkVariantDynamic :: String -> SomeCompiledShader -> ShaderVariant
mkVariantDynamic name (SomeCompiledShader shader) =
  mkVariant name shader

createVariantState :: Ptr SDL_GPUDevice -> Ptr SDL_Renderer -> ShaderVariant -> IO VariantState
createVariantState device renderer variant = do
  frag <- createShader device sdl_GPU_SHADERSTAGE_FRAGMENT
    (svSamplerCount variant)
    (svStorageTextureCount variant)
    (svStorageBufferCount variant)
    (svUniformCount variant)
    (variantSpirv variant)
  state <- createRenderState renderer frag
  pure VariantState
    { vsName = svName variant
    , vsState = state
    , vsShader = frag
    , vsUniformCount = svUniformCount variant
    , vsParamsUniform = svParamsUniform variant
    }

variantSpirv :: ShaderVariant -> BS.ByteString
variantSpirv ShaderVariant{svPrepared = prepared} =
  shaderSpirv (psShader prepared)

destroyVariantState :: Ptr SDL_GPUDevice -> VariantState -> IO ()
destroyVariantState device variant = do
  sdlDestroyGPURenderState (vsState variant)
  sdlReleaseGPUShader device (vsShader variant)

sdlCheckBool :: String -> IO CBool -> IO ()
sdlCheckBool label action = do
  ok <- action
  unless (fromCBool ok) (sdlFail label)

sdlFail :: String -> IO a
sdlFail label = do
  err <- sdlGetError >>= peekCString
  error (label <> ": " <> err)

fromCBool :: CBool -> Bool
fromCBool (CBool 0) = False
fromCBool _ = True

data ParamsU = ParamsU
  { time_res :: V4 Float
  , color :: V4 Float
  } deriving (Eq, Show, Generic)

instance ToUniform ParamsU
