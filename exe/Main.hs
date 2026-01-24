{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Spirdo.Wesl
  ( BindingDesc(..)
  , BindingInfo(..)
  , BindingKind(..)
  , CompiledShader(..)
  , CompileOptions(..)
  , ToUniform(..)
  , V4(..)
  , binding
  , bindingInfoForMap
  , bindingMap
  , HasBinding
  , uniform
  , packUniform
  , OverrideValue(..)
  , ReflectBindings
  , ShaderInterface(..)
  , SomeCompiledShader(..)
  , compileWeslToSpirvWith
  , defaultCompileOptions
  , samplerBindingsFor
  , storageBufferBindingsFor
  , storageTextureBindingsFor
  , uniformBindingsFor
  )
import Examples.Compute
import Examples.Fragments
import Examples.Override (fragmentOverrideSrc)
import Examples.Vertex
import Spirdo.SDL3.Safe (withSDL, withWindow, withGPURenderer)

data ShaderVariant = ShaderVariant
  { svName :: String
  , svSpirv :: BS.ByteString
  , svSamplerCount :: Word32
  , svStorageTextureCount :: Word32
  , svStorageBufferCount :: Word32
  , svUniformCount :: Word32
  , svUniformSlot :: Word32
  , svInterface :: ShaderInterface
  , svParamsInfo :: BindingInfo
  }

data VariantState = VariantState
  { vsName :: String
  , vsState :: Ptr SDL_GPURenderState
  , vsShader :: Ptr SDL_GPUShader
  , vsUniformCount :: Word32
  , vsUniformSlot :: Word32
  , vsInterface :: ShaderInterface
  , vsParamsInfo :: BindingInfo
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
      BS.writeFile ("fragment-" <> show ix <> ".spv") (svSpirv variant)
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
        loop renderer window device variantStates 0 color 0

        mapM_ (destroyVariantState device) variantStates

loop :: Ptr SDL_Renderer -> Ptr SDL_Window -> Ptr SDL_GPUDevice -> [VariantState] -> Int -> SDL_FColor -> CFloat -> IO ()
loop renderer window device variants idx color t = do
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
    loop renderer window device variants idx' color (t + 0.016)

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
      info = vsParamsInfo variant
  case packUniform (biType info) (uniform params) of
    Left err -> error ("uniform pack failed: " <> err)
    Right bytes ->
      BS.useAsCStringLen bytes $ \(paramPtr, len) ->
        sdlCheckBool "sdlSetGPURenderStateFragmentUniforms" $
          sdlSetGPURenderStateFragmentUniforms (vsState variant) (biBinding info) (castPtr paramPtr) (fromIntegral len)
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

fragmentUniformCount :: ReflectBindings iface => CompiledShader iface -> Word32
fragmentUniformCount shader = case uniformBindingsFor shader of
  bindings -> bindingCount bindings

fragmentUniformSlot :: ReflectBindings iface => CompiledShader iface -> Word32
fragmentUniformSlot shader = case uniformBindingsFor shader of
  (b:_) -> descBinding b
  [] -> 0

bindingCount :: [BindingDesc] -> Word32
bindingCount [] = 0
bindingCount bindings = 1 + maximum (map descBinding bindings)

mkVariant :: (ReflectBindings iface, HasBinding "params" iface ~ 'True) => String -> CompiledShader iface -> ShaderVariant
mkVariant name shader =
  let samplerCount = bindingCount (samplerBindingsFor shader)
      storageBufferCount = bindingCount (storageBufferBindingsFor shader)
      storageTextureCount = bindingCount (storageTextureBindingsFor shader)
      uniformCount = fragmentUniformCount shader
      uniformSlot = fragmentUniformSlot shader
      paramsDesc = binding @"params" shader
      iface = shaderInterface shader
      paramsInfo =
        case bindingInfoForMap (descName paramsDesc) (bindingMap iface) of
          Nothing -> error ("variant " <> name <> ": params binding not found")
          Just info -> info
  in ShaderVariant name (shaderSpirv shader) samplerCount storageTextureCount storageBufferCount uniformCount uniformSlot iface paramsInfo

mkVariantDynamic :: String -> SomeCompiledShader -> ShaderVariant
mkVariantDynamic name (SomeCompiledShader shader) =
  let iface = shaderInterface shader
      infos = siBindings iface
      samplerCount = bindingCountInfo isSamplerKind infos
      storageBufferCount = bindingCountInfo isStorageBufferKind infos
      storageTextureCount = bindingCountInfo isStorageTextureKind infos
      uniformCount = bindingCountInfo (== BUniform) infos
      uniformSlot =
        case [biBinding info | info <- infos, biKind info == BUniform] of
          (b:_) -> b
          [] -> 0
      paramsInfo =
        case bindingInfoForMap "params" (bindingMap iface) of
          Nothing -> error ("variant " <> name <> ": params binding not found")
          Just info -> info
  in ShaderVariant name (shaderSpirv shader) samplerCount storageTextureCount storageBufferCount uniformCount uniformSlot iface paramsInfo
  where
    bindingCountInfo predKind xs =
      let bindings = [biBinding info | info <- xs, predKind (biKind info)]
      in if null bindings then 0 else 1 + maximum bindings

    isSamplerKind k =
      k `elem`
        [ BSampler
        , BSamplerComparison
        , BTexture1D
        , BTexture1DArray
        , BTexture2D
        , BTexture2DArray
        , BTexture3D
        , BTextureCube
        , BTextureCubeArray
        , BTextureMultisampled2D
        , BTextureDepth2D
        , BTextureDepth2DArray
        , BTextureDepthCube
        , BTextureDepthCubeArray
        , BTextureDepthMultisampled2D
        ]

    isStorageBufferKind k = k == BStorageRead || k == BStorageReadWrite

    isStorageTextureKind k =
      k `elem` [BStorageTexture1D, BStorageTexture2D, BStorageTexture2DArray, BStorageTexture3D]

createVariantState :: Ptr SDL_GPUDevice -> Ptr SDL_Renderer -> ShaderVariant -> IO VariantState
createVariantState device renderer variant = do
  frag <- createShader device sdl_GPU_SHADERSTAGE_FRAGMENT
    (svSamplerCount variant)
    (svStorageTextureCount variant)
    (svStorageBufferCount variant)
    (svUniformCount variant)
    (svSpirv variant)
  state <- createRenderState renderer frag
  pure VariantState
    { vsName = svName variant
    , vsState = state
    , vsShader = frag
    , vsUniformCount = svUniformCount variant
    , vsUniformSlot = svUniformSlot variant
    , vsInterface = svInterface variant
    , vsParamsInfo = svParamsInfo variant
    }

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
