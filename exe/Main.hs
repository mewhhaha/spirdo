{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Foreign
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CBool(..), CFloat, CInt(..))

import Spirdo.SDL3
import Spirdo.Wesl (BindingDesc(..), CompiledShader(..), ReflectBindings, uniformBindingsFor, wesl)

main :: IO ()
main = do
  let fragmentShader =
        [wesl|
struct Params {
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

@fragment
fn main() -> @location(0) vec4<f32> {
  return params.color;
}
|]
  BS.writeFile "fragment.spv" (shaderSpirv fragmentShader)

  sdlCheckBool "sdlInit" (sdlInit sdl_INIT_VIDEO)

  withCString "Spirdo SDL3" $ \title -> do
    window <- sdlCreateWindow title 800 600 0
    when (window == nullPtr) (sdlFail "sdlCreateWindow")

    renderer <- sdlCreateGPURenderer nullPtr window
    when (renderer == nullPtr) (sdlFail "sdlCreateGPURenderer")

    device <- sdlGetGPURendererDevice renderer
    when (device == nullPtr) (sdlFail "sdlGetGPURendererDevice")

    formats <- sdlGetGPUShaderFormats device
    when ((formats .&. sdl_GPU_SHADERFORMAT_SPIRV) == 0) (sdlFail "SPIR-V not supported by SDL GPU renderer")

    frag <- createShader device sdl_GPU_SHADERSTAGE_FRAGMENT (fragmentUniformCount fragmentShader) (shaderSpirv fragmentShader)
    state <- createRenderState renderer frag

    let uniformSlot = fragmentUniformSlot fragmentShader
        color = SDL_FColor 0.1 0.6 1.0 1.0

    loop renderer state uniformSlot color

    sdlDestroyGPURenderState state
    sdlReleaseGPUShader device frag
    sdlDestroyRenderer renderer
    sdlDestroyWindow window

  sdlQuit

loop :: Ptr SDL_Renderer -> Ptr SDL_GPURenderState -> Word32 -> SDL_FColor -> IO ()
loop renderer state uniformSlot color = do
  quit <- pollQuit
  unless quit $ do
    renderFrame renderer state uniformSlot color
    sdlDelay 16
    loop renderer state uniformSlot color

renderFrame :: Ptr SDL_Renderer -> Ptr SDL_GPURenderState -> Word32 -> SDL_FColor -> IO ()
renderFrame renderer state uniformSlot color = do
  sdlCheckBool "sdlSetRenderDrawColor" (sdlSetRenderDrawColor renderer 8 8 8 255)
  sdlCheckBool "sdlRenderClear" (sdlRenderClear renderer)

  with color $ \colorPtr ->
    sdlCheckBool "sdlSetGPURenderStateFragmentUniforms" $
      sdlSetGPURenderStateFragmentUniforms state uniformSlot (castPtr colorPtr) (fromIntegral (sizeOf color))
  sdlCheckBool "sdlSetGPURenderState" (sdlSetGPURenderState renderer state)

  let positions :: [CFloat]
      positions =
        [ 160, 120
        , 640, 120
        , 400, 480
        ]
      uvs :: [CFloat]
      uvs =
        [ 0, 0
        , 1, 0
        , 0.5, 1
        ]
      colors :: [SDL_FColor]
      colors =
        [ SDL_FColor 1 1 1 1
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
          sdlRenderGeometryRaw renderer nullPtr posPtr strideXY colorPtr strideColor uvPtr strideUV 3 nullPtr 0 0

  sdlCheckBool "sdlRenderPresent" (sdlRenderPresent renderer)

pollQuit :: IO Bool
pollQuit = allocaBytes sdlEventSize $ \eventPtr -> go eventPtr
  where
    go ptr = do
      has <- sdlPollEvent ptr
      if not (fromCBool has)
        then pure False
        else do
          evType <- peek (castPtr ptr :: Ptr Word32)
          if evType == sdl_EVENT_QUIT
            then pure True
            else go ptr

createShader :: Ptr SDL_GPUDevice -> SDL_GPUShaderStage -> Word32 -> BS.ByteString -> IO (Ptr SDL_GPUShader)
createShader device stage uniformCount bytes =
  BS.useAsCStringLen bytes $ \(ptr, len) ->
    withCString "main" $ \entry -> do
      let info = SDL_GPUShaderCreateInfo
            { shaderCodeSize = fromIntegral len
            , shaderCode = castPtr ptr
            , shaderEntrypoint = entry
            , shaderFormat = sdl_GPU_SHADERFORMAT_SPIRV
            , shaderStage = stage
            , shaderNumSamplers = 0
            , shaderNumStorageTextures = 0
            , shaderNumStorageBuffers = 0
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
  [] -> 0
  bindings -> 1 + maximum (map descBinding bindings)

fragmentUniformSlot :: ReflectBindings iface => CompiledShader iface -> Word32
fragmentUniformSlot shader = case uniformBindingsFor shader of
  (b:_) -> descBinding b
  [] -> 0

sdlCheckBool :: String -> IO CBool -> IO ()
sdlCheckBool label action = do
  ok <- action
  when (not (fromCBool ok)) (sdlFail label)

sdlFail :: String -> IO a
sdlFail label = do
  err <- sdlGetError >>= peekCString
  error (label <> ": " <> err)

fromCBool :: CBool -> Bool
fromCBool (CBool 0) = False
fromCBool _ = True
