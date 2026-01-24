module Spirdo.SDL3.Safe
  ( withSDL
  , withWindow
  , withGPURenderer
  , withGPUShader
  , withGPURenderState
  , throwIfNull
  , sdlChecked
  ) where

import Control.Exception (bracket, bracket_)
import Control.Monad (unless)
import Data.Word (Word32)
import Foreign (Ptr, nullPtr, with)
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CBool(..), CInt)

import Spirdo.SDL3

withSDL :: Word32 -> IO a -> IO a
withSDL flags action =
  bracket_ (sdlChecked "sdlInit" (sdlInit flags)) sdlQuit action

withWindow :: String -> Int -> Int -> Word32 -> (Ptr SDL_Window -> IO a) -> IO a
withWindow title w h flags action =
  bracket create sdlDestroyWindow action
  where
    create = withCString title $ \cTitle -> do
      let w' = fromIntegral w :: CInt
      let h' = fromIntegral h :: CInt
      win <- sdlCreateWindow cTitle w' h' flags
      throwIfNull "sdlCreateWindow" win

withGPURenderer :: Ptr SDL_Window -> (Ptr SDL_Renderer -> IO a) -> IO a
withGPURenderer window action =
  bracket create sdlDestroyRenderer action
  where
    create = do
      renderer <- sdlCreateGPURenderer nullPtr window
      throwIfNull "sdlCreateGPURenderer" renderer

withGPUShader :: Ptr SDL_GPUDevice -> SDL_GPUShaderCreateInfo -> (Ptr SDL_GPUShader -> IO a) -> IO a
withGPUShader device info action =
  bracket create (sdlReleaseGPUShader device) action
  where
    create = with info $ \infoPtr -> do
      shader <- sdlCreateGPUShader device infoPtr
      throwIfNull "sdlCreateGPUShader" shader

withGPURenderState :: Ptr SDL_Renderer -> SDL_GPURenderStateCreateInfo -> (Ptr SDL_GPURenderState -> IO a) -> IO a
withGPURenderState renderer info action =
  bracket create sdlDestroyGPURenderState action
  where
    create = with info $ \infoPtr -> do
      state <- sdlCreateGPURenderState renderer infoPtr
      throwIfNull "sdlCreateGPURenderState" state

sdlChecked :: String -> IO CBool -> IO ()
sdlChecked label action = do
  ok <- action
  unless (fromCBool ok) (sdlFail label)

throwIfNull :: String -> Ptr a -> IO (Ptr a)
throwIfNull label ptr =
  if ptr == nullPtr
    then sdlFail label
    else pure ptr

sdlFail :: String -> IO a
sdlFail label = do
  err <- sdlGetError >>= peekCString
  ioError (userError (label <> ": " <> err))

fromCBool :: CBool -> Bool
fromCBool (CBool 0) = False
fromCBool _ = True
