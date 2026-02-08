module Spirdo.Wesl.Emit.Encoding
  ( spirvToBytes
  , encodeString
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word32, Word8)
import Foreign.Storable (pokeByteOff)

spirvToBytes :: [Word32] -> ByteString
spirvToBytes words32 =
  BSI.unsafeCreate (length words32 * 4) $ \ptr -> go ptr 0 words32
  where
    go _ _ [] = pure ()
    go ptr off (w:ws) = do
      pokeWord32LE ptr off w
      go ptr (off + 4) ws

    pokeWord32LE ptr off w = do
      pokeByteOff ptr off (fromIntegral (w .&. 0xFF) :: Word8)
      pokeByteOff ptr (off + 1) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
      pokeByteOff ptr (off + 2) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
      pokeByteOff ptr (off + 3) (fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8)
{-# INLINE spirvToBytes #-}

encodeString :: String -> [Word32]
encodeString = go 0 0
  where
    go acc _ [] = [acc]
    go acc shift (ch:rest) =
      let byte = fromIntegral (fromEnum ch) .&. 0xFF
          acc' = acc .|. (byte `shiftL` shift)
      in if shift == 24
          then acc' : go 0 0 rest
          else go acc' (shift + 8) rest
{-# INLINE encodeString #-}
