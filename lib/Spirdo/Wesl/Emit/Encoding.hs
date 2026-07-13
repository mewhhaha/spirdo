module Spirdo.Wesl.Emit.Encoding
  ( spirvToBytes
  , encodeString
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
encodeString string
  | '\0' `elem` string = error "SPIR-V strings cannot contain embedded NUL characters"
  | otherwise = packBytes (BS.unpack (TE.encodeUtf8 (T.pack string)) <> [0])
  where
    packBytes [] = []
    packBytes bytes = packWord 0 0 bytes

    packWord acc _ [] = [acc]
    packWord acc shift (byte:rest) =
      let acc' = acc .|. (fromIntegral byte `shiftL` shift)
      in if shift == 24
          then acc' : packBytes rest
          else packWord acc' (shift + 8) rest
{-# INLINE encodeString #-}
