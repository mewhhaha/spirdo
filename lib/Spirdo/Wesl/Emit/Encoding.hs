module Spirdo.Wesl.Emit.Encoding
  ( spirvToBytes
  , encodeString
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word32)

spirvToBytes :: [Word32] -> ByteString
spirvToBytes words32 = BSL.toStrict (BB.toLazyByteString (foldMap BB.word32LE words32))

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
