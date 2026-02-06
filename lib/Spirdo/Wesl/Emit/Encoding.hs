module Spirdo.Wesl.Emit.Encoding
  ( spirvToBytes
  , encodeString
  ) where

import Data.Bits ((.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word32)

spirvToBytes :: [Word32] -> ByteString
spirvToBytes words32 = BSL.toStrict (BB.toLazyByteString (foldMap BB.word32LE words32))

encodeString :: String -> [Word32]
encodeString str =
  let bytes = map (fromIntegral . fromEnum) (str <> "\0")
      chunks = chunk4 bytes
  in map packWord chunks

chunk4 :: [Word32] -> [[Word32]]
chunk4 [] = []
chunk4 xs = take 4 (xs <> repeat 0) : chunk4 (drop 4 xs)

packWord :: [Word32] -> Word32
packWord [a, b, c, d] = a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24)
packWord _ = 0
