-- | Bounded UTF-8 reads at WESL file-system boundaries.
module Spirdo.Wesl.SourceFile
  ( Utf8File(..)
  , maxManifestUtf8Bytes
  , maxSourceChars
  , maxSourceUtf8Bytes
  , readBoundedUtf8File
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Spirdo.Wesl.Types (CompileError(..))
import System.IO (IOMode(ReadMode), Handle, withBinaryFile)

data Utf8File = Utf8File
  { text :: !Text
  , bytes :: !Int
  }

maxSourceChars :: Int
maxSourceChars = 1024 * 1024

-- A Unicode scalar value occupies at most four bytes in UTF-8.
maxSourceUtf8Bytes :: Int
maxSourceUtf8Bytes = 4 * maxSourceChars

maxManifestUtf8Bytes :: Int
maxManifestUtf8Bytes = 256 * 1024

readBoundedUtf8File :: String -> Int -> FilePath -> IO (Either CompileError Utf8File)
readBoundedUtf8File label byteLimit path = do
  result <- try (withBinaryFile path ReadMode (readAtMost (byteLimit + 1)))
  pure $
    case result of
      Left ioErr ->
        Left
          ( CompileError
              ("failed to read " <> label <> ": " <> path <> " (" <> show (ioErr :: IOException) <> ")")
              Nothing
              Nothing
          )
      Right contents
        | BS.length contents > byteLimit ->
            Left
              ( CompileError
                  ( label <> " exceeds UTF-8 byte limit: " <> path
                      <> " (read at least " <> show (byteLimit + 1)
                      <> " bytes, limit " <> show byteLimit <> ")"
                  )
                  Nothing
                  Nothing
              )
        | otherwise ->
            case TE.decodeUtf8' contents of
              Left unicodeErr ->
                Left
                  ( CompileError
                      ("invalid UTF-8 in " <> label <> ": " <> path <> " (" <> show unicodeErr <> ")")
                      Nothing
                      Nothing
                  )
              Right decoded -> Right (Utf8File decoded (BS.length contents))

readAtMost :: Int -> Handle -> IO BS.ByteString
readAtMost byteCount handle = BS.concat . reverse <$> go byteCount []
  where
    go remaining chunks
      | remaining <= 0 = pure chunks
      | otherwise = do
          chunk <- BS.hGetSome handle (min remaining 32768)
          if BS.null chunk
            then pure chunks
            else go (remaining - BS.length chunk) (chunk : chunks)
