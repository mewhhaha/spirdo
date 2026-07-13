-- | Regression checks for SPIR-V literal-string encoding.
module EncodingRegression (checks) where

import Control.Monad (unless)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Word (Word16, Word32)

import qualified Spirdo.Wesl as Wesl

checks :: [(String, IO ())]
checks =
  [ ("string-encoding:utf8-entry-point-name", checkUtf8EntryPointName)
  , ("string-encoding:utf8-packing-boundary", checkUtf8PackingBoundary)
  , ("string-encoding:embedded-nul-is-rejected-by-parser", checkEmbeddedNulRejection)
  ]

checkUtf8EntryPointName :: IO ()
checkUtf8EntryPointName = do
  entryNameWords <- compileEntryPointName "café"
  unless (entryNameWords == [0xc3666163, 0x000000a9]) $
    fail ("unexpected UTF-8 encoding for café: " <> show entryNameWords)

checkUtf8PackingBoundary :: IO ()
checkUtf8PackingBoundary = do
  entryNameWords <- compileEntryPointName "éé"
  unless (entryNameWords == [0xa9c3a9c3, 0]) $
    fail ("unexpected UTF-8 encoding at four-byte boundary: " <> show entryNameWords)

checkEmbeddedNulRejection :: IO ()
checkEmbeddedNulRejection = do
  result <- Wesl.compile [] (Wesl.sourceText embeddedNulSource)
  case result of
    Left _ -> pure ()
    Right _ -> fail "parser accepted an identifier containing an embedded NUL"

compileEntryPointName :: String -> IO [Word32]
compileEntryPointName name = do
  result <- Wesl.compile [] (Wesl.sourceText (fragmentSource name))
  shader <-
    case result of
      Left err -> fail (Wesl.renderCompileError err)
      Right compiled -> pure compiled
  instructions <-
    case decodeInstructions (Wesl.shaderSpirv shader) of
      Left message -> fail message
      Right parsed -> pure parsed
  case find ((== 15) . fst) instructions of
    Just (_, _model : _id : operands) -> pure (takeSpirvStringWords operands)
    _ -> fail "compiled SPIR-V did not contain an OpEntryPoint instruction"

takeSpirvStringWords :: [Word32] -> [Word32]
takeSpirvStringWords = go
  where
    go [] = []
    go (word:rest)
      | hasNulByte word = [word]
      | otherwise = word : go rest

    hasNulByte word =
      word .&. 0x000000ff == 0
        || word .&. 0x0000ff00 == 0
        || word .&. 0x00ff0000 == 0
        || word .&. 0xff000000 == 0

fragmentSource :: String -> String
fragmentSource name =
  unlines
    [ "@fragment"
    , "fn " <> name <> "() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

embeddedNulSource :: String
embeddedNulSource = fragmentSource "valid\0name"

decodeInstructions :: BS.ByteString -> Either String [(Word16, [Word32])]
decodeInstructions bytes = go 20 []
  where
    go offset instructions
      | offset == BS.length bytes = Right (reverse instructions)
      | offset + 4 > BS.length bytes = Left "truncated SPIR-V instruction header"
      | otherwise =
          let instruction = word32At offset
              wordCount = fromIntegral (instruction `shiftR` 16)
              opcode = fromIntegral (instruction .&. 0xffff)
              nextOffset = offset + wordCount * 4
          in if wordCount == 0 || nextOffset > BS.length bytes
              then Left ("invalid SPIR-V instruction at byte " <> show offset)
              else
                let operands = [word32At (offset + ix * 4) | ix <- [1 .. wordCount - 1]]
                in go nextOffset ((opcode, operands) : instructions)

    word32At offset =
      let byte0 = fromIntegral (BS.index bytes offset)
          byte1 = fromIntegral (BS.index bytes (offset + 1))
          byte2 = fromIntegral (BS.index bytes (offset + 2))
          byte3 = fromIntegral (BS.index bytes (offset + 3))
      in byte0 .|. (byte1 `shiftL` 8) .|. (byte2 `shiftL` 16) .|. (byte3 `shiftL` 24)
