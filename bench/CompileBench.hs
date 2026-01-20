module Main (main) where

import qualified Data.ByteString as BS
import Data.List (findIndex, isPrefixOf, tails)
import GHC.Clock (getMonotonicTimeNSec)
import Spirdo.Wesl (compileWeslToSpirvBytes)
import System.Exit (exitFailure)

findSubstr :: String -> String -> Maybe Int
findSubstr needle haystack = findIndex (isPrefixOf needle) (tails haystack)

extractShaderBlock :: String -> String -> Either String String
extractShaderBlock name src = do
  let marker = "let " <> name
  i <- maybe (Left ("missing marker: " <> marker)) Right (findSubstr marker src)
  let afterMarker = drop (i + length marker) src
  j <- maybe (Left "missing [wesl| after marker") Right (findSubstr "[wesl|" afterMarker)
  let afterStart = drop (j + length "[wesl|") afterMarker
  k <- maybe (Left "missing |] terminator") Right (findSubstr "|]" afterStart)
  Right (take k afterStart)

compileOnce :: String -> IO Int
compileOnce src =
  case compileWeslToSpirvBytes src of
    Left err -> do
      putStrLn ("compile failed: " <> show err)
      exitFailure
    Right bs -> pure (BS.length bs)

main :: IO ()
main = do
  srcFile <- readFile "exe/Main.hs"
  shaderSrc <- case extractShaderBlock "fragmentFeatureShader" srcFile of
    Left msg -> do
      putStrLn ("failed to extract shader: " <> msg)
      exitFailure
    Right s -> pure s
  _ <- compileOnce shaderSrc
  let iters :: Int
      iters = 50
  t0 <- getMonotonicTimeNSec
  total <- go shaderSrc iters 0
  t1 <- getMonotonicTimeNSec
  let dt = t1 - t0
  let per = fromIntegral dt / fromIntegral iters :: Double
  putStrLn ("compile iterations: " <> show iters)
  putStrLn ("total bytes: " <> show total)
  putStrLn ("total time (ns): " <> show dt)
  putStrLn ("time per compile (ns): " <> show per)
  where
    go _ 0 acc = pure acc
    go src n acc = do
      len <- compileOnce src
      go src (n - 1) (acc + len)
