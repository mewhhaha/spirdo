-- | Executable entry point.
module Main (main) where

import qualified Data.ByteString as BS
import GHC.Clock (getMonotonicTimeNSec)
import Spirdo.Wesl (CompileOptions(..), compileWeslToSpirvBytes, compileWeslToSpirvBytesWithTimings, defaultCompileOptions)
import System.Exit (exitFailure)
import System.Environment (lookupEnv)

compileOnce :: String -> IO Int
compileOnce src =
  case compileWeslToSpirvBytes src of
    Left err -> do
      putStrLn ("compile failed: " <> show err)
      exitFailure
    Right bs -> pure (BS.length bs)

compileOnceTimed :: String -> IO Int
compileOnceTimed src = do
  result <- compileWeslToSpirvBytesWithTimings (defaultCompileOptions { timingVerbose = True }) src
  case result of
    Left err -> do
      putStrLn ("compile failed: " <> show err)
      exitFailure
    Right bs -> pure (BS.length bs)

main :: IO ()
main = do
  shaderSrc <- readFile "bench/fixtures/feature.wesl"
  timing <- lookupEnv "SPIRDO_TIMING"
  _ <-
    case timing of
      Just "1" -> compileOnceTimed shaderSrc
      Just "true" -> compileOnceTimed shaderSrc
      _ -> compileOnce shaderSrc
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
