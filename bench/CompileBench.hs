-- | Sequential compilation benchmark scenarios.
module Main (main) where

import qualified Data.ByteString as BS
import GHC.Clock (getMonotonicTimeNSec)
import Spirdo.Wesl
  ( Diagnostic
  , ShaderBundle
  , compile
  , compileWithDiagnostics
  , shaderSpirv
  , sourceNamed
  )
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

data Scenario = Scenario
  { name :: !String
  , source :: !String
  , compileOne :: String -> IO (Either String Int)
  }

compileShader :: String -> String -> IO (Either String Int)
compileShader scenarioName shaderSource = do
  result <- compile [] (sourceNamed scenarioName shaderSource)
  pure $
    case result of
      Left err -> Left (show err)
      Right shader -> Right (forceShader shader)

compileShaderWithDiagnostics :: String -> String -> IO (Either String Int)
compileShaderWithDiagnostics scenarioName shaderSource = do
  result <- compileWithDiagnostics [] (sourceNamed scenarioName shaderSource)
  pure $
    case result of
      Left err -> Left (show err)
      Right (shader, diagnostics) ->
        Right (forceShader shader + forceDiagnostics diagnostics)

forceShader :: ShaderBundle -> Int
forceShader = BS.foldl' (\total byte -> total + fromIntegral byte) 0 . shaderSpirv

forceDiagnostics :: [Diagnostic] -> Int
forceDiagnostics = length . show

manyFunctionsShader :: Int -> String
manyFunctionsShader functionCount =
  unlines
    ( "fn chain_0(x: i32) -> i32 {"
        : "  return x + 1;"
        : "}"
        : concatMap functionLines [1 .. functionCount - 1]
        <> [ "@compute @workgroup_size(1, 1, 1)"
           , "fn main() {"
           , "  let result = chain_" <> show (functionCount - 1) <> "(0);"
           , "  if (result == 0) { }"
           , "}"
           ]
    )
  where
    functionLines index =
      [ "fn chain_" <> show index <> "(x: i32) -> i32 {"
      , "  return chain_" <> show (index - 1) <> "(x) + 1;"
      , "}"
      ]

runScenario :: Int -> Scenario -> IO ()
runScenario iterations scenario = do
  warmup <- scenario.compileOne scenario.source
  case warmup of
    Left err -> failScenario scenario.name "warmup" err
    Right forcedResult -> forcedResult `seq` pure ()
  startedAt <- getMonotonicTimeNSec
  result <- runIterations iterations scenario 0
  finishedAt <- getMonotonicTimeNSec
  case result of
    Left err -> failScenario scenario.name "measurement" err
    Right forcedResult -> do
      let elapsedNs = finishedAt - startedAt
          perCompileNs = fromIntegral elapsedNs / fromIntegral iterations :: Double
      putStrLn (scenario.name <> ": " <> show iterations <> " sequential compiles")
      putStrLn ("  SPIR-V and diagnostics checksum: " <> show forcedResult)
      putStrLn ("  total time (ns): " <> show elapsedNs)
      putStrLn ("  time per compile (ns): " <> show perCompileNs)

runIterations :: Int -> Scenario -> Int -> IO (Either String Int)
runIterations remaining scenario forcedResult
  | remaining == 0 = pure (Right forcedResult)
  | otherwise = do
      result <- scenario.compileOne scenario.source
      case result of
        Left err -> pure (Left err)
        Right nextResult ->
          let nextForcedResult = forcedResult + nextResult
          in nextForcedResult `seq` runIterations (remaining - 1) scenario nextForcedResult

failScenario :: String -> String -> String -> IO a
failScenario scenarioName phase err = do
  putStrLn ("benchmark scenario " <> scenarioName <> " failed during " <> phase <> ": " <> err)
  exitFailure

benchmarkIterations :: IO Int
benchmarkIterations = do
  configured <- lookupEnv "SPIRDO_BENCH_ITERS"
  case configured of
    Nothing -> pure 20
    Just rawValue ->
      case readMaybe rawValue of
        Just iterations | iterations > 0 -> pure iterations
        _ -> do
          putStrLn ("SPIRDO_BENCH_ITERS must be a positive integer, got " <> show rawValue)
          exitFailure

main :: IO ()
main = do
  iterations <- benchmarkIterations
  featureShader <- readFile "bench/fixtures/feature.wesl"
  diagnosticsShader <- readFile "bench/fixtures/diagnostics.wesl"
  resourceShader <- readFile "bench/fixtures/resources.wesl"
  let scenarios =
        [ Scenario "feature shader" featureShader (compileShader "feature.wesl")
        , Scenario "many functions (96)" (manyFunctionsShader 96) (compileShader "many-functions.wesl")
        , Scenario "diagnostics enabled" diagnosticsShader (compileShaderWithDiagnostics "diagnostics.wesl")
        , Scenario "resource-heavy shader" resourceShader (compileShader "resources.wesl")
        ]
  putStrLn ("compile iterations per scenario: " <> show iterations)
  mapM_ (runScenario iterations) scenarios
