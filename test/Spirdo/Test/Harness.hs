-- | Small executable-test runner with deterministic name filtering.
module Spirdo.Test.Harness
  ( TestCheck
  , testFilterFromEnvironmentAndArgs
  , runChecks
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_)
import Data.List (isInfixOf)
import System.Environment (getArgs, lookupEnv)

type TestCheck = (String, IO ())

-- | Read a test-name substring from @--match SUBSTRING@, @--match=SUBSTRING@,
-- or @SPIRDO_TEST_FILTER@. A command-line filter takes precedence.
testFilterFromEnvironmentAndArgs :: IO (Maybe String)
testFilterFromEnvironmentAndArgs = do
  environmentFilter <- lookupEnv "SPIRDO_TEST_FILTER"
  arguments <- getArgs
  commandLineFilter <- parseArguments arguments
  case commandLineFilter of
    Just _ -> pure commandLineFilter
    Nothing -> do
      _ <- traverse rejectEmpty environmentFilter
      pure environmentFilter
  where
    parseArguments [] = pure Nothing
    parseArguments ("--match" : substring : rest) = do
      rejectEmpty substring
      rejectDuplicate rest
      pure (Just substring)
    parseArguments (argument : rest)
      | Just substring <- stripMatchPrefix argument = do
          rejectEmpty substring
          rejectDuplicate rest
          pure (Just substring)
      | otherwise = fail ("unknown test argument: " <> argument)

    stripMatchPrefix argument =
      case splitAt 8 argument of
        ("--match=", substring) -> Just substring
        _ -> Nothing

    rejectEmpty substring
      | null substring = fail "test filter cannot be empty"
      | otherwise = pure ()

    rejectDuplicate [] = pure ()
    rejectDuplicate _ = fail "only one --match test filter may be supplied"

-- | Run matching checks in one named section and return how many ran.
runChecks :: Maybe String -> String -> [TestCheck] -> IO Int
runChecks testFilter section checks = do
  let matchingChecks = filter (matchesFilter testFilter section . fst) checks
  if null matchingChecks
    then pure 0
    else do
      putStrLn ("== " <> section)
      forM_ matchingChecks $ \(label, action) -> do
        result <- (try action :: IO (Either SomeException ()))
        case result of
          Left err ->
            fail (section <> "/" <> label <> ": " <> displayException err)
          Right () -> pure ()
      pure (length matchingChecks)

matchesFilter :: Maybe String -> String -> String -> Bool
matchesFilter Nothing _ _ = True
matchesFilter (Just substring) section label =
  substring `isInfixOf` (section <> "/" <> label)
