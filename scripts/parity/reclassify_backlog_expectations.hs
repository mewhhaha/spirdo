{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (foldM, forM, when)
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Spirdo.Wesl.Reflection
  ( Option(..)
  , OverrideSpecMode(..)
  , SamplerBindingMode(..)
  , compileFileWith
  )

header :: String
header =
  "id\tdomain\tspec_ref\tkind\texpected\tsource\torigin\torigin_ref\terror_contains\toracles\toptions\towner\texit_criteria"

defaultOptions :: [Option]
defaultOptions = [OptEnableFeature "f16"]

ownerDefault :: String
ownerDefault = "wesl-parity"

exitDefault :: String
exitDefault = "replace backlog row with executable fixture and concrete spec_ref mapping"

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

splitBy :: Char -> String -> [String]
splitBy delim raw =
  case break (== delim) raw of
    (a, []) -> [a]
    (a, _ : rest) -> a : splitBy delim rest

joinBy :: Char -> [String] -> String
joinBy _ [] = ""
joinBy delim (x : xs) = foldl (\acc part -> acc <> [delim] <> part) x xs

parseOptions :: String -> Either String [Option]
parseOptions raw = do
  let toks = filter (not . null) (map trim (splitBy ',' raw))
  let hasNoBase = "no-base" `elem` toks
  let seed = if hasNoBase then [] else defaultOptions
  foldM append seed (filter (\t -> t /= "base" && t /= "no-base") toks)
  where
    append acc tok =
      case tok of
        "sampler:combined" -> Right (acc <> [OptSamplerMode SamplerCombined])
        "sampler:separate" -> Right (acc <> [OptSamplerMode SamplerSeparate])
        "spec:strict" -> Right (acc <> [OptOverrideSpecMode SpecStrict])
        "spec:parity" -> Right (acc <> [OptOverrideSpecMode SpecParity])
        _ ->
          case stripPrefix' "feature:" tok of
            Just feat | not (null feat) -> Right (acc <> [OptEnableFeature feat])
            _ -> Left ("unknown option token: " <> tok)

stripPrefix' :: String -> String -> Maybe String
stripPrefix' prefix value
  | prefix `isPrefixOf` value = Just (drop (length prefix) value)
  | otherwise = Nothing

isBacklogRow :: [String] -> Bool
isBacklogRow cols =
  map toLower (trim (cols !! 3)) == "backlog"
    && map toLower (trim (cols !! 6)) == "cts"

isInlineSource :: String -> Bool
isInlineSource src = "inline:" `isPrefixOf` trim src

updateForPass :: [String] -> [String]
updateForPass cols =
  take 3 cols
    <> ["cts", "pass"]
    <> [cols !! 5, cols !! 6, cols !! 7]
    <> [cols !! 8, cols !! 9, cols !! 10]
    <> ["", ""]

updateForFail :: [String] -> [String]
updateForFail cols =
  take 3 cols
    <> ["backlog", "xfail"]
    <> [cols !! 5, cols !! 6, cols !! 7]
    <> [cols !! 8, cols !! 9, cols !! 10]
    <> [owner, exitCriteria]
  where
    owner = let v = trim (cols !! 11) in if null v then ownerDefault else cols !! 11
    exitCriteria = let v = trim (cols !! 12) in if null v then exitDefault else cols !! 12

main :: IO ()
main = do
  args <- getArgs
  let manifestPath = fromMaybe "test/parity/manifest.tsv" (listToMaybe args)
  raw <- readFile manifestPath
  let ls = lines raw
  case ls of
    [] -> fail ("empty manifest: " <> manifestPath)
    (h : rest) -> do
      when (map toLower (trim h) /= map toLower header) $
        fail ("unexpected manifest header in " <> manifestPath)
      let manifestDir = takeDirectory manifestPath
      (rows', stats) <- go manifestDir rest 0 0 0 0
      writeFile manifestPath (unlines (h : rows'))
      putStrLn
        ( "reclassify_backlog_expectations: "
            <> "total_backlog="
            <> show (totalBacklog stats)
            <> " promoted_pass="
            <> show (promotedPass stats)
            <> " still_xfail="
            <> show (stillXFail stats)
            <> " skipped_inline="
            <> show (skippedInline stats)
        )

data Stats = Stats
  { totalBacklog :: Int
  , promotedPass :: Int
  , stillXFail :: Int
  , skippedInline :: Int
  }

go :: FilePath -> [String] -> Int -> Int -> Int -> Int -> IO ([String], Stats)
go manifestDir rows0 total passCount failCount inlineCount = do
  out <- forM rows0 $ \line ->
    if null (trim line) || "#" `isPrefixOf` trim line
      then pure (line, 0, 0, 0, 0)
      else do
        let cols = splitBy '\t' line
        if length cols /= 13
          then fail ("expected 13 columns, got " <> show (length cols) <> " in line: " <> line)
          else
            if not (isBacklogRow cols)
              then pure (line, 0, 0, 0, 0)
              else do
                let src = cols !! 5
                if isInlineSource src
                  then pure (joinBy '\t' (updateForFail cols), 1, 0, 1, 1)
                  else do
                    opts <- either fail pure (parseOptions (cols !! 10))
                    result <- compileFileWith opts (manifestDir </> src)
                    case result of
                      Left _ ->
                        pure (joinBy '\t' (updateForFail cols), 1, 0, 1, 0)
                      Right _ ->
                        pure (joinBy '\t' (updateForPass cols), 1, 1, 0, 0)
  let rows' = [line | (line, _, _, _, _) <- out]
  let total' = total + sum [n | (_, n, _, _, _) <- out]
  let pass' = passCount + sum [n | (_, _, n, _, _) <- out]
  let fail' = failCount + sum [n | (_, _, _, n, _) <- out]
  let inline' = inlineCount + sum [n | (_, _, _, _, n) <- out]
  pure
    ( rows'
    , Stats
        { totalBacklog = total'
        , promotedPass = pass'
        , stillXFail = fail'
        , skippedInline = inline'
        }
    )
