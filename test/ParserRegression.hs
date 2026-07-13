{-# LANGUAGE OverloadedRecordDot #-}

-- | Public-path regressions for WGSL/WESL lexical and parser boundaries.
module ParserRegression (checks) where

import Control.Monad (forM_, unless)
import Data.List (isInfixOf)

import Spirdo.Wesl
  ( CompileError(..)
  , Option(..)
  , compile
  , sourceNamed
  )

checks :: [(String, IO ())]
checks =
  [ ("parser-valid-numeric-literals", checkValidNumericLiterals)
  , ("parser-malformed-numeric-literals", checkMalformedNumericLiterals)
  , ("parser-typed-hex-exactness", checkTypedHexExactness)
  , ("parser-abstract-float-literals-must-be-finite", checkAbstractFloatFiniteness)
  , ("parser-resource-budgets", checkResourceBudgets)
  , ("parser-identifier-rules", checkIdentifierRules)
  , ("parser-unicode-and-phony-declaration-extension", checkUnicodeAndPhonyIdentifiers)
  , ("parser-nested-block-comments", checkNestedBlockComments)
  , ("parser-template-close-adjacency", checkTemplateCloseAdjacency)
  , ("parser-truncated-source-locations", checkTruncatedSourceLocations)
  , ("parser-duplicate-override-ids", checkDuplicateOverrideIds)
  ]

checkValidNumericLiterals :: IO ()
checkValidNumericLiterals =
  expectSuccess
    [OptEnableFeature "f16"]
    ("enable f16;\n" <> programWithExpressions
      [ ".5"
      , "1."
      , "0.e+4f"
      , "01."
      , "1e-3"
      , "42f"
      , "1e3h"
      , "1.2f"
      , "0h"
      , "0xa.fp+2"
      , "0x1P+4f"
      , "0X.3"
      , "0x3p+2h"
      , "0X1.fp-4"
      , "0x3.2p+2h"
      , "0x1f"
      , "0x1.0f"
      , "0x1p2"
      ])

checkMalformedNumericLiterals :: IO ()
checkMalformedNumericLiterals =
  forM_ malformedLiterals $ \spelling ->
    expectFailure
      ["malformed numeric literal", spelling]
      []
      (programWithExpressions [spelling])
  where
    malformedLiterals =
      [ "012"
      , "01f"
      , "01h"
      , "1_0"
      , "1__2"
      , "1_"
      , "0x_1"
      , "0x"
      , "1e"
      , "1e+"
      , "1e5h"
      , "0x1p"
      , "0x.p1"
      , "0x1.0h"
      , "1.0i"
      ]

checkTypedHexExactness :: IO ()
checkTypedHexExactness =
  expectFailure
    ["0x1.00000001p0f", "not exactly representable"]
    []
    (programWithExpressions ["0x1.00000001p0f"])

checkAbstractFloatFiniteness :: IO ()
checkAbstractFloatFiniteness = do
  expectFailure
    ["1e400", "abstract-float literal must be finite binary64"]
    []
    (programWithExpressions ["1e400"])
  expectFailure
    ["0x1p+2000", "abstract-float literal must be finite binary64"]
    []
    (programWithExpressions ["0x1p+2000"])

checkDuplicateOverrideIds :: IO ()
checkDuplicateOverrideIds =
  forM_
    [ "@id(1) @id(1) override value: u32 = 1u;\n" <> computeEntry
    , "@id(1) @id(2) override value: u32 = 1u;\n" <> computeEntry
    ] $
      expectFailure ["duplicate @id attributes"] []

checkResourceBudgets :: IO ()
checkResourceBudgets = do
  expectFailure
    ["source length 1048577 characters exceeds limit 1048576"]
    []
    (replicate 1048577 ' ')
  expectFailure
    ["token count 65537 exceeds limit 65536"]
    []
    (replicate 65537 ';')
  expectFailure
    ["numeric literal length 257 characters exceeds limit 256"]
    []
    (programWithExpressions [replicate 257 '9'])
  expectFailure
    ["delimiter nesting depth 257 exceeds limit 256"]
    []
    (programWithExpressions [replicate 257 '(' <> "1" <> replicate 257 ')'])
  expectFailure
    ["unary prefix operator count 257 exceeds limit 256"]
    []
    (programWithExpressions [replicate 257 '!' <> "true"])
  expectFailure
    ["translate-time prefix operator count 257 exceeds limit 256"]
    []
    (replicate257TextPrefix <> computeEntry)
  expectFailure
    ["type nesting depth 257 exceeds limit 256"]
    []
    ("alias Deep = " <> concat (replicate 257 "array<") <> "i32" <> replicate 257 '>' <> ";\n" <> computeEntry)
  where
    replicate257TextPrefix = "@if(" <> replicate 257 '!' <> "true)\n"

checkIdentifierRules :: IO ()
checkIdentifierRules = do
  forM_ invalidDeclarations $ \(spelling, source) ->
    expectFailure
      ["invalid declaration identifier", spelling]
      []
      source
  expectFailure
    ["invalid import identifier", "if"]
    []
    ("import shader::if;\n" <> computeEntry)
  expectFailure
    ["invalid import identifier", "self"]
    []
    ("import shader as self;\n" <> computeEntry)
  where
    invalidDeclarations =
      [ ("if", "@compute @workgroup_size(1)\nfn if() {}\n")
      , ("_", "struct _ { value: i32 }\n" <> computeEntry)
      , ("__private", "alias __private = i32;\n" <> computeEntry)
      , ("class", "const class = 1;\n" <> computeEntry)
      ]

checkUnicodeAndPhonyIdentifiers :: IO ()
checkUnicodeAndPhonyIdentifiers =
  expectSuccess
    []
    (unlines
      [ "fn Δέλτα(value: i32) -> i32 {"
      , "  return value;"
      , "}"
      , "@compute @workgroup_size(1)"
      , "fn main() {"
      , "  let _ = Δέλτα(1);"
      , "}"
      ])

checkNestedBlockComments :: IO ()
checkNestedBlockComments = do
  expectSuccess
    []
    ("/* outer /* middle /* inner */ middle */ outer */\n" <> computeEntry)
  expectFailure
    ["block comment nesting depth 129 exceeds limit 128"]
    []
    (concat (replicate 129 "/*") <> concat (replicate 129 "*/") <> computeEntry)

checkTemplateCloseAdjacency :: IO ()
checkTemplateCloseAdjacency =
  expectSuccess
    []
    (unlines
      [ "@compute @workgroup_size(1)"
      , "fn main() {"
      , "  let value: vec2<f32>=vec2<f32>(1.0);"
      , "  let pointer: ptr<function,vec2<f32>>=&value;"
      , "  let comparison = 2>=1;"
      , "  var shifted = 4;"
      , "  shifted>>=1;"
      , "  let _ = pointer;"
      , "}"
      ])

checkTruncatedSourceLocations :: IO ()
checkTruncatedSourceLocations = do
  expectFailureAt
    ["expected"]
    (3, 17)
    []
    "@compute\nfn main() {\n  let value = 1;"
  expectFailureAt
    ["unterminated block comment", "remaining nesting depth 1"]
    (2, 13)
    []
    "@compute\nfn main() { /* never closes"

programWithExpressions :: [String] -> String
programWithExpressions expressions =
  unlines $
    [ "@compute @workgroup_size(1)"
    , "fn main() {"
    ]
      <> zipWith declaration [0 :: Int ..] expressions
      <> ["}"]
  where
    declaration index expression =
      "  let literal" <> show index <> " = " <> expression <> ";"

computeEntry :: String
computeEntry = "@compute @workgroup_size(1)\nfn main() {}\n"

expectSuccess :: [Option] -> String -> IO ()
expectSuccess options source = do
  result <- compile options (sourceNamed "parser-regression.wesl" source)
  case result of
    Left err -> fail ("expected parser regression source to compile: " <> show err)
    Right _ -> pure ()

expectFailure :: [String] -> [Option] -> String -> IO ()
expectFailure expectedFragments options source = do
  result <- compile options (sourceNamed "parser-regression.wesl" source)
  case result of
    Left err -> assertErrorFragments expectedFragments err
    Right _ -> fail ("expected parser regression source to fail: " <> take 160 source)

expectFailureAt :: [String] -> (Int, Int) -> [Option] -> String -> IO ()
expectFailureAt expectedFragments expectedPosition options source = do
  result <- compile options (sourceNamed "parser-regression.wesl" source)
  case result of
    Left err -> do
      assertErrorFragments expectedFragments err
      let actualPosition = (err.ceLine, err.ceColumn)
          wantedPosition = (Just (fst expectedPosition), Just (snd expectedPosition))
      unless (actualPosition == wantedPosition) $
        fail ("expected error at " <> show wantedPosition <> ", got " <> show actualPosition <> ": " <> show err)
    Right _ -> fail "expected truncated parser regression source to fail"

assertErrorFragments :: [String] -> CompileError -> IO ()
assertErrorFragments expectedFragments err =
  forM_ expectedFragments $ \fragment ->
    unless (fragment `isInfixOf` (err.ceMessage)) $
      fail ("expected error containing " <> show fragment <> ", got " <> show err)
