-- | Regression checks for import and statement semantic validation.
module ImportSemanticRegression (checks) where

import Control.Exception (IOException, bracket, bracketOnError, catch)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import System.Directory
  ( createDirectory
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath ((</>))
import System.IO (Handle, hClose, openBinaryTempFile)

import Spirdo.Wesl

checks :: [(String, IO ())]
checks =
  [ ("import-semantic:filesystem-diamond", checkFilesystemDiamondImports)
  , ("import-semantic:filesystem-import-byte-limit", checkFilesystemImportByteLimit)
  , ("import-semantic:filesystem-import-invalid-utf8", checkFilesystemImportInvalidUtf8)
  , ("import-semantic:filesystem-import-depth-limit", checkFilesystemImportDepthLimit)
  , ("import-semantic:filesystem-preloaded-import-depth-limit", checkFilesystemPreloadedImportDepthLimit)
  , ("import-semantic:filesystem-import-module-count-limit", checkFilesystemImportModuleCountLimit)
  , ("import-semantic:filesystem-import-source-char-limit", checkFilesystemImportSourceCharLimit)
  , ("import-semantic:missing-item", checkMissingItemImport)
  , ("import-semantic:missing-item-alias", checkMissingAliasedItemImport)
  , ("import-semantic:item-import", checkItemImport)
  , ("import-semantic:item-import-alias", checkAliasedItemImport)
  , ("import-semantic:alias-declaration-conflict", checkImportAliasDeclarationConflict)
  , ("statement-semantic:break-outside", expectInlineFailureAt "break is only allowed" breakOutsideLoopSource)
  , ("statement-semantic:continue-outside", expectInlineFailureAt "continue is only allowed" continueOutsideLoopSource)
  , ("statement-semantic:break-if-outside", expectInlineFailureAt "break if is only allowed" breakIfOutsideLoopSource)
  , ("statement-semantic:break-if-loop-body", expectInlineFailure breakIfInLoopBodySource)
  , ("statement-semantic:break-if-nested-continuing", expectInlineFailure breakIfNestedInContinuingSource)
  , ("statement-semantic:break-if-not-final", expectInlineFailure breakIfNotFinalInContinuingSource)
  , ("statement-semantic:break-in-continuing", expectInlineFailure breakInContinuingSource)
  , ("statement-semantic:continue-in-continuing", expectInlineFailure continueInContinuingSource)
  , ("statement-semantic:break-switch", expectInlineSuccess breakInSwitchSource)
  , ("statement-semantic:continue-nested-switch", expectInlineSuccess continueInNestedSwitchSource)
  , ("statement-semantic:break-if-final-continuing", expectInlineSuccess breakIfFinalInContinuingSource)
  , ("statement-semantic:nested-loop-in-continuing", expectInlineSuccess nestedLoopInContinuingSource)
  , ("module-semantic:duplicate-value-declaration", expectInlineFailure duplicateValueDeclarationSource)
  , ("module-semantic:duplicate-global-declaration", expectInlineFailure duplicateGlobalDeclarationSource)
  , ("module-semantic:duplicate-type-declaration", expectInlineFailure duplicateTypeDeclarationSource)
  , ("module-semantic:duplicate-struct-declaration", expectInlineFailure duplicateStructDeclarationSource)
  , ("module-semantic:duplicate-struct-field", expectInlineFailure duplicateStructFieldSource)
  , ("const-semantic:and-short-circuits-invalid-rhs", expectInlineSuccess shortCircuitAndSource)
  , ("const-semantic:or-short-circuits-invalid-rhs", expectInlineSuccess shortCircuitOrSource)
  , ("const-semantic:negative-shift", expectInlineFailure negativeShiftSource)
  , ("const-semantic:width-shift", expectInlineFailure widthShiftSource)
  , ("const-semantic:huge-shift", expectInlineFailure hugeShiftSource)
  , ("const-semantic:non-finite-int-conversion", checkNonFiniteIntConversion)
  , ("const-semantic:if-shadow-scope", expectInlineSuccess constIfShadowSource)
  , ("const-semantic:outer-mutation-inner-shadow", expectInlineSuccess constOuterMutationInnerShadowSource)
  , ("const-semantic:for-scope", expectInlineSuccess constForScopeSource)
  , ("const-semantic:switch-scope", expectInlineSuccess constSwitchScopeSource)
  , ("const-semantic:fallthrough-scope", expectInlineSuccess constFallthroughScopeSource)
  , ("override-semantic:duplicate-option-key", checkDuplicateOverrideOptionKey)
  , ("override-semantic:unknown-option-key", checkUnknownOverrideOptionKey)
  , ("diagnostic-semantic:error-const-assert", expectInlineFailure diagnosticErrorConstAssertSource)
  , ("diagnostic-semantic:error-unused-variable", expectInlineFailure diagnosticErrorUnusedVariableSource)
  , ("diagnostic-semantic:root-directive-isolation", checkRootDiagnosticIsolation)
  ]

expectInlineSuccess :: String -> IO ()
expectInlineSuccess source = do
  result <- compile [] (sourceText source)
  case result of
    Left err -> fail ("expected compilation success, got: " <> renderCompileError err)
    Right _ -> pure ()

expectInlineFailure :: String -> IO ()
expectInlineFailure source = do
  result <- compile [] (sourceText source)
  case result of
    Left _ -> pure ()
    Right _ -> fail "expected compilation failure"

expectInlineFailureAt :: String -> String -> IO ()
expectInlineFailureAt expectedMessage source = do
  result <- compile [] (sourceText source)
  case result of
    Left err -> do
      unless (expectedMessage `isInfixOf` renderCompileError err) $
        fail ("expected error containing " <> show expectedMessage <> ", got: " <> renderCompileError err)
      whenMissingPosition err
    Right _ -> fail "expected compilation failure"
  where
    whenMissingPosition err =
      if isNothing err.ceLine || isNothing err.ceColumn
        then fail "expected compile error to include a source position"
        else pure ()

withTemporaryDirectory :: String -> (FilePath -> IO a) -> IO a
withTemporaryDirectory prefix = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      temporaryDirectory <- getTemporaryDirectory
      bracketOnError
        (openBinaryTempFile temporaryDirectory prefix)
        cleanupTemporaryFile
        $ \(path, handle) -> do
            hClose handle
            removeFile path
            createDirectory path
            pure path

cleanupTemporaryFile :: (FilePath, Handle) -> IO ()
cleanupTemporaryFile (path, handle) = do
  hClose handle `catch` ignoreIOException
  exists <- doesFileExist path
  when exists (removeFile path)

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = pure ()

checkFilesystemDiamondImports :: IO ()
checkFilesystemDiamondImports =
  withTemporaryDirectory "spirdo-diamond" $ \root -> do
    writeFile (root </> "diamond_base.wesl") "fn commonValue() -> f32 { return 0.5; }"
    writeFile (root </> "left.wesl") (unlines ["import diamond_base;", "fn leftValue() -> f32 { return diamond_base::commonValue(); }"])
    writeFile (root </> "right.wesl") (unlines ["import diamond_base;", "fn rightValue() -> f32 { return diamond_base::commonValue(); }"])
    let mainFile = root </> "main.wesl"
    writeFile mainFile diamondMainSource
    result <- compile [] (sourceFile mainFile)
    case result of
      Left err -> fail ("filesystem diamond imports failed: " <> renderCompileError err)
      Right _ -> pure ()

checkFilesystemImportByteLimit :: IO ()
checkFilesystemImportByteLimit =
  withTemporaryDirectory "spirdo-import-byte-limit" $ \root -> do
    let importedFile = root </> "library.wesl"
        mainFile = root </> "main.wesl"
    BS.writeFile importedFile (BS.replicate (sourceByteLimit + 1) 32)
    writeFile mainFile (importMainSource "import library;")
    expectFileFailureContaining
      mainFile
      [ importedFile
      , "read at least " <> show (sourceByteLimit + 1) <> " bytes"
      , "limit " <> show sourceByteLimit
      ]

checkFilesystemImportInvalidUtf8 :: IO ()
checkFilesystemImportInvalidUtf8 =
  withTemporaryDirectory "spirdo-import-invalid-utf8" $ \root -> do
    let importedFile = root </> "library.wesl"
        mainFile = root </> "main.wesl"
    BS.writeFile importedFile (BS.pack [0xff])
    writeFile mainFile (importMainSource "import library;")
    expectFileFailureContaining mainFile ["invalid UTF-8 in imported module", importedFile]

checkFilesystemImportDepthLimit :: IO ()
checkFilesystemImportDepthLimit =
  withTemporaryDirectory "spirdo-import-depth-limit" $ \root -> do
    let mainFile = root </> "main.wesl"
        deepestModule = root </> moduleFileName (filesystemImportDepthLimit + 1)
    writeFile mainFile (importMainSource "import module1;")
    mapM_ (writeDepthModule root) [1 .. filesystemImportDepthLimit + 1]
    expectFileFailureContaining
      mainFile
      [ deepestModule
      , "depth " <> show (filesystemImportDepthLimit + 1)
      , "limit " <> show filesystemImportDepthLimit
      ]

checkFilesystemPreloadedImportDepthLimit :: IO ()
checkFilesystemPreloadedImportDepthLimit =
  withTemporaryDirectory "spirdo-preloaded-import-depth-limit" $ \root -> do
    let mainFile = root </> "main.wesl"
        leafFile = root </> "leaf.wesl"
    writeFile mainFile (unlines ["import leaf;", "import module1;", computeMain])
    writeFile leafFile "const leaf_value = 0;"
    mapM_ (writePreloadedDepthModule root) [1 .. filesystemImportDepthLimit]
    expectFileFailureContaining
      mainFile
      [ leafFile
      , "depth " <> show (filesystemImportDepthLimit + 1)
      , "limit " <> show filesystemImportDepthLimit
      ]

checkFilesystemImportModuleCountLimit :: IO ()
checkFilesystemImportModuleCountLimit =
  withTemporaryDirectory "spirdo-import-module-count-limit" $ \root -> do
    let mainFile = root </> "main.wesl"
        overflowingModule = root </> moduleFileName filesystemImportModuleLimit
        importedModules = ["import module" <> show n <> ";" | n <- [1 .. filesystemImportModuleLimit]]
    writeFile mainFile (unlines (importedModules <> [computeMain]))
    mapM_ (\n -> writeFile (root </> moduleFileName n) "") [1 .. filesystemImportModuleLimit]
    expectFileFailureContaining
      mainFile
      [ overflowingModule
      , show (filesystemImportModuleLimit + 1) <> " modules"
      , "limit " <> show filesystemImportModuleLimit
      ]

checkFilesystemImportSourceCharLimit :: IO ()
checkFilesystemImportSourceCharLimit =
  withTemporaryDirectory "spirdo-import-source-char-limit" $ \root -> do
    let mainFile = root </> "main.wesl"
        overflowingModule = root </> ("source" <> show aggregateSourceModuleCount <> ".wesl")
        imports = ["import source" <> show n <> ";" | n <- [1 .. aggregateSourceModuleCount]]
        mainSource = unlines (imports <> [computeMain])
        expectedChars = aggregateSourceModuleCount * parserSourceCharLimit + length mainSource
    writeFile mainFile mainSource
    mapM_ (\n -> BS.writeFile (root </> ("source" <> show n <> ".wesl")) (BS.replicate parserSourceCharLimit 32)) [1 .. aggregateSourceModuleCount]
    expectFileFailureContaining
      mainFile
      [ overflowingModule
      , show expectedChars <> " characters"
      , "limit " <> show filesystemImportSourceCharLimit
      ]

sourceByteLimit :: Int
sourceByteLimit = 4 * 1024 * 1024

filesystemImportModuleLimit :: Int
filesystemImportModuleLimit = 256

filesystemImportSourceCharLimit :: Int
filesystemImportSourceCharLimit = 16 * 1024 * 1024

filesystemImportDepthLimit :: Int
filesystemImportDepthLimit = 64

parserSourceCharLimit :: Int
parserSourceCharLimit = 1024 * 1024

aggregateSourceModuleCount :: Int
aggregateSourceModuleCount = filesystemImportSourceCharLimit `div` parserSourceCharLimit

moduleFileName :: Int -> FilePath
moduleFileName n = "module" <> show n <> ".wesl"

writeDepthModule :: FilePath -> Int -> IO ()
writeDepthModule root n =
  let source =
        if n > filesystemImportDepthLimit
          then "const leaf = 0;"
          else "import module" <> show (n + 1) <> ";"
  in writeFile (root </> moduleFileName n) source

writePreloadedDepthModule :: FilePath -> Int -> IO ()
writePreloadedDepthModule root n =
  let source =
        if n == filesystemImportDepthLimit
          then "import leaf;"
          else "import module" <> show (n + 1) <> ";"
  in writeFile (root </> moduleFileName n) source

checkMissingItemImport :: IO ()
checkMissingItemImport =
  withImportLibrary $ \root -> do
    let mainFile = root </> "main.wesl"
    writeFile mainFile (importMainSource "import library::missing;")
    expectFileFailure mainFile

checkMissingAliasedItemImport :: IO ()
checkMissingAliasedItemImport =
  withImportLibrary $ \root -> do
    let mainFile = root </> "main.wesl"
    writeFile mainFile (importMainSource "import library::missing as importedMissing;")
    expectFileFailure mainFile

checkItemImport :: IO ()
checkItemImport =
  withImportLibrary $ \root -> do
    let mainFile = root </> "main.wesl"
    writeFile mainFile (unlines ["import library::shade;", fragmentMain "return shade();"])
    expectFileSuccess mainFile

checkAliasedItemImport :: IO ()
checkAliasedItemImport =
  withImportLibrary $ \root -> do
    let mainFile = root </> "main.wesl"
    writeFile mainFile (unlines ["import library::shade as importedShade;", fragmentMain "return importedShade();"])
    expectFileSuccess mainFile

checkImportAliasDeclarationConflict :: IO ()
checkImportAliasDeclarationConflict =
  withImportLibrary $ \root -> do
    let mainFile = root </> "main.wesl"
    writeFile mainFile (unlines ["import library::shade;", "const shade = 1;", fragmentMain "return vec4(0.0);"])
    expectFileFailure mainFile

withImportLibrary :: (FilePath -> IO a) -> IO a
withImportLibrary action =
  withTemporaryDirectory "spirdo-import" $ \root -> do
    writeFile (root </> "library.wesl") "fn shade() -> vec4<f32> { return vec4(0.25, 0.5, 0.75, 1.0); }"
    action root

expectFileSuccess :: FilePath -> IO ()
expectFileSuccess path = do
  result <- compile [] (sourceFile path)
  case result of
    Left err -> fail ("expected file compilation success, got: " <> renderCompileError err)
    Right _ -> pure ()

expectFileFailure :: FilePath -> IO ()
expectFileFailure path = do
  result <- compile [] (sourceFile path)
  case result of
    Left _ -> pure ()
    Right _ -> fail "expected file compilation failure"

expectFileFailureContaining :: FilePath -> [String] -> IO ()
expectFileFailureContaining path expectedFragments = do
  result <- compile [] (sourceFile path)
  case result of
    Left err -> do
      let rendered = renderCompileError err
      mapM_
        (\expected ->
          unless (expected `isInfixOf` rendered) $
            fail ("expected error containing " <> show expected <> ", got: " <> rendered)
        )
        expectedFragments
    Right _ -> fail "expected file compilation failure"

checkDuplicateOverrideOptionKey :: IO ()
checkDuplicateOverrideOptionKey = do
  result <- compile [OptOverrides [("scale", OVI32 1), ("scale", OVI32 2)]] (sourceText overrideSource)
  case result of
    Left _ -> pure ()
    Right _ -> fail "expected duplicate override option key to fail"

checkUnknownOverrideOptionKey :: IO ()
checkUnknownOverrideOptionKey = do
  result <- compile [OptOverrides [("unknown", OVI32 1)]] (sourceText overrideSource)
  case result of
    Left _ -> pure ()
    Right _ -> fail "expected unknown override option key to fail"

checkNonFiniteIntConversion :: IO ()
checkNonFiniteIntConversion = do
  result <- compile [OptEnableFeature "f16"] (sourceText nonFiniteIntConversionSource)
  case result of
    Left _ -> pure ()
    Right _ -> fail "expected non-finite integer conversion to fail"

checkRootDiagnosticIsolation :: IO ()
checkRootDiagnosticIsolation =
  withTemporaryDirectory "spirdo-diagnostic" $ \root -> do
    writeFile (root </> "library.wesl") (unlines ["diagnostic(off, const_assert);", "const_assert(false);"])
    let mainFile = root </> "main.wesl"
    writeFile mainFile (unlines ["import library;", "const_assert(false);", computeMain])
    expectFileFailure mainFile

fragmentMain :: String -> String
fragmentMain body =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , body
    , "}"
    ]

computeMain :: String
computeMain =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main() {}"
    ]

importMainSource :: String -> String
importMainSource importDecl = unlines [importDecl, fragmentMain "return vec4(0.0);"]

diamondMainSource :: String
diamondMainSource =
  unlines
    [ "import left;"
    , "import right;"
    , fragmentMain "return vec4(left::leftValue(), right::rightValue(), 0.0, 1.0);"
    ]

breakOutsideLoopSource :: String
breakOutsideLoopSource = unlines [computeHeader, "  break;", "}"]

continueOutsideLoopSource :: String
continueOutsideLoopSource = unlines [computeHeader, "  continue;", "}"]

breakIfOutsideLoopSource :: String
breakIfOutsideLoopSource = unlines [computeHeader, "  break if (true);", "}"]

breakIfInLoopBodySource :: String
breakIfInLoopBodySource = unlines [computeHeader, "  loop {", "    break if (true);", "  }", "}"]

breakIfNestedInContinuingSource :: String
breakIfNestedInContinuingSource =
  unlines
    [ computeHeader
    , "  loop {"
    , "    continuing {"
    , "      if (true) { break if (true); }"
    , "    }"
    , "  }"
    , "}"
    ]

breakIfNotFinalInContinuingSource :: String
breakIfNotFinalInContinuingSource =
  unlines
    [ computeHeader
    , "  loop {"
    , "    continuing {"
    , "      break if (true);"
    , "      let x = 1;"
    , "    }"
    , "  }"
    , "}"
    ]

breakInContinuingSource :: String
breakInContinuingSource =
  unlines [computeHeader, "  loop { continuing { break; } }", "}"]

continueInContinuingSource :: String
continueInContinuingSource =
  unlines [computeHeader, "  loop { continuing { continue; } }", "}"]

breakInSwitchSource :: String
breakInSwitchSource =
  unlines
    [ computeHeader
    , "  switch (0) {"
    , "    default: { break; }"
    , "  }"
    , "}"
    ]

continueInNestedSwitchSource :: String
continueInNestedSwitchSource =
  unlines
    [ computeHeader
    , "  loop {"
    , "    switch (0) {"
    , "      default: { continue; }"
    , "    }"
    , "  }"
    , "}"
    ]

breakIfFinalInContinuingSource :: String
breakIfFinalInContinuingSource =
  unlines
    [ computeHeader
    , "  loop {"
    , "    continuing {"
    , "      break if (true);"
    , "    }"
    , "  }"
    , "}"
    ]

nestedLoopInContinuingSource :: String
nestedLoopInContinuingSource =
  unlines
    [ computeHeader
    , "  loop {"
    , "    continuing {"
    , "      loop { break; }"
    , "      break if (true);"
    , "    }"
    , "  }"
    , "}"
    ]

computeHeader :: String
computeHeader = "@compute @workgroup_size(1)\nfn main() {"

duplicateValueDeclarationSource :: String
duplicateValueDeclarationSource =
  unlines
    [ "const duplicate = 1;"
    , "fn duplicate() -> i32 { return 2; }"
    , computeMain
    ]

duplicateGlobalDeclarationSource :: String
duplicateGlobalDeclarationSource =
  unlines
    [ "const duplicate: i32 = 1;"
    , "var<private> duplicate: i32 = 2;"
    , computeMain
    ]

duplicateTypeDeclarationSource :: String
duplicateTypeDeclarationSource =
  unlines
    [ "struct Shared { value: i32, };"
    , "alias Shared = i32;"
    , computeMain
    ]

duplicateStructDeclarationSource :: String
duplicateStructDeclarationSource =
  unlines
    [ "struct Shared { value: i32, };"
    , "struct Shared { other: i32, };"
    , computeMain
    ]

duplicateStructFieldSource :: String
duplicateStructFieldSource =
  unlines
    [ "struct Pair {"
    , "  value: f32,"
    , "  value: f32,"
    , "};"
    , computeMain
    ]

shortCircuitAndSource :: String
shortCircuitAndSource = unlines ["const_assert(!(false && (1 / 0 == 0)));", computeMain]

shortCircuitOrSource :: String
shortCircuitOrSource = unlines ["const_assert(true || (1 / 0 == 0));", computeMain]

negativeShiftSource :: String
negativeShiftSource = unlines ["const_assert((1i << -1i) == 0i);", computeMain]

widthShiftSource :: String
widthShiftSource = unlines ["const_assert((1i << 32i) == 0i);", computeMain]

hugeShiftSource :: String
hugeShiftSource = unlines ["const_assert((1i << 999999999999999999999999999999999999999999999999i) == 0i);", computeMain]

nonFiniteIntConversionSource :: String
nonFiniteIntConversionSource =
  unlines ["enable f16;", "const_assert(i32(f16(65520.0)) == 0i);", computeMain]

constIfShadowSource :: String
constIfShadowSource =
  unlines
    [ "diagnostic(off, shadowing);"
    , "fn ifShadow() -> i32 {"
    , "  let value = 1;"
    , "  if (true) { let value = 2; }"
    , "  return value;"
    , "}"
    , "const_assert(ifShadow() == 1);"
    , computeMain
    ]

constOuterMutationInnerShadowSource :: String
constOuterMutationInnerShadowSource =
  unlines
    [ "diagnostic(off, shadowing);"
    , "fn mutateOuter() -> i32 {"
    , "  var value = 1;"
    , "  if (true) {"
    , "    value = 2;"
    , "    if (true) { let value = 100; }"
    , "    value += 3;"
    , "  }"
    , "  return value;"
    , "}"
    , "const_assert(mutateOuter() == 5);"
    , computeMain
    ]

constForScopeSource :: String
constForScopeSource =
  unlines
    [ "diagnostic(off, shadowing);"
    , "fn sumIndices() -> i32 {"
    , "  var total = 0;"
    , "  for (var index = 0; index < 3; index++) {"
    , "    total += index;"
    , "    let index = 100;"
    , "  }"
    , "  return total;"
    , "}"
    , "const_assert(sumIndices() == 3);"
    , computeMain
    ]

constSwitchScopeSource :: String
constSwitchScopeSource =
  unlines
    [ "diagnostic(off, shadowing);"
    , "fn switchShadow() -> i32 {"
    , "  var value = 1;"
    , "  switch (0) {"
    , "    case 0: { let value = 9; break; }"
    , "    default: {}"
    , "  }"
    , "  return value;"
    , "}"
    , "const_assert(switchShadow() == 1);"
    , computeMain
    ]

constFallthroughScopeSource :: String
constFallthroughScopeSource =
  unlines
    [ "diagnostic(off, shadowing);"
    , "fn fallthroughScope() -> i32 {"
    , "  var value = 1;"
    , "  switch (0) {"
    , "    case 0: { let value = 9; fallthrough; }"
    , "    case 1: { value = 4; break; }"
    , "    default: {}"
    , "  }"
    , "  return value;"
    , "}"
    , "const_assert(fallthroughScope() == 4);"
    , computeMain
    ]

overrideSource :: String
overrideSource = unlines ["override scale: i32 = 0;", computeMain]

diagnosticErrorConstAssertSource :: String
diagnosticErrorConstAssertSource = unlines ["diagnostic(error, const_assert);", "const_assert(false);", computeMain]

diagnosticErrorUnusedVariableSource :: String
diagnosticErrorUnusedVariableSource =
  unlines ["diagnostic(error, unused_variable);", computeHeader, "  let unused = 1;", "}"]
