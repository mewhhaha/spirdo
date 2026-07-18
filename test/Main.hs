{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Executable entry point.
module Main (main) where

import Control.Exception (IOException, SomeException, bracket, bracketOnError, catch, displayException, finally, try)
import Control.Monad (foldM, forM_, unless, when)
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import Data.Char (isSpace, toLower)
import Data.List (find, inits, isInfixOf, isPrefixOf, stripPrefix, tails)
import qualified Data.Set as Set
import Data.Foldable (toList)
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.Word (Word16, Word32, Word64)
import GHC.Float (castFloatToWord32)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , findExecutable
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Environment (lookupEnv)
import System.FilePath ((</>), isAbsolute, takeDirectory)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, openBinaryTempFile, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck
  ( Gen
  , arbitrary
  , counterexample
  , forAllShrink
  , ioProperty
  , isSuccess
  , maxSuccess
  , quickCheckWithResult
  , shrink
  , stdArgs
  )
import Unsafe.Coerce (unsafeCoerce)

import qualified CompilerCacheRegression
import qualified EncodingRegression
import qualified EmitSemanticRegression
import qualified ImportSemanticRegression
import qualified InputSafetyRegression
import qualified LayoutConformanceRegression
import qualified PackageResolutionRegression
import qualified ParserRegression
import qualified PublicApiRegression
import qualified UniformSafetyRegression
import Spirdo.Test.Fixtures
import Spirdo.Test.Harness (runChecks, testFilterFromEnvironmentAndArgs)
import Spirdo.Wesl.Reflection
import Spirdo.Wesl.Inputs
  ( SamplerHandle(..)
  , TextureHandle(..)
  , inputsFor
  , inputsUniforms
  )
import qualified Spirdo.Wesl.Inputs as Inputs
import GHC.Generics (Generic)

data PayloadU = PayloadU
  { a :: Float
  , b :: V3 Float
  , c :: M3 Float
  , d :: [V2 Float]
  } deriving (Generic)

instance ToUniform PayloadU

newtype ParamsU = ParamsU
  { v :: V4 Float
  } deriving (Generic)

instance ToUniform ParamsU

newtype PayloadMissingU = PayloadMissingU
  { a :: Float
  } deriving (Generic)

instance ToUniform PayloadMissingU

data PayloadExtraU = PayloadExtraU
  { a :: Float
  , b :: V3 Float
  , c :: M3 Float
  , d :: [V2 Float]
  , oops :: Float
  } deriving (Generic)

instance ToUniform PayloadExtraU

data InnerU = InnerU
  { a :: V2 Float
  , b :: Float
  } deriving (Generic)

instance ToUniform InnerU

data ParamsExtendedU = ParamsExtendedU
  { m3 :: M3 Float
  , m4 :: M4 Float
  , arr2 :: [V2 Float]
  , arr3 :: [V3 Float]
  , inner :: InnerU
  } deriving (Generic)

instance ToUniform ParamsExtendedU

data Inner2U = Inner2U
  { v :: V3 Float
  , w :: Float
  } deriving (Generic)

instance ToUniform Inner2U

newtype Outer2U = Outer2U
  { inners :: [Inner2U]
  } deriving (Generic)

instance ToUniform Outer2U

data ParamsExtended2U = ParamsExtended2U
  { m34 :: M3x4 Float
  , m43 :: M4x3 Float
  , mats :: [M2 Float]
  , nested :: Outer2U
  , h :: Half
  , hv :: V3 Half
  , hv4 :: V4 Half
  } deriving (Generic)

instance ToUniform ParamsExtended2U


defaultOpts :: [Option]
defaultOpts =
  [ OptEnableFeature "f16"
  , OptEnableFeature "uniform_buffer_standard_layout"
  ]

separateOpts :: [Option]
separateOpts = [OptSamplerMode SamplerSeparate]

data ParityExpected
  = ParityPass
  | ParityFail
  | ParityXFail
  deriving (Eq, Show)

data ParityCase = ParityCase
  { id :: String
  , domain :: String
  , specRef :: String
  , kind :: String
  , expected :: ParityExpected
  , source :: ParitySource
  , origin :: String
  , originRef :: String
  , options :: [Option]
  , errContains :: Maybe String
  , oracles :: [String]
  , owner :: Maybe String
  , exitCriteria :: Maybe String
  } deriving (Show)

data ParitySource
  = ParityFile FilePath
  | ParityInline String String
  deriving (Show)

isTruthy :: String -> Bool
isTruthy raw =
  let lower = map toLower raw
  in lower == "1" || lower == "true" || lower == "yes" || lower == "on"

inlineSource :: String -> Source
inlineSource = SourceInline "<inline>"

compileInline :: [Option] -> String -> Either CompileError SomeShader
compileInline opts src = compileWith opts (inlineSource src)

compileBytes :: [Option] -> String -> Either CompileError BS.ByteString
compileBytes opts src = do
  SomeShader shader <- compileInline opts src
  pure (shaderSpirv shader)

compileBytesWithDiagnostics :: [Option] -> String -> Either CompileError (BS.ByteString, [Diagnostic])
compileBytesWithDiagnostics opts src = do
  (SomeShader shader, diags) <- compileWithDiagnostics opts (inlineSource src)
  pure ((shaderSpirv shader), diags)

smokeShaders :: [(String, String)]
smokeShaders =
  [ ("compute-atomics", computeShader)
  , ("compute-barriers", barrierShader)
  , ("atomic-compare-exchange", atomicCompareExchangeShader)
  , ("typed-ctors", typedCtorShader)
  , ("fragment-derivatives", fragmentShader)
  , ("fragment-sample-mask", sampleMaskShader)
  , ("vertex-struct-io", vertexShader)
  , ("vertex-io-attrs", vertexIoAttrShader)
  , ("storage-texture", storageTextureShader)
  , ("sampler-texture", samplerShader)
  , ("bitwise-ops", bitwiseShader)
  , ("builtin-extras", builtinExtraShader)
  , ("runtime-array-length", runtimeArrayLengthShader)
  , ("alias-override", aliasOverrideShader)
  , ("layout-attrs", layoutAttrShader)
  , ("globals-f16", globalsShader)
  , ("switch-loop", switchLoopShader)
  , ("switch-fallthrough", switchFallthroughShader)
  , ("const-arith", constArithShader)
  , ("const-float", constFloatShader)
  , ("const-composite", constCompositeShader)
  , ("const-fn", constFnShader)
  , ("discard", discardShader)
  , ("ptr-abstract-literals", pointerShader)
  , ("texture-variants", textureVariantsShader)
  , ("texture-load-sampled", textureLoadShader)
  , ("storage-texture-array", storageTextureArrayShader)
  , ("texture-advanced", textureAdvancedShader)
  ]

inlineParitySources :: [(String, String)]
inlineParitySources =
  [ ("computeShader", computeShader)
  , ("barrierShader", barrierShader)
  , ("atomicCompareExchangeShader", atomicCompareExchangeShader)
  , ("typedCtorShader", typedCtorShader)
  , ("sampleMaskShader", sampleMaskShader)
  , ("fragmentShader", fragmentShader)
  , ("vertexIoAttrShader", vertexIoAttrShader)
  , ("vertexShader", vertexShader)
  , ("storageTextureShader", storageTextureShader)
  , ("samplerShader", samplerShader)
  , ("fragmentBlendSrcShader", fragmentBlendSrcShader)
  , ("bitwiseShader", bitwiseShader)
  , ("builtinExtraShader", builtinExtraShader)
  , ("runtimeArrayLengthShader", runtimeArrayLengthShader)
  , ("aliasOverrideShader", aliasOverrideShader)
  , ("switchLoopShader", switchLoopShader)
  , ("switchFallthroughShader", switchFallthroughShader)
  , ("constArithShader", constArithShader)
  , ("constFloatShader", constFloatShader)
  , ("constCompositeShader", constCompositeShader)
  , ("constFnShader", constFnShader)
  , ("discardShader", discardShader)
  , ("pointerShader", pointerShader)
  , ("textureVariantsShader", textureVariantsShader)
  , ("textureLoadShader", textureLoadShader)
  , ("storageTextureArrayShader", storageTextureArrayShader)
  , ("textureAdvancedShader", textureAdvancedShader)
  , ("globalsShader", globalsShader)
  , ("layoutAttrShader", layoutAttrShader)
  , ("fragmentBlendSrcNoEnableShader", fragmentBlendSrcNoEnableShader)
  , ("fragmentBlendSrcOnlyOneShader", fragmentBlendSrcOnlyOneShader)
  , ("fragmentBlendSrcLocationOneShader", fragmentBlendSrcLocationOneShader)
  , ("textureBarrierFragmentShader", textureBarrierFragmentShader)
  , ("vertexInterpolateIntegerShader", vertexInterpolateIntegerShader)
  , ("vertexInvariantLocationShader", vertexInvariantLocationShader)
  , ("vertexBoolLocationShader", vertexBoolLocationShader)
  , ("f16NoEnableShader", f16NoEnableShader)
  , ("storageWriteAccessShader", storageWriteAccessShader)
  , ("pointerParamWorkgroupShader", pointerParamWorkgroupShader)
  , ("pointerParamStorageShader", pointerParamStorageShader)
  , ("nagaEntryPointerParam", nagaEntryPointerParam)
  , ("nagaFragmentReturnNoBinding", nagaFragmentReturnNoBinding)
  , ("nagaNonEntryParamIoAttrs", nagaNonEntryParamIoAttrs)
  , ("nagaNegativeLocation", nagaNegativeLocation)
  , ("nagaLocationTooManyArgs", nagaLocationTooManyArgs)
  , ("nagaStageAttrWithArgs", nagaStageAttrWithArgs)
  , ("nagaDuplicateLocationReturn", nagaDuplicateLocationReturn)
  , ("nagaDuplicateBuiltinReturn", nagaDuplicateBuiltinReturn)
  , ("nagaDuplicateGroupAttr", nagaDuplicateGroupAttr)
  , ("nagaDuplicateBindingAttr", nagaDuplicateBindingAttr)
  , ("nagaDuplicateFragmentStageAttr", nagaDuplicateFragmentStageAttr)
  , ("nagaDuplicateWorkgroupSizeAttr", nagaDuplicateWorkgroupSizeAttr)
  , ("nagaTextureBarrierFragment", nagaTextureBarrierFragment)
  , ("nagaStorageWriteBuffer", nagaStorageWriteBuffer)
  , ("nagaF16Enable", nagaF16Enable)
  , ("nagaInterpolateFloatLinear", nagaInterpolateFloatLinear)
  , ("duplicateBindingShader", duplicateBindingShader)
  , ("malformedHexLiteralShader", malformedHexLiteralShader)
  , ("badStructFieldShader", badStructFieldShader)
  , ("nonIfStatementAttrShader", nonIfStatementAttrShader)
  , ("nonIfSwitchCaseAttrShader", nonIfSwitchCaseAttrShader)
  , ("nonIfLoopAttrShader", nonIfLoopAttrShader)
  , ("negativeI32RangeShader", negativeI32RangeShader)
  , ("computeWithoutWorkgroupSizeShader", computeWithoutWorkgroupSizeShader)
  , ("workgroupOverrideShader", workgroupOverrideShader)
  , ("moduleConstDeclShader", moduleConstDeclShader)
  , ("invalidMatrixDimensionsShader", invalidMatrixDimensionsShader)
  , ("badSwitchShader", badSwitchShader)
  , ("badConstAssertShader", badConstAssertShader)
  ]

lookupInlineParitySource :: String -> Maybe String
lookupInlineParitySource key = lookup key inlineParitySources

main :: IO ()
main = do
  testFilter <- testFilterFromEnvironmentAndArgs
  spirvVal <- findExecutable "spirv-val"
  nagaExe <- findExecutable "naga"
  configuredValidatorRequirement <- lookupEnv "SPIRDO_REQUIRE_VALIDATORS"
  requireValidators <-
    case configuredValidatorRequirement of
      Nothing -> pure False
      Just rawValue ->
        case map toLower rawValue of
          "1" -> pure True
          "true" -> pure True
          "yes" -> pure True
          "on" -> pure True
          "0" -> pure False
          "false" -> pure False
          "no" -> pure False
          "off" -> pure False
          _ -> fail ("SPIRDO_REQUIRE_VALIDATORS must be a boolean, got " <> show rawValue)
  when requireValidators $ do
    when (isNothing spirvVal) $
      fail "SPIRDO_REQUIRE_VALIDATORS=1 but spirv-val was not found in PATH"
    when (isNothing nagaExe) $
      fail "SPIRDO_REQUIRE_VALIDATORS=1 but naga was not found in PATH"
  parityCases <- loadParityManifest ("test" </> "parity" </> "manifest.tsv")
  parityRules <- loadParityRules ("test" </> "parity" </> "rules.tsv")
  assertParityRuleCoverage parityRules parityCases
  smokeCheckCount <- runChecks testFilter "Smoke Compile"
    [ ( label
      , do
          bytes <- case compileBytes defaultOpts src of
            Left err -> fail (label <> ": " <> show err)
            Right bs -> pure bs
          assertSpirv spirvVal label bytes
      )
    | (label, src) <- smokeShaders
    ]

  importCheckCount <- runChecks testFilter "Imports and Parity Fixtures" $
    [ ("if-translation", checkIfTranslation)
    , ("import-compile", checkImportCompile spirvVal)
    , ("import-item-compile", checkImportItemCompile spirvVal)
    , ("import-alias-compile", checkImportAliasCompile spirvVal)
    , ("import-struct-zero-ctor", checkImportStructZeroCtorCompile spirvVal)
    , ("import-qualified-const-compile", checkImportQualifiedConstCompile spirvVal)
    ]
      <> [ ( "parity:" <> parityCase.id <> " [" <> parityCase.specRef <> "]" <> if isBacklogCase parityCase then " [backlog]" else ""
           , runParityCase spirvVal nagaExe parityCase
           )
         | parityCase <- filter (not . isUnmappedBacklogCase) parityCases
         ]

  languageCheckCount <- runChecks testFilter "Language and Typecheck"
    [ ("switch-const-validation", checkSwitchConstValidation)
    , ("const-assert-validation", checkConstAssertValidation)
    , ("malformed-hex-literal", checkMalformedHexLiteral)
    , ("struct-field-separators", checkStructFieldSeparators)
    , ("non-if-statement-attrs-rejected", checkNonIfStatementAttrsRejected)
    , ("non-if-switch-case-attrs-rejected", checkNonIfSwitchCaseAttrsRejected)
    , ("non-if-loop-attrs-rejected", checkNonIfLoopAttrsRejected)
    , ("module-const-decl", checkModuleConstDecl)
    , ("invalid-matrix-dimensions-rejected", checkInvalidMatrixDimensionsRejected)
    , ("negative-i32-range", checkNegativeI32Range)
    , ("compute-requires-workgroup-size", checkComputeRequiresWorkgroupSize)
    , ("super-import-containment", checkSuperImportContainment)
    ]

  overrideCheckCount <- runChecks testFilter "Overrides and Diagnostics"
    [ ("override-specialization", checkOverrideSpecialization spirvVal)
    , ("override-default", checkOverrideDefault spirvVal)
    , ("override-missing", checkOverrideMissing)
    , ("workgroup-size-override-reject", checkWorkgroupSizeOverrideReject)
    , ("override-dependency", checkOverrideDependency spirvVal)
    , ("override-parity-mode", checkOverrideParityMode)
    , ("diagnostic-override", checkDiagnosticOverride)
    , ("diagnostic-warning", checkDiagnosticWarning)
    , ("diagnostic-unreachable", checkDiagnosticUnreachable)
    , ("diagnostic-unused-expression", checkDiagnosticUnusedExpr)
    , ("diagnostic-unused-variable", checkDiagnosticUnusedVar)
    , ("diagnostic-unused-parameter", checkDiagnosticUnusedParam)
    , ("diagnostic-shadowing", checkDiagnosticShadowing)
    , ("diagnostic-constant-condition", checkDiagnosticConstantCondition)
    , ("diagnostic-duplicate-case", checkDiagnosticDuplicateCase)
    ]

  compatibilityCheckCount <- runChecks testFilter "WGSL Compatibility and Oracles"
    [ ("naga-oracle-parity", checkNagaOracleParity nagaExe)
    , ("texture-barrier-stage", checkTextureBarrierStage)
    , ("blend-src-enabled", checkBlendSrcEnabled spirvVal)
    , ("blend-src-requires-enable", checkBlendSrcRequiresEnable)
    , ("blend-src-pair-rules", checkBlendSrcPairRules)
    , ("interpolate-integer-rule", checkInterpolateIntegerRule)
    , ("invariant-rule", checkInvariantRule)
    , ("location-io-type-rule", checkLocationIoTypeRule)
    , ("f16-requires-enable", checkF16RequiresEnable)
    , ("storage-write-access-rejected", checkStorageWriteAccessRejected)
    , ("pointer-param-address-space-rules", checkPointerParamAddressSpaceRules)
    , ("entry-pointer-param-rejected", checkEntryPointerParamRejected)
    , ("fragment-return-binding-required", checkFragmentReturnBindingRequired)
    , ("non-entry-param-io-attrs-rejected", checkNonEntryParamIoAttrsRejected)
    , ("negative-location-rejected", checkNegativeLocationRejected)
    , ("location-too-many-args-rejected", checkLocationTooManyArgsRejected)
    , ("stage-attr-with-args-rejected", checkStageAttrWithArgsRejected)
    , ("duplicate-location-attr-rejected", checkDuplicateLocationAttrRejected)
    , ("duplicate-builtin-attr-rejected", checkDuplicateBuiltinAttrRejected)
    , ("duplicate-group-binding-attrs-rejected", checkDuplicateGroupBindingAttrsRejected)
    , ("duplicate-stage-attrs-rejected", checkDuplicateStageAttrsRejected)
    , ("non-entry-function-attr-accepted", checkNonEntryFunctionAttrAccepted)
    ]

  interfaceCheckCount <- runChecks testFilter "Interface, Uniforms, and Inputs"
    [ ("sampler-interface", checkSamplerInterface)
    , ("combined-sampler-interface", checkCombinedSamplerInterface)
    , ("sampler-value-combined-error", checkSamplerValueCombinedError)
    , ("pack-uniform-layout", checkPackUniformLayout)
    , ("pack-uniform-errors", checkPackUniformErrors)
    , ("pack-uniform-from", checkPackUniformFrom)
    , ("uniform-storable", checkUniformStorable)
    , ("vertex-attributes", checkVertexAttributes)
    , ("binding-plan", checkBindingPlan)
    , ("input-ordering", checkInputOrdering)
    , ("inputs-combined-ok", checkInputsCombinedOk)
    , ("inputs-missing-bindings-rejected", checkInputsMissingBindingsRejected)
    , ("inputs-separate-mode-rejects-sampled-texture", checkInputsSeparateModeRejectsSampledTexture)
    , ("inputs-duplicate-builder", checkInputsDuplicateBuilder)
    , ("uniform-quickcheck", checkQuickCheck)
    ]

  regressionCheckCount <- runChecks testFilter "Regression Gates"
    [ ("duplicate-bindings", checkDuplicateBindings)
    , ("golden-spirv", checkGoldenSpirv)
    , ("short-spirv-header", checkShortSpirvHeader)
    ]
  featureCheckCount <- runChecks testFilter "Feature Regressions" $
    ImportSemanticRegression.checks
      <> CompilerCacheRegression.checks
      <> EncodingRegression.checks
      <> EmitSemanticRegression.checks
      <> InputSafetyRegression.checks
      <> LayoutConformanceRegression.checks
      <> PackageResolutionRegression.checks
      <> ParserRegression.checks
      <> PublicApiRegression.checks
      <> UniformSafetyRegression.checks
  let selectedCheckCount =
        sum
          [ smokeCheckCount
          , importCheckCount
          , languageCheckCount
          , overrideCheckCount
          , compatibilityCheckCount
          , interfaceCheckCount
          , regressionCheckCount
          , featureCheckCount
          ]
  when (isJust testFilter && selectedCheckCount == 0) $
    fail ("test filter matched no checks: " <> show testFilter)
  putStrLn "All selected tests passed."

assertSpirv :: Maybe FilePath -> String -> BS.ByteString -> IO ()
assertSpirv spirvVal label bytes = do
  unless (BS.length bytes >= 20) $
    fail (label <> ": SPIR-V output is shorter than the 20-byte header: " <> show (BS.length bytes) <> " bytes")
  unless (BS.length bytes `mod` 4 == 0) $
    fail (label <> ": SPIR-V size not multiple of 4")
  let magic = word32At bytes 0
  unless (magic == 0x07230203) $
    fail (label <> ": bad SPIR-V magic")
  validateSpirvVal spirvVal label bytes

checkShortSpirvHeader :: IO ()
checkShortSpirvHeader = do
  result <- try (assertSpirv Nothing "short-header" (BS.replicate 4 0))
  case result of
    Left err
      | "shorter than the 20-byte header: 4 bytes" `isInfixOf` displayException (err :: SomeException) -> pure ()
      | otherwise -> fail ("short SPIR-V header returned the wrong error: " <> displayException err)
    Right () -> fail "four bytes were accepted as a complete SPIR-V module"

word32At :: BS.ByteString -> Int -> Word32
word32At bytes offset =
  let b0 = fromIntegral (BS.index bytes offset) :: Word32
      b1 = fromIntegral (BS.index bytes (offset + 1)) :: Word32
      b2 = fromIntegral (BS.index bytes (offset + 2)) :: Word32
      b3 = fromIntegral (BS.index bytes (offset + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

word16At :: BS.ByteString -> Int -> Word16
word16At bytes offset =
  let b0 = fromIntegral (BS.index bytes offset) :: Word16
      b1 = fromIntegral (BS.index bytes (offset + 1)) :: Word16
  in b0 .|. (b1 `shiftL` 8)

validateSpirvVal :: Maybe FilePath -> String -> BS.ByteString -> IO ()
validateSpirvVal mSpirvVal label bytes =
  case mSpirvVal of
    Nothing -> pure ()
    Just exe -> do
      tmpDir <- getTemporaryDirectory
      let safeLabel = map sanitize label
      bracket
        (openBinaryTempFile tmpDir (safeLabel <> ".spv"))
        cleanupTemporaryFile
        $ \(path, handle) -> do
            BS.hPut handle bytes
            hClose handle
            (code, _out, err) <- readProcessWithExitCode exe [path] ""
            case code of
              ExitSuccess -> pure ()
              ExitFailure _ -> fail (label <> ": spirv-val failed: " <> err)
  where
    sanitize c
      | c == '/' || c == '\\' = '_'
      | otherwise = c

cleanupTemporaryFile :: (FilePath, Handle) -> IO ()
cleanupTemporaryFile (path, handle) = do
  hClose handle `catch` ignoreIOException
  removeIfExists path

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = pure ()


checkSamplerInterface :: IO ()
checkSamplerInterface =
  case compileInline separateOpts samplerShader of
    Left err -> fail ("sampler-interface: " <> show err)
    Right (SomeShader shader) -> do
      let kinds = map (.biKind) (shaderInterface shader).siBindings
      unless (BTexture2D `elem` kinds && BSampler `elem` kinds) $
        fail "sampler-interface: expected texture_2d and sampler bindings"

checkCombinedSamplerInterface :: IO ()
checkCombinedSamplerInterface =
  case compileInline defaultOpts samplerShader of
    Left err -> fail ("sampler-combined-interface: " <> show err)
    Right (SomeShader shader) -> do
      let kinds = map (.biKind) (shaderInterface shader).siBindings
      when (BSampler `elem` kinds) $
        fail "sampler-combined-interface: sampler binding should be omitted in combined mode"

checkSamplerValueCombinedError :: IO ()
checkSamplerValueCombinedError =
  case compileInline defaultOpts samplerValueShader of
    Left (CompileError msg _ _) ->
      unless ("sampler values are unavailable in combined mode" `isInfixOf` msg) $
        fail ("sampler-combined-error: unexpected error: " <> msg)
    Right _ ->
      fail "sampler-combined-error: expected failure when say using sampler value in combined mode"

checkTextureBarrierStage :: IO ()
checkTextureBarrierStage =
  case compileInline defaultOpts textureBarrierFragmentShader of
    Left (CompileError msg _ _) ->
      unless ("textureBarrier is not available in this shader stage" `isInfixOf` msg) $
        fail ("texture-barrier-stage: unexpected error: " <> msg)
    Right _ ->
      fail "texture-barrier-stage: expected failure for textureBarrier in fragment stage"

checkNagaOracleParity :: Maybe FilePath -> IO ()
checkNagaOracleParity mNaga =
  case mNaga of
    Nothing -> pure ()
    Just nagaExe -> do
      let cases =
            [ ("naga-parity:texture-barrier-fragment", defaultOpts, nagaTextureBarrierFragment, False)
            , ("naga-parity:storage-write-buffer", defaultOpts, nagaStorageWriteBuffer, False)
            , ("naga-parity:blend-src-enabled", defaultOpts <> [OptEnableFeature "dual_source_blending"], nagaBlendSrcEnabled, True)
            , ("naga-parity:blend-src-no-enable", defaultOpts <> [OptEnableFeature "dual_source_blending"], nagaBlendSrcNoEnable, False)
            , ("naga-parity:f16-no-enable", defaultOpts, nagaF16NoEnable, False)
            , ("naga-parity:f16-enable", defaultOpts, nagaF16Enable, True)
            , ("naga-parity:interpolate-int-linear", defaultOpts, nagaInterpolateIntLinear, False)
            , ("naga-parity:interpolate-float-linear", defaultOpts, nagaInterpolateFloatLinear, True)
            , ("naga-parity:pointer-param-workgroup", defaultOpts, pointerParamWorkgroupShader, False)
            , ("naga-parity:pointer-param-storage", defaultOpts, pointerParamStorageShader, False)
            , ("naga-parity:entry-pointer-param", defaultOpts, nagaEntryPointerParam, False)
            , ("naga-parity:fragment-return-no-binding", defaultOpts, nagaFragmentReturnNoBinding, False)
            , ("naga-parity:non-entry-param-io-attrs", defaultOpts, nagaNonEntryParamIoAttrs, False)
            , ("naga-parity:negative-location", defaultOpts, nagaNegativeLocation, False)
            , ("naga-parity:location-too-many-args", defaultOpts, nagaLocationTooManyArgs, False)
            , ("naga-parity:duplicate-location-return", defaultOpts, nagaDuplicateLocationReturn, False)
            , ("naga-parity:duplicate-builtin-return", defaultOpts, nagaDuplicateBuiltinReturn, False)
            , ("naga-parity:duplicate-group-attr", defaultOpts, nagaDuplicateGroupAttr, False)
            , ("naga-parity:duplicate-binding-attr", defaultOpts, nagaDuplicateBindingAttr, False)
            , ("naga-parity:duplicate-fragment-stage-attr", defaultOpts, nagaDuplicateFragmentStageAttr, False)
            , ("naga-parity:duplicate-workgroup-size-attr", defaultOpts, nagaDuplicateWorkgroupSizeAttr, False)
            , ("naga-parity:stage-attr-with-args", defaultOpts, nagaStageAttrWithArgs, False)
            , ("naga-parity:non-entry-fn-attr", defaultOpts, nagaNonEntryFunctionAttr, True)
            ]
      forM_ cases $ \(label, opts, src, expectedOk) -> do
        spirdoOk <- case compileInline opts src of
          Left _ -> pure False
          Right _ -> pure True
        (nagaOk, nagaLog) <- runNagaCheck nagaExe label src
        when (spirdoOk /= nagaOk) $
          fail
            ( label
                <> ": spirdo/naga mismatch (spirdo="
                <> show spirdoOk
                <> ", naga="
                <> show nagaOk
                <> ")\n"
                <> nagaLog
            )
        when (spirdoOk /= expectedOk) $
          fail
            ( label
                <> ": unexpected result (got "
                <> show spirdoOk
                <> ", expected "
                <> show expectedOk
                <> ")"
            )

runNagaCheck :: FilePath -> String -> String -> IO (Bool, String)
runNagaCheck nagaExe label src = do
  tmpDir <- getTemporaryDirectory
  (srcPath, handle) <- openTempFile tmpDir "spirdo-naga.wgsl"
  hClose handle
  let outPath = srcPath <> ".spv"
      cleanup = removeIfExists srcPath >> removeIfExists outPath
  (do
      writeFile srcPath src
      (code, out, err) <- readProcessWithExitCode nagaExe [srcPath, outPath, "--input-kind", "wgsl"] ""
      case code of
        ExitSuccess -> pure (True, out <> err)
        ExitFailure _ -> pure (False, label <> ": " <> out <> err)
    ) `finally` cleanup

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)

checkBlendSrcEnabled :: Maybe FilePath -> IO ()
checkBlendSrcEnabled spirvVal =
  case compileBytes [OptEnableFeature "dual_source_blending"] fragmentBlendSrcShader of
    Left err -> fail ("blend-src-enabled: " <> show err)
    Right bytes -> assertSpirv spirvVal "blend-src-enabled" bytes

checkBlendSrcRequiresEnable :: IO ()
checkBlendSrcRequiresEnable =
  case compileInline [OptEnableFeature "dual_source_blending"] fragmentBlendSrcNoEnableShader of
    Left (CompileError msg _ _) ->
      unless ("@blend_src requires `enable dual_source_blending;`" `isInfixOf` msg) $
        fail ("blend-src-requires-enable: unexpected error: " <> msg)
    Right _ ->
      fail "blend-src-requires-enable: expected failure when shader omits enable dual_source_blending"

checkBlendSrcPairRules :: IO ()
checkBlendSrcPairRules = do
  case compileInline [OptEnableFeature "dual_source_blending"] fragmentBlendSrcOnlyOneShader of
    Left (CompileError msg _ _) ->
      unless ("if @blend_src is used, fragment outputs must be exactly two @location(0) fields with @blend_src(0) and @blend_src(1)" `isInfixOf` msg) $
        fail ("blend-src-pair-rules(one): unexpected error: " <> msg)
    Right _ ->
      fail "blend-src-pair-rules(one): expected failure for incomplete @blend_src pair"
  case compileInline [OptEnableFeature "dual_source_blending"] fragmentBlendSrcLocationOneShader of
    Left (CompileError msg _ _) ->
      unless
        ( "@blend_src is only valid on @location(0)" `isInfixOf` msg
            || "if @blend_src is used, fragment outputs must be exactly two @location(0) fields with @blend_src(0) and @blend_src(1)" `isInfixOf` msg
        )
        $
        fail ("blend-src-pair-rules(location): unexpected error: " <> msg)
    Right _ ->
      fail "blend-src-pair-rules(location): expected failure for @blend_src on non-zero location"

checkInterpolateIntegerRule :: IO ()
checkInterpolateIntegerRule =
  case compileInline defaultOpts vertexInterpolateIntegerShader of
    Left (CompileError msg _ _) ->
      unless ("@interpolate(perspective|linear, ...): only floating-point scalars/vectors are allowed" `isInfixOf` msg) $
        fail ("interpolate-integer-rule: unexpected error: " <> msg)
    Right _ ->
      fail "interpolate-integer-rule: expected failure for non-flat interpolation on integer IO"

checkInvariantRule :: IO ()
checkInvariantRule =
  case compileInline defaultOpts vertexInvariantLocationShader of
    Left (CompileError msg _ _) ->
      unless ("@invariant is only allowed on @builtin(position) vertex outputs and fragment inputs" `isInfixOf` msg) $
        fail ("invariant-rule: unexpected error: " <> msg)
    Right _ ->
      fail "invariant-rule: expected failure for @invariant on @location IO"

checkLocationIoTypeRule :: IO ()
checkLocationIoTypeRule =
  case compileInline defaultOpts vertexBoolLocationShader of
    Left (CompileError msg _ _) ->
      unless ("stage @location inputs/outputs must be scalar or vector i32/u32/f16/f32" `isInfixOf` msg) $
        fail ("location-io-type-rule: unexpected error: " <> msg)
    Right _ ->
      fail "location-io-type-rule: expected failure for bool @location IO"

checkF16RequiresEnable :: IO ()
checkF16RequiresEnable =
  case compileInline defaultOpts f16NoEnableShader of
    Left (CompileError msg _ _) ->
      unless ("f16 usage requires `enable f16;`" `isInfixOf` msg) $
        fail ("f16-requires-enable: unexpected error: " <> msg)
    Right _ ->
      fail "f16-requires-enable: expected failure for f16 usage without enable directive"

checkStorageWriteAccessRejected :: IO ()
checkStorageWriteAccessRejected =
  case compileInline defaultOpts storageWriteAccessShader of
    Left (CompileError msg _ _) ->
      unless ("unsupported storage access: write" `isInfixOf` msg) $
        fail ("storage-write-access-rejected: unexpected error: " <> msg)
    Right _ ->
      fail "storage-write-access-rejected: expected failure for var<storage, write> buffer"

checkPointerParamAddressSpaceRules :: IO ()
checkPointerParamAddressSpaceRules = do
  case compileInline defaultOpts pointerParamWorkgroupShader of
    Left (CompileError msg _ _) ->
      unless ("function pointer parameters must use ptr<function,...> or ptr<private,...>" `isInfixOf` msg) $
        fail ("pointer-param-workgroup: unexpected error: " <> msg)
    Right _ ->
      fail "pointer-param-workgroup: expected failure for ptr<workgroup,...> function parameter"
  case compileInline defaultOpts pointerParamStorageShader of
    Left (CompileError msg _ _) ->
      unless ("function pointer parameters must use ptr<function,...> or ptr<private,...>" `isInfixOf` msg) $
        fail ("pointer-param-storage: unexpected error: " <> msg)
    Right _ ->
      fail "pointer-param-storage: expected failure for ptr<storage,...> function parameter"

checkEntryPointerParamRejected :: IO ()
checkEntryPointerParamRejected =
  case compileInline defaultOpts nagaEntryPointerParam of
    Left (CompileError msg _ _) ->
      unless ("entry point parameters cannot be pointers" `isInfixOf` msg) $
        fail ("entry-pointer-param: unexpected error: " <> msg)
    Right _ ->
      fail "entry-pointer-param: expected failure for pointer entry parameter"

checkFragmentReturnBindingRequired :: IO ()
checkFragmentReturnBindingRequired =
  case compileInline defaultOpts nagaFragmentReturnNoBinding of
    Left (CompileError msg _ _) ->
      unless ("fragment entry point return must use @location or @builtin" `isInfixOf` msg) $
        fail ("fragment-return-binding: unexpected error: " <> msg)
    Right _ ->
      fail "fragment-return-binding: expected failure for unbound fragment return"

checkNonEntryParamIoAttrsRejected :: IO ()
checkNonEntryParamIoAttrsRejected =
  case compileInline defaultOpts nagaNonEntryParamIoAttrs of
    Left (CompileError msg _ _) ->
      unless ("parameter attributes are only allowed on entry points" `isInfixOf` msg) $
        fail ("non-entry-param-io-attrs: unexpected error: " <> msg)
    Right _ ->
      fail "non-entry-param-io-attrs: expected failure for @location on non-entry function parameter"

checkNegativeLocationRejected :: IO ()
checkNegativeLocationRejected =
  case compileInline defaultOpts nagaNegativeLocation of
    Left (CompileError msg _ _) ->
      unless ("@location must be a non-negative 32-bit integer" `isInfixOf` msg) $
        fail ("negative-location: unexpected error: " <> msg)
    Right _ ->
      fail "negative-location: expected failure for @location(-1)"

checkLocationTooManyArgsRejected :: IO ()
checkLocationTooManyArgsRejected =
  case compileInline defaultOpts nagaLocationTooManyArgs of
    Left (CompileError msg _ _) ->
      unless ("@location expects exactly one integer argument" `isInfixOf` msg) $
        fail ("location-too-many-args: unexpected error: " <> msg)
    Right _ ->
      fail "location-too-many-args: expected failure for @location with two args"

checkStageAttrWithArgsRejected :: IO ()
checkStageAttrWithArgsRejected =
  case compileInline defaultOpts nagaStageAttrWithArgs of
    Left (CompileError msg _ _) ->
      unless ("invalid entry point attributes" `isInfixOf` msg) $
        fail ("stage-attr-with-args: unexpected error: " <> msg)
    Right _ ->
      fail "stage-attr-with-args: expected failure for stage attribute with args"

checkDuplicateLocationAttrRejected :: IO ()
checkDuplicateLocationAttrRejected =
  case compileInline defaultOpts nagaDuplicateLocationReturn of
    Left (CompileError msg _ _) ->
      unless ("duplicate @location attributes" `isInfixOf` msg) $
        fail ("duplicate-location-return: unexpected error: " <> msg)
    Right _ ->
      fail "duplicate-location-return: expected failure for duplicate @location return attributes"

checkDuplicateBuiltinAttrRejected :: IO ()
checkDuplicateBuiltinAttrRejected =
  case compileInline defaultOpts nagaDuplicateBuiltinReturn of
    Left (CompileError msg _ _) ->
      unless ("duplicate @builtin attributes" `isInfixOf` msg) $
        fail ("duplicate-builtin-return: unexpected error: " <> msg)
    Right _ ->
      fail "duplicate-builtin-return: expected failure for duplicate @builtin return attributes"

checkDuplicateGroupBindingAttrsRejected :: IO ()
checkDuplicateGroupBindingAttrsRejected = do
  case compileInline defaultOpts nagaDuplicateGroupAttr of
    Left (CompileError msg _ _) ->
      unless ("duplicate @group attributes" `isInfixOf` msg) $
        fail ("duplicate-group-attr: unexpected error: " <> msg)
    Right _ ->
      fail "duplicate-group-attr: expected failure for duplicate @group attributes"
  case compileInline defaultOpts nagaDuplicateBindingAttr of
    Left (CompileError msg _ _) ->
      unless ("duplicate @binding attributes" `isInfixOf` msg) $
        fail ("duplicate-binding-attr: unexpected error: " <> msg)
    Right _ ->
      fail "duplicate-binding-attr: expected failure for duplicate @binding attributes"

checkDuplicateStageAttrsRejected :: IO ()
checkDuplicateStageAttrsRejected = do
  case compileInline defaultOpts nagaDuplicateFragmentStageAttr of
    Left (CompileError msg _ _) ->
      unless ("invalid entry point attributes" `isInfixOf` msg) $
        fail ("duplicate-fragment-stage-attr: unexpected error: " <> msg)
    Right _ ->
      fail "duplicate-fragment-stage-attr: expected failure for duplicate @fragment attributes"
  case compileInline defaultOpts nagaDuplicateWorkgroupSizeAttr of
    Left (CompileError msg _ _) ->
      unless ("invalid entry point attributes" `isInfixOf` msg) $
        fail ("duplicate-workgroup-size-attr: unexpected error: " <> msg)
    Right _ ->
      fail "duplicate-workgroup-size-attr: expected failure for duplicate @workgroup_size attributes"

checkNonEntryFunctionAttrAccepted :: IO ()
checkNonEntryFunctionAttrAccepted =
  case compileBytes defaultOpts nagaNonEntryFunctionAttr of
    Left err -> fail ("non-entry-fn-attr: " <> show err)
    Right _ -> pure ()

checkPackUniformLayout :: IO ()
checkPackUniformLayout =
  case compileInline defaultOpts packUniformShader of
    Left err -> fail ("pack-uniform-layout: " <> show err)
    Right (SomeShader shader) -> do
      info <- case find (\b -> b.biName == "payload") (shaderInterface shader).siBindings of
        Nothing -> fail "pack-uniform-layout: missing payload binding"
        Just bi -> pure bi
      let value =
            PayloadU
              { a = 1.25
              , b = V3 2.0 3.0 4.0
              , c = M3 (V3 10.0 11.0 12.0) (V3 13.0 14.0 15.0) (V3 16.0 17.0 18.0)
              , d = [V2 21.0 22.0, V2 23.0 24.0]
              }
      bytes <-
        case packUniformFrom info.biType value of
          Left err -> fail ("pack-uniform-layout: " <> err)
          Right bs -> pure bs
      case info.biType of
        TLStruct _ fields _ size -> do
          unless (BS.length bytes == fromIntegral size) $
            fail "pack-uniform-layout: byte size mismatch"
          aOffset <- fieldOffset "a" fields
          bOffset <- fieldOffset "b" fields
          cField <- fieldLayout "c" fields
          dField <- fieldLayout "d" fields
          assertFloatAt "pack-uniform-layout:a" bytes aOffset 1.25
          assertFloatAt "pack-uniform-layout:b0" bytes bOffset 2.0
          assertFloatAt "pack-uniform-layout:b1" bytes (bOffset + 4) 3.0
          assertFloatAt "pack-uniform-layout:b2" bytes (bOffset + 8) 4.0
          let paddingBytes = [BS.index bytes (bOffset + 12 + i) | i <- [0 .. 3]]
          unless (all (== 0) paddingBytes) $
            fail "pack-uniform-layout: vec3 padding not zeroed"
          case cField.flType of
            TLMatrix _ rows _ _ _ stride -> do
              let cOffset = fromIntegral cField.flOffset
              let stride' = fromIntegral stride
              assertFloatAt "pack-uniform-layout:c00" bytes (cOffset + 0 * stride' + 0 * 4) 10.0
              assertFloatAt "pack-uniform-layout:c11" bytes (cOffset + 1 * stride' + 1 * 4) 14.0
              assertFloatAt "pack-uniform-layout:c22" bytes (cOffset + 2 * stride' + (rows - 1) * 4) 18.0
            _ -> fail "pack-uniform-layout: expected matrix layout for c"
          case dField.flType of
            TLArray _ stride _ _ _ -> do
              let dOffset = fromIntegral dField.flOffset
              let stride' = fromIntegral stride
              assertFloatAt "pack-uniform-layout:d0x" bytes dOffset 21.0
              assertFloatAt "pack-uniform-layout:d0y" bytes (dOffset + 4) 22.0
              assertFloatAt "pack-uniform-layout:d1x" bytes (dOffset + stride') 23.0
              assertFloatAt "pack-uniform-layout:d1y" bytes (dOffset + stride' + 4) 24.0
            _ -> fail "pack-uniform-layout: expected array layout for d"
        _ -> fail "pack-uniform-layout: expected struct layout"
  where
    fieldOffset name fields =
      case find (\fld -> fld.flName == name) fields of
        Just fld -> pure (fromIntegral fld.flOffset)
        Nothing -> fail ("pack-uniform-layout: missing field " <> name)
    fieldLayout name fields =
      case find (\fld -> fld.flName == name) fields of
        Just fld -> pure fld
        Nothing -> fail ("pack-uniform-layout: missing field " <> name)
    assertFloatAt label bytes offset value = do
      let got = word32At bytes offset
      let expected = castFloatToWord32 value
      unless (got == expected) $
        fail (label <> ": expected " <> show expected <> ", got " <> show got)

checkPackUniformErrors :: IO ()
checkPackUniformErrors =
  case compileInline defaultOpts packUniformShader of
    Left err -> fail ("pack-uniform-errors: " <> show err)
    Right (SomeShader shader) -> do
      info <- case find (\b -> b.biName == "payload") (shaderInterface shader).siBindings of
        Nothing -> fail "pack-uniform-errors: missing payload binding"
        Just bi -> pure bi
      let missing =
            PayloadMissingU
              { a = 1.0
              }
      let missingRes = packUniformFrom info.biType missing
      case missingRes of
        Left err ->
          unless ("missing struct field" `isInfixOf` err) $
            fail ("pack-uniform-errors: unexpected missing error: " <> err)
        Right _ -> fail "pack-uniform-errors: expected missing field failure"
      let extra =
            PayloadExtraU
              { a = 1.0
              , b = V3 0.0 0.0 0.0
              , c = M3 (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0)
              , d = [V2 0.0 0.0, V2 0.0 0.0]
              , oops = 0.0
              }
      let extraRes = packUniformFrom info.biType extra
      case extraRes of
        Left err ->
          unless ("unexpected struct fields" `isInfixOf` err) $
            fail ("pack-uniform-errors: unexpected extra error: " <> err)
        Right _ -> fail "pack-uniform-errors: expected extra field failure"

checkPackUniformFrom :: IO ()
checkPackUniformFrom =
  case compileInline defaultOpts packUniformShader of
    Left err -> fail ("pack-uniform-from: " <> show err)
    Right (SomeShader shader) -> do
      info <- case find (\b -> b.biName == "payload") (shaderInterface shader).siBindings of
        Nothing -> fail "pack-uniform-from: missing payload binding"
        Just bi -> pure bi
      let payload =
            PayloadU
              { a = 1.0
              , b = V3 2.0 3.0 4.0
              , c = M3 (V3 10.0 11.0 12.0) (V3 13.0 14.0 15.0) (V3 16.0 17.0 18.0)
              , d = [V2 21.0 22.0, V2 23.0 24.0]
              }
      case packUniformFrom info.biType payload of
        Left err -> fail ("pack-uniform-from: " <> err)
        Right _ -> pure ()

checkUniformStorable :: IO ()
checkUniformStorable = do
  let layout = TLScalar F32 4 4
  case validateUniformStorableUnchecked layout (Proxy @Float) of
    Left err -> fail ("uniform-storable: unexpected failure: " <> err)
    Right () -> pure ()
  case validateUniformStorableUnchecked layout (Proxy @Word64) of
    Left _ -> pure ()
    Right () -> fail "uniform-storable: expected size mismatch for Word64"
  packed <- packUniformStorableUnchecked layout (1.0 :: Float)
  case packed of
    Left err -> fail ("uniform-storable: pack failed: " <> err)
    Right bytes ->
      unless (BS.length bytes == 4) $
        fail "uniform-storable: expected 4 bytes"

checkVertexAttributes :: IO ()
checkVertexAttributes =
  case compileInline defaultOpts vertexShader of
    Left err -> fail ("vertex-attributes: " <> show err)
    Right (SomeShader shader) ->
      case vertexAttributes (shaderInterface shader) of
        Left err -> fail ("vertex-attributes: " <> err)
        Right attrs ->
          case attrs of
            [VertexAttribute _ loc fmt] -> do
              unless (loc == 0) $
                fail "vertex-attributes: expected location 0"
              unless (fmt == VF32x2) $
                fail ("vertex-attributes: expected VF32x2, got " <> show fmt)
            _ -> fail ("vertex-attributes: expected 1 attribute, got " <> show (length attrs))

checkBindingPlan :: IO ()
checkBindingPlan =
  case compileInline separateOpts samplerShader of
    Left err -> fail ("binding-plan: " <> show err)
    Right (SomeShader shader) -> do
      let iface = (shaderInterface shader)
      let plan = (shaderPlan shader)
      unless (length plan.bpBindings == length iface.siBindings) $
        fail "binding-plan: binding count mismatch"
      let group0 = filter (\b -> b.biGroup == 0) plan.bpBindings
      unless (length group0 == 2) $
        fail "binding-plan: expected 2 bindings in group 0"
      let byGroupValues = concat (toList plan.bpByGroup)
      let byGroupBindings = map (.biBinding) byGroupValues
      unless (byGroupBindings == [0, 1]) $
        fail ("binding-plan: expected grouped bindings [0,1], got " <> show byGroupBindings)
      unless (length plan.bpSamplers == 1) $
        fail "binding-plan: expected 1 sampler binding"
      unless (length plan.bpTextures == 1) $
        fail "binding-plan: expected 1 texture binding"

checkInputOrdering :: IO ()
checkInputOrdering =
  case inputsFor orderingShader (Inputs.uniform @"b" (ParamsU (V4 1 2 3 4) :: ParamsU) <> Inputs.uniform @"a" (ParamsU (V4 0 0 0 0) :: ParamsU)) of
    Left err -> fail ("input-ordering: " <> err.ieMessage)
    Right inputs -> do
      let names = map (.uiName) (inputsUniforms inputs)
      unless (names == ["a", "b"]) $
        fail ("input-ordering: expected [\"a\",\"b\"], got " <> show names)

checkInputsCombinedOk :: IO ()
checkInputsCombinedOk =
  case inputsFor combinedInputShader
        (Inputs.uniform @"params" (ParamsU (V4 1 2 3 4) :: ParamsU)
          <> Inputs.sampledTexture @"tex" (TextureHandle 9) (SamplerHandle 3)) of
    Left err -> fail ("inputs-combined-ok: " <> err.ieMessage)
    Right _ -> pure ()

checkInputsMissingBindingsRejected :: IO ()
checkInputsMissingBindingsRejected =
  case inputsFor orderingShader mempty of
    Left err ->
      unless ("missing required bindings:" `isInfixOf` err.ieMessage) $
        fail ("inputs-missing-bindings: unexpected error: " <> err.ieMessage)
    Right _ ->
      fail "inputs-missing-bindings: expected missing required bindings error"

checkInputsSeparateModeRejectsSampledTexture :: IO ()
checkInputsSeparateModeRejectsSampledTexture =
  case compileInline separateOpts samplerShader of
    Left err -> fail ("inputs-separate-sampledtexture: " <> show err)
    Right (SomeShader someShader) ->
      let shader :: Shader
            'SamplerSeparate
            '[ 'Binding "tex" 'BTexture2D 0 0 ('TTexture2D 'SF32)
             , 'Binding "samp" 'BSampler 0 1 'TSampler
             ]
          shader = unsafeCoerce someShader
          invalidSampledTextureBuilder =
            unsafeCoerce
              ( Inputs.sampledTexture @"tex" (TextureHandle 9) (SamplerHandle 3)
                  :: Inputs.InputsBuilder
                      'SamplerCombined
                      '[ 'Binding "tex" 'BTexture2D 0 0 ('TTexture2D 'SF32)
                       , 'Binding "samp" 'BSampler 0 1 'TSampler
                       ]
              )
              :: Inputs.InputsBuilder
                  'SamplerSeparate
                  '[ 'Binding "tex" 'BTexture2D 0 0 ('TTexture2D 'SF32)
                   , 'Binding "samp" 'BSampler 0 1 'TSampler
                   ]
      in case inputsFor
            shader
            invalidSampledTextureBuilder of
          Left err ->
            unless
              ("sampledTexture is not supported in SamplerSeparate mode" `isInfixOf` err.ieMessage)
              (fail ("inputs-separate-sampledtexture: unexpected error: " <> err.ieMessage))
          Right _ ->
            fail "inputs-separate-sampledtexture: expected sampledTexture rejection in SamplerSeparate mode"

checkInputsDuplicateBuilder :: IO ()
checkInputsDuplicateBuilder =
  case inputsFor combinedInputShader
        (Inputs.uniform @"params" (ParamsU (V4 1 2 3 4) :: ParamsU)
          <> Inputs.uniform @"params" (ParamsU (V4 5 6 7 8) :: ParamsU)) of
    Left err ->
      unless ("duplicate binding entry" `isInfixOf` err.ieMessage) $
        fail ("inputs-duplicate: unexpected error: " <> err.ieMessage)
    Right _ ->
      fail "inputs-duplicate: expected duplicate binding error"

checkQuickCheck :: IO ()
checkQuickCheck = do
  runQuickCheckProperty "pack scalar f32" 200 arbitrary shrink checkPackScalarF32
  runQuickCheckProperty "pack vec2<f32>" 200 genV2 shrinkV2 checkPackVec2F32
  runQuickCheckProperty "pack vec3<f32>" 200 genV3 shrinkV3 checkPackVec3F32
  runQuickCheckProperty "pack vec4<f32>" 200 genV4 shrinkV4 checkPackVec4F32
  runQuickCheckProperty "pack mat2x2<f32>" 200 genM2 shrinkM2 checkPackMat2F32
  checkPackUniformExtendedRandom
  checkPackUniformExtended2

runQuickCheckProperty :: Show a => String -> Int -> Gen a -> (a -> [a]) -> (a -> IO ()) -> IO ()
runQuickCheckProperty label successes generator shrinkInput checkInput = do
  result <-
    quickCheckWithResult
      stdArgs { maxSuccess = successes }
      ( forAllShrink generator shrinkInput $ \input ->
          ioProperty $ do
            outcome <- try (checkInput input) :: IO (Either SomeException ())
            pure $
              case outcome of
                Left err ->
                  counterexample
                    (label <> " failed for " <> show input <> ": " <> displayException err)
                    False
                Right () -> counterexample label True
      )
  unless (isSuccess result) $
    fail ("QuickCheck property failed: " <> label)

checkPackScalarF32 :: Float -> IO ()
checkPackScalarF32 value =
  case packUniform (TLScalar F32 4 4) (uniform value) of
    Left err -> fail ("quickcheck: packScalarF32 failed: " <> err)
    Right bytes -> do
      unless (BS.length bytes == 4) $
        fail "quickcheck: packScalarF32 wrong byte size"
      unless (word32At bytes 0 == castFloatToWord32 value) $
        fail "quickcheck: packScalarF32 wrong bits"

checkPackVec2F32 :: V2 Float -> IO ()
checkPackVec2F32 (V2 x y) =
  let align = 8 :: Word32
      size = 8 :: Word32
      layout = TLVector 2 F32 align size
  in case packUniform layout (uniform (V2 x y)) of
      Left err -> fail ("quickcheck: packVec2F32 failed: " <> err)
      Right bytes -> do
        unless (BS.length bytes == fromIntegral size) $
          fail "quickcheck: packVec2F32 wrong byte size"
        unless (word32At bytes 0 == castFloatToWord32 x) $
          fail "quickcheck: packVec2F32 wrong x bits"
        unless (word32At bytes 4 == castFloatToWord32 y) $
          fail "quickcheck: packVec2F32 wrong y bits"

checkPackVec3F32 :: V3 Float -> IO ()
checkPackVec3F32 (V3 x y z) =
  let align = 16 :: Word32
      size = 12 :: Word32
      layout = TLVector 3 F32 align size
  in case packUniform layout (uniform (V3 x y z)) of
      Left err -> fail ("quickcheck: packVec3F32 failed: " <> err)
      Right bytes -> do
        unless (BS.length bytes == fromIntegral size) $
          fail "quickcheck: packVec3F32 wrong byte size"
        unless (word32At bytes 0 == castFloatToWord32 x) $
          fail "quickcheck: packVec3F32 wrong x bits"
        unless (word32At bytes 4 == castFloatToWord32 y) $
          fail "quickcheck: packVec3F32 wrong y bits"
        unless (word32At bytes 8 == castFloatToWord32 z) $
          fail "quickcheck: packVec3F32 wrong z bits"

checkPackVec4F32 :: V4 Float -> IO ()
checkPackVec4F32 (V4 x y z w) =
  let align = 16 :: Word32
      size = 16 :: Word32
      layout = TLVector 4 F32 align size
  in case packUniform layout (uniform (V4 x y z w)) of
      Left err -> fail ("quickcheck: packVec4F32 failed: " <> err)
      Right bytes -> do
        unless (BS.length bytes == fromIntegral size) $
          fail "quickcheck: packVec4F32 wrong byte size"
        unless (word32At bytes 0 == castFloatToWord32 x) $
          fail "quickcheck: packVec4F32 wrong x bits"
        unless (word32At bytes 4 == castFloatToWord32 y) $
          fail "quickcheck: packVec4F32 wrong y bits"
        unless (word32At bytes 8 == castFloatToWord32 z) $
          fail "quickcheck: packVec4F32 wrong z bits"
        unless (word32At bytes 12 == castFloatToWord32 w) $
          fail "quickcheck: packVec4F32 wrong w bits"

checkPackMat2F32 :: M2 Float -> IO ()
checkPackMat2F32 (M2 (V2 a b) (V2 c d)) =
  let align = 8 :: Word32
      stride = 8 :: Word32
      size = 16 :: Word32
      layout = TLMatrix 2 2 F32 align size stride
  in case packUniform layout (uniform (M2 (V2 a b) (V2 c d))) of
      Left err -> fail ("quickcheck: packMat2F32 failed: " <> err)
      Right bytes -> do
        unless (BS.length bytes == fromIntegral size) $
          fail "quickcheck: packMat2F32 wrong byte size"
        unless (word32At bytes 0 == castFloatToWord32 a) $
          fail "quickcheck: packMat2F32 wrong a bits"
        unless (word32At bytes 4 == castFloatToWord32 b) $
          fail "quickcheck: packMat2F32 wrong b bits"
        unless (word32At bytes 8 == castFloatToWord32 c) $
          fail "quickcheck: packMat2F32 wrong c bits"
        unless (word32At bytes 12 == castFloatToWord32 d) $
          fail "quickcheck: packMat2F32 wrong d bits"

genV4 :: Gen (V4 Float)
genV4 = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

shrinkV4 :: V4 Float -> [V4 Float]
shrinkV4 (V4 a b c d) =
  [V4 a' b c d | a' <- shrink a]
    <> [V4 a b' c d | b' <- shrink b]
    <> [V4 a b c' d | c' <- shrink c]
    <> [V4 a b c d' | d' <- shrink d]

genV2 :: Gen (V2 Float)
genV2 = V2 <$> arbitrary <*> arbitrary

shrinkV2 :: V2 Float -> [V2 Float]
shrinkV2 (V2 a b) =
  [V2 a' b | a' <- shrink a]
    <> [V2 a b' | b' <- shrink b]

genV3 :: Gen (V3 Float)
genV3 = V3 <$> arbitrary <*> arbitrary <*> arbitrary

shrinkV3 :: V3 Float -> [V3 Float]
shrinkV3 (V3 a b c) =
  [V3 a' b c | a' <- shrink a]
    <> [V3 a b' c | b' <- shrink b]
    <> [V3 a b c' | c' <- shrink c]

genM2 :: Gen (M2 Float)
genM2 = M2 <$> genV2 <*> genV2

shrinkM2 :: M2 Float -> [M2 Float]
shrinkM2 (M2 a b) =
  [M2 a' b | a' <- shrinkV2 a]
    <> [M2 a b' | b' <- shrinkV2 b]

genM3 :: Gen (M3 Float)
genM3 = M3 <$> genV3 <*> genV3 <*> genV3

shrinkM3 :: M3 Float -> [M3 Float]
shrinkM3 (M3 a b c) =
  [M3 a' b c | a' <- shrinkV3 a]
    <> [M3 a b' c | b' <- shrinkV3 b]
    <> [M3 a b c' | c' <- shrinkV3 c]

genM4 :: Gen (M4 Float)
genM4 = M4 <$> genV4 <*> genV4 <*> genV4 <*> genV4

shrinkM4 :: M4 Float -> [M4 Float]
shrinkM4 (M4 a b c d) =
  [M4 a' b c d | a' <- shrinkV4 a]
    <> [M4 a b' c d | b' <- shrinkV4 b]
    <> [M4 a b c' d | c' <- shrinkV4 c]
    <> [M4 a b c d' | d' <- shrinkV4 d]

genInner :: Gen (V2 Float, Float)
genInner = (,) <$> genV2 <*> arbitrary

shrinkInner :: (V2 Float, Float) -> [(V2 Float, Float)]
shrinkInner (vec, scalar) =
  [(vec', scalar) | vec' <- shrinkV2 vec]
    <> [(vec, scalar') | scalar' <- shrink scalar]

shrinkElements :: (a -> [a]) -> [a] -> [[a]]
shrinkElements shrinkElement values =
  [ before <> [value'] <> after
  | (before, value : after) <- zip (inits values) (tails values)
  , value' <- shrinkElement value
  ]

genExtendedUniform :: Gen (M3 Float, M4 Float, [V2 Float], [V3 Float], (V2 Float, Float))
genExtendedUniform =
  (,,,,)
    <$> genM3
    <*> genM4
    <*> sequence [genV2, genV2, genV2, genV2]
    <*> sequence [genV3, genV3, genV3]
    <*> genInner

shrinkExtendedUniform :: (M3 Float, M4 Float, [V2 Float], [V3 Float], (V2 Float, Float)) -> [(M3 Float, M4 Float, [V2 Float], [V3 Float], (V2 Float, Float))]
shrinkExtendedUniform (matrix3, matrix4, vectors2, vectors3, innerValue) =
  [(matrix3', matrix4, vectors2, vectors3, innerValue) | matrix3' <- shrinkM3 matrix3]
    <> [(matrix3, matrix4', vectors2, vectors3, innerValue) | matrix4' <- shrinkM4 matrix4]
    <> [(matrix3, matrix4, vectors2', vectors3, innerValue) | vectors2' <- shrinkElements shrinkV2 vectors2]
    <> [(matrix3, matrix4, vectors2, vectors3', innerValue) | vectors3' <- shrinkElements shrinkV3 vectors3]
    <> [(matrix3, matrix4, vectors2, vectors3, innerValue') | innerValue' <- shrinkInner innerValue]

genExtendedUniform2 :: Gen ([Float], [Float], [M2 Float], [(V3 Float, Float)], (Word16, Word16, Word16, Word16, Word16, Word16))
genExtendedUniform2 =
  (,,,,)
    <$> sequence (replicate 12 arbitrary)
    <*> sequence (replicate 12 arbitrary)
    <*> sequence [genM2, genM2]
    <*> sequence [genV3 >>= \vec -> (,) vec <$> arbitrary, genV3 >>= \vec -> (,) vec <$> arbitrary]
    <*> ((,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

shrinkExtendedUniform2 :: ([Float], [Float], [M2 Float], [(V3 Float, Float)], (Word16, Word16, Word16, Word16, Word16, Word16)) -> [([Float], [Float], [M2 Float], [(V3 Float, Float)], (Word16, Word16, Word16, Word16, Word16, Word16))]
shrinkExtendedUniform2 (matrix34, matrix43, matrices, inners, halves) =
  [(matrix34', matrix43, matrices, inners, halves) | matrix34' <- shrinkElements shrink matrix34]
    <> [(matrix34, matrix43', matrices, inners, halves) | matrix43' <- shrinkElements shrink matrix43]
    <> [(matrix34, matrix43, matrices', inners, halves) | matrices' <- shrinkElements shrinkM2 matrices]
    <> [(matrix34, matrix43, matrices, inners', halves) | inners' <- shrinkElements (\(vec, scalar) -> [(vec', scalar) | vec' <- shrinkV3 vec] <> [(vec, scalar') | scalar' <- shrink scalar]) inners]
    <> [(matrix34, matrix43, matrices, inners, halves') | halves' <- shrinkHalves halves]
  where
    shrinkHalves (a, b, c, d, e, f) =
      [(a', b, c, d, e, f) | a' <- shrink a]
        <> [(a, b', c, d, e, f) | b' <- shrink b]
        <> [(a, b, c', d, e, f) | c' <- shrink c]
        <> [(a, b, c, d', e, f) | d' <- shrink d]
        <> [(a, b, c, d, e', f) | e' <- shrink e]
        <> [(a, b, c, d, e, f') | f' <- shrink f]

checkPackUniformExtendedRandom :: IO ()
checkPackUniformExtendedRandom =
  case compileInline defaultOpts packUniformExtendedShader of
    Left err -> fail ("quickcheck: pack-uniform-extended: " <> show err)
    Right (SomeShader shader) -> do
      info <- case find (\b -> b.biName == "params") (shaderInterface shader).siBindings of
        Nothing -> fail "quickcheck: pack-uniform-extended: missing params binding"
        Just bi -> pure bi
      case info.biType of
        TLStruct _ fields _ size -> do
          fieldM3 <- findFieldLayout "m3" fields
          fieldM4 <- findFieldLayout "m4" fields
          fieldArr2 <- findFieldLayout "arr2" fields
          fieldArr3 <- findFieldLayout "arr3" fields
          fieldInner <- findFieldLayout "inner" fields
          runQuickCheckProperty "pack extended uniform" 80 genExtendedUniform shrinkExtendedUniform $ \(m3, m4, arr2, arr3, (innerA, innerB)) -> do
            let value =
                  ParamsExtendedU
                    { m3 = m3
                    , m4 = m4
                    , arr2 = arr2
                    , arr3 = arr3
                    , inner = InnerU innerA innerB
                    }
            bytes <-
              case packUniformFrom info.biType value of
                Left err -> fail ("quickcheck: pack-uniform-extended: " <> err)
                Right bs -> pure bs
            unless (BS.length bytes == fromIntegral size) $
              fail "quickcheck: pack-uniform-extended size mismatch"
            assertMatrix3 "m3" bytes fieldM3 m3
            assertMatrix4 "m4" bytes fieldM4 m4
            assertArrayVec2 "arr2" bytes fieldArr2 arr2
            assertArrayVec3 "arr3" bytes fieldArr3 arr3
            assertInner "inner" bytes fieldInner innerA innerB
        _ -> fail "quickcheck: pack-uniform-extended: expected struct layout"
  where
    findFieldLayout name fields =
      case find (\fld -> fld.flName == name) fields of
        Just fld -> pure fld
        Nothing -> fail ("quickcheck: pack-uniform-extended missing field " <> name)

    assertMatrix3 label bytes field mat =
      case field.flType of
        TLMatrix cols rows _ _ _ stride -> do
          let base = fromIntegral field.flOffset
          case mat of
            M3 c0 c1 c2 | cols == 3 && rows == 3 -> do
              assertVec3At (label <> ":c0") bytes base stride c0
              assertVec3At (label <> ":c1") bytes (base + fromIntegral stride) stride c1
              assertVec3At (label <> ":c2") bytes (base + fromIntegral stride * 2) stride c2
            _ -> fail ("quickcheck: pack-uniform-extended: unexpected matrix shape for " <> label)
        _ -> fail ("quickcheck: pack-uniform-extended: expected matrix for " <> label)

    assertMatrix4 label bytes field mat =
      case field.flType of
        TLMatrix cols rows _ _ _ stride -> do
          let base = fromIntegral field.flOffset
          case mat of
            M4 c0 c1 c2 c3 | cols == 4 && rows == 4 -> do
              assertVec4At (label <> ":c0") bytes base c0
              assertVec4At (label <> ":c1") bytes (base + fromIntegral stride) c1
              assertVec4At (label <> ":c2") bytes (base + fromIntegral stride * 2) c2
              assertVec4At (label <> ":c3") bytes (base + fromIntegral stride * 3) c3
            _ -> fail ("quickcheck: pack-uniform-extended: unexpected matrix shape for " <> label)
        _ -> fail ("quickcheck: pack-uniform-extended: expected matrix for " <> label)

    assertArrayVec2 label bytes field values =
      case field.flType of
        TLArray (Just len) stride _ _ _ -> do
          unless (len == length values) $
            fail ("quickcheck: pack-uniform-extended: " <> label <> " length mismatch")
          let base = fromIntegral field.flOffset
          forM_ (zip [0 ..] values) $ \(ix, v) ->
            assertVec2At (label <> ":" <> show ix) bytes (base + fromIntegral stride * ix) v
        _ -> fail ("quickcheck: pack-uniform-extended: expected array for " <> label)

    assertArrayVec3 label bytes field values =
      case field.flType of
        TLArray (Just len) stride _ _ _ -> do
          unless (len == length values) $
            fail ("quickcheck: pack-uniform-extended: " <> label <> " length mismatch")
          let base = fromIntegral field.flOffset
          forM_ (zip [0 ..] values) $ \(ix, v) ->
            assertVec3At (label <> ":" <> show ix) bytes (base + fromIntegral stride * ix) stride v
        _ -> fail ("quickcheck: pack-uniform-extended: expected array for " <> label)

    assertInner label bytes field innerA innerB =
      case field.flType of
        TLStruct _ innerFields _ size -> do
          fieldA <- findFieldLayout "a" innerFields
          fieldB <- findFieldLayout "b" innerFields
          let base = fromIntegral field.flOffset
          let offA = base + fromIntegral fieldA.flOffset
          let offB = base + fromIntegral fieldB.flOffset
          assertVec2At (label <> ".a") bytes offA innerA
          assertScalarAt (label <> ".b") bytes offB innerB
          let used = fromIntegral fieldB.flOffset + 4
          let padBytes = [BS.index bytes (base + used + i) | size > fromIntegral used, i <- [0 .. fromIntegral size - used - 1]]
          unless (all (== 0) padBytes) $
            fail ("quickcheck: pack-uniform-extended: " <> label <> " padding not zeroed")
        _ -> fail ("quickcheck: pack-uniform-extended: expected struct for " <> label)

    assertScalarAt label bytes offset value =
      unless (word32At bytes offset == castFloatToWord32 value) $
        fail ("quickcheck: pack-uniform-extended: " <> label <> " wrong bits")

    assertVec2At label bytes offset (V2 x y) = do
      assertScalarAt (label <> ".x") bytes offset x
      assertScalarAt (label <> ".y") bytes (offset + 4) y

    assertVec3At label bytes offset stride (V3 x y z) = do
      assertScalarAt (label <> ".x") bytes offset x
      assertScalarAt (label <> ".y") bytes (offset + 4) y
      assertScalarAt (label <> ".z") bytes (offset + 8) z
      let paddingBytes = [BS.index bytes (offset + 12 + i) | stride > 12, i <- [0 .. fromIntegral stride - 12 - 1]]
      unless (all (== 0) paddingBytes) $
        fail ("quickcheck: pack-uniform-extended: " <> label <> " padding not zeroed")

    assertVec4At label bytes offset (V4 x y z w) = do
      assertScalarAt (label <> ".x") bytes offset x
      assertScalarAt (label <> ".y") bytes (offset + 4) y
      assertScalarAt (label <> ".z") bytes (offset + 8) z
      assertScalarAt (label <> ".w") bytes (offset + 12) w

checkPackUniformExtended2 :: IO ()
checkPackUniformExtended2 =
  case compileInline defaultOpts packUniformExtendedShader2 of
    Left err -> fail ("quickcheck: pack-uniform-extended2: " <> show err)
    Right (SomeShader shader) -> do
      info <- case find (\b -> b.biName == "params2") (shaderInterface shader).siBindings of
        Nothing -> fail "quickcheck: pack-uniform-extended2: missing params2 binding"
        Just bi -> pure bi
      case info.biType of
        TLStruct _ fields _ size -> do
          fieldM34 <- findFieldLayout "m34" fields
          fieldM43 <- findFieldLayout "m43" fields
          fieldMats <- findFieldLayout "mats" fields
          fieldNested <- findFieldLayout "nested" fields
          fieldH <- findFieldLayout "h" fields
          fieldHv <- findFieldLayout "hv" fields
          fieldHv4 <- findFieldLayout "hv4" fields
          runQuickCheckProperty "pack nested f16 uniform" 60 genExtendedUniform2 shrinkExtendedUniform2 $ \(m34Vals, m43Vals, mats, inners, (h0, h1, h2, h3, h4, h5)) -> do
            m34 <-
              maybe
                (fail "quickcheck: pack-uniform-extended2: expected 12 mat3x4 values")
                pure
                (m3x4FromList m34Vals)
            m43 <-
              maybe
                (fail "quickcheck: pack-uniform-extended2: expected 12 mat4x3 values")
                pure
                (m4x3FromList m43Vals)
            let innerValues = [Inner2U v0 w0 | (v0, w0) <- inners]
            let value =
                  ParamsExtended2U
                    { m34 = m34
                    , m43 = m43
                    , mats = mats
                    , nested = Outer2U innerValues
                    , h = Half h0
                    , hv = V3 (Half h1) (Half h2) (Half h3)
                    , hv4 = V4 (Half h2) (Half h3) (Half h4) (Half h5)
                    }
            bytes <-
              case packUniformFrom info.biType value of
                Left err -> fail ("quickcheck: pack-uniform-extended2: " <> err)
                Right bs -> pure bs
            unless (BS.length bytes == fromIntegral size) $
              fail "quickcheck: pack-uniform-extended2 size mismatch"
            assertMatrixF32 "m34" bytes fieldM34 3 4 m34Vals
            assertMatrixF32 "m43" bytes fieldM43 4 3 m43Vals
            assertArrayMat2 "mats" bytes fieldMats mats
            assertNestedArray "nested" bytes fieldNested inners
            assertHalfScalar "h" bytes fieldH h0
            assertHalfVector "hv" bytes fieldHv [h1, h2, h3]
            assertHalfVector "hv4" bytes fieldHv4 [h2, h3, h4, h5]
        _ -> fail "quickcheck: pack-uniform-extended2: expected struct layout"
  where
    findFieldLayout name fields =
      case find (\fld -> fld.flName == name) fields of
        Just fld -> pure fld
        Nothing -> fail ("quickcheck: pack-uniform-extended2 missing field " <> name)

    m3x4FromList vals =
      case vals of
        [a,b,c,d,e,f,g,h,i,j,k,l] ->
          Just (M3x4 (V4 a b c d) (V4 e f g h) (V4 i j k l))
        _ -> Nothing

    m4x3FromList vals =
      case vals of
        [a,b,c,d,e,f,g,h,i,j,k,l] ->
          Just (M4x3 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l))
        _ -> Nothing

    assertMatrixF32 label bytes field cols rows vals =
      case field.flType of
        TLMatrix c r _ _ _ stride
          | c == cols && r == rows -> do
              let base = fromIntegral field.flOffset
              unless (length vals == cols * rows) $
                fail ("quickcheck: pack-uniform-extended2: " <> label <> " value count mismatch")
              forM_ (zip [0 ..] vals) $ \(ix, v) -> do
                let col = ix `div` rows
                let row = ix `mod` rows
                let off = base + fromIntegral stride * col + row * 4
                unless (word32At bytes off == castFloatToWord32 v) $
                  fail ("quickcheck: pack-uniform-extended2: " <> label <> " wrong bits at " <> show ix)
          | otherwise -> fail ("quickcheck: pack-uniform-extended2: " <> label <> " unexpected matrix shape")
        _ -> fail ("quickcheck: pack-uniform-extended2: " <> label <> " expected matrix")

    assertArrayMat2 label bytes field mats =
      case field.flType of
        TLArray (Just len) stride elemLayout _ _ -> do
          unless (len == length mats) $
            fail ("quickcheck: pack-uniform-extended2: " <> label <> " length mismatch")
          let base = fromIntegral field.flOffset
          forM_ (zip [0 ..] mats) $ \(ix, mat) ->
            case elemLayout of
              TLMatrix 2 2 _ _ _ elemStride -> do
                let off = base + fromIntegral stride * ix
                case mat of
                  M2 (V2 a b) (V2 c d) -> do
                    unless (word32At bytes off == castFloatToWord32 a) $
                      fail ("quickcheck: pack-uniform-extended2: " <> label <> " m2 a")
                    unless (word32At bytes (off + 4) == castFloatToWord32 b) $
                      fail ("quickcheck: pack-uniform-extended2: " <> label <> " m2 b")
                    unless (word32At bytes (off + fromIntegral elemStride) == castFloatToWord32 c) $
                      fail ("quickcheck: pack-uniform-extended2: " <> label <> " m2 c")
                    unless (word32At bytes (off + fromIntegral elemStride + 4) == castFloatToWord32 d) $
                      fail ("quickcheck: pack-uniform-extended2: " <> label <> " m2 d")
              _ -> fail ("quickcheck: pack-uniform-extended2: " <> label <> " expected mat2 element")
        _ -> fail ("quickcheck: pack-uniform-extended2: " <> label <> " expected array")

    assertNestedArray label bytes field inners =
      case field.flType of
        TLStruct _ nestedFields _ _ -> do
          innerField <- findFieldLayout "inners" nestedFields
          case innerField.flType of
            TLArray (Just len) stride elemLayout _ _ -> do
              unless (len == length inners) $
                fail ("quickcheck: pack-uniform-extended2: " <> label <> " inner length mismatch")
              let base = fromIntegral field.flOffset + fromIntegral innerField.flOffset
              forM_ (zip [0 ..] inners) $ \(ix, (v0, w0)) ->
                case elemLayout of
                  TLStruct _ innerFields _ innerSize -> do
                    let off = base + fromIntegral stride * ix
                    fieldV <- findFieldLayout "v" innerFields
                    fieldW <- findFieldLayout "w" innerFields
                    let offV = off + fromIntegral fieldV.flOffset
                    let offW = off + fromIntegral fieldW.flOffset
                    let vStride = case fieldV.flType of
                          TLVector _ _ _ s -> fromIntegral s
                          _ -> 16
                    assertVec3At (label <> ".inners[" <> show ix <> "].v") bytes offV vStride v0
                    assertScalarAt (label <> ".inners[" <> show ix <> "].w") bytes offW w0
                    let used = maximum (map (\fld -> fromIntegral (fld.flOffset + fld.flSize)) innerFields)
                    let paddingBytes =
                          [BS.index bytes (off + used + j) | innerSize > fromIntegral used, j <- [0 .. fromIntegral innerSize - used - 1]]
                    unless (all (== 0) paddingBytes) $
                      fail ("quickcheck: pack-uniform-extended2: " <> label <> " inner padding not zeroed")
                  _ -> fail ("quickcheck: pack-uniform-extended2: " <> label <> " expected inner struct")
            _ -> fail ("quickcheck: pack-uniform-extended2: " <> label <> " expected inner array")
        _ -> fail ("quickcheck: pack-uniform-extended2: " <> label <> " expected struct")

    assertScalarAt label bytes offset value =
      unless (word32At bytes offset == castFloatToWord32 value) $
        fail ("quickcheck: pack-uniform-extended2: " <> label <> " wrong bits")

    assertVec3At label bytes offset stride (V3 x y z) = do
      assertScalarAt (label <> ".x") bytes offset x
      assertScalarAt (label <> ".y") bytes (offset + 4) y
      assertScalarAt (label <> ".z") bytes (offset + 8) z
      let paddingBytes = [BS.index bytes (offset + 12 + i) | stride > 12, i <- [0 .. stride - 12 - 1]]
      unless (all (== 0) paddingBytes) $
        fail ("quickcheck: pack-uniform-extended2: " <> label <> " padding not zeroed")

    assertHalfScalar label bytes field val =
      case field.flType of
        TLScalar F16 _ _ -> do
          let off = fromIntegral field.flOffset
          unless (word16At bytes off == val) $
            fail ("quickcheck: pack-uniform-extended2: " <> label <> " half bits mismatch")
        _ -> fail ("quickcheck: pack-uniform-extended2: " <> label <> " expected f16 scalar")

    assertHalfVector label bytes field vals =
      case field.flType of
        TLVector n F16 _ size -> do
          let off = fromIntegral field.flOffset
          unless (n == length vals) $
            fail ("quickcheck: pack-uniform-extended2: " <> label <> " length mismatch")
          forM_ (zip [0 ..] vals) $ \(ix, v) -> do
            unless (word16At bytes (off + ix * 2) == v) $
              fail ("quickcheck: pack-uniform-extended2: " <> label <> " half bits mismatch")
          let padding = fromIntegral size - n * 2
          let paddingBytes = [BS.index bytes (off + n * 2 + i) | padding > 0, i <- [0 .. padding - 1]]
          unless (all (== 0) paddingBytes) $
            fail ("quickcheck: pack-uniform-extended2: " <> label <> " padding not zeroed")
        _ -> fail ("quickcheck: pack-uniform-extended2: " <> label <> " expected f16 vector")

checkDuplicateBindings :: IO ()
checkDuplicateBindings =
  case compileInline defaultOpts duplicateBindingShader of
    Left (CompileError msg _ _) ->
      unless ("duplicate binding" `isInfixOf` msg) $
        fail ("duplicate-bindings: unexpected error: " <> msg)
    Right _ -> fail "duplicate-bindings: expected failure"

orderingShader :: Shader 'SamplerCombined
  '[ 'Binding "b" 'BUniform 0 1 ('TStruct '[ 'Field "v" ('TVec 4 'SF32)])
   , 'Binding "a" 'BUniform 0 0 ('TStruct '[ 'Field "v" ('TVec 4 'SF32)])
   ]
orderingShader = $(spirv defaultCompileOptions imports [wesl|
struct Params { v: vec4<f32>; };

@group(0) @binding(1) var<uniform> b: Params;
@group(0) @binding(0) var<uniform> a: Params;

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  return a.v + b.v;
}
|])

duplicateBindingShader :: String
duplicateBindingShader =
  unlines
    [ "struct Params { v: vec4<f32>; };"
    , "@group(0) @binding(0) var<uniform> a: Params;"
    , "@group(0) @binding(1) var<uniform> a: Params;"
    , ""
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  return vec4(0.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

checkGoldenSpirv :: IO ()
checkGoldenSpirv = do
  update <- fmap (maybe False isTruthy) (lookupEnv "SPIRDO_UPDATE_GOLDEN")
  let dir = "test" </> "golden"
  let fixtures =
        [ ("compute-basic", goldenComputeShader)
        , ("fragment-basic", goldenFragmentShader)
        ]
  whenUpdate update (createDirectoryIfMissing True dir)
  forM_ fixtures $ \(label, src) -> do
    bytes <- case compileBytes defaultOpts src of
      Left err -> fail ("golden:" <> label <> ": " <> show err)
      Right bs -> pure bs
    let path = dir </> (label <> ".spv.golden")
    if update
      then BS.writeFile path bytes
      else do
        exists <- doesFileExist path
        unless exists $
          fail ("golden: missing " <> path <> " (set SPIRDO_UPDATE_GOLDEN=1 to generate)")
        expected <- BS.readFile path
        unless (expected == bytes) $
          fail ("golden: mismatch for " <> label)
  where
    whenUpdate True act = act
    whenUpdate False _ = pure ()

checkIfTranslation :: IO ()
checkIfTranslation = do
  let opts = [OptEnableFeature "FOO"]
  case compileBytes opts ifShader of
    Left err -> fail ("if-translation: " <> show err)
    Right _ -> pure ()
  case compileBytes defaultOpts ifShader of
    Left _ -> pure ()
    Right _ -> fail "if-translation: expected failure without FOO feature"

checkMalformedHexLiteral :: IO ()
checkMalformedHexLiteral = do
  case compileInline defaultOpts malformedHexLiteralShader of
    Left _ -> pure ()
    Right _ -> fail "malformed-hex-literal: expected parse failure for 0x/0x__"

checkStructFieldSeparators :: IO ()
checkStructFieldSeparators =
  case compileInline defaultOpts badStructFieldShader of
    Left _ -> pure ()
    Right _ -> fail "struct-field-separator: expected failure for adjacent fields without delimiter"

checkNonIfStatementAttrsRejected :: IO ()
checkNonIfStatementAttrsRejected =
  case compileInline defaultOpts nonIfStatementAttrShader of
    Left _ -> pure ()
    Right _ -> fail "statement-attrs: expected failure for non-@if statement attributes"

checkNonIfSwitchCaseAttrsRejected :: IO ()
checkNonIfSwitchCaseAttrsRejected =
  case compileInline defaultOpts nonIfSwitchCaseAttrShader of
    Left _ -> pure ()
    Right _ -> fail "switch-case-attrs: expected failure for non-@if switch-case attributes"

checkNonIfLoopAttrsRejected :: IO ()
checkNonIfLoopAttrsRejected =
  case compileInline defaultOpts nonIfLoopAttrShader of
    Left _ -> pure ()
    Right _ -> fail "loop-attrs: expected failure for non-@if loop attributes"

checkModuleConstDecl :: IO ()
checkModuleConstDecl =
  case compileBytes defaultOpts moduleConstDeclShader of
    Left err -> fail ("module-const-decl: " <> show err)
    Right _ -> pure ()

checkInvalidMatrixDimensionsRejected :: IO ()
checkInvalidMatrixDimensionsRejected =
  case compileInline defaultOpts invalidMatrixDimensionsShader of
    Left _ -> pure ()
    Right _ -> fail "invalid-matrix-dimensions: expected failure for mat5x5"

checkNegativeI32Range :: IO ()
checkNegativeI32Range =
  case compileBytes defaultOpts negativeI32RangeShader of
    Left err -> fail ("negative-i32-range: " <> show err)
    Right _ -> pure ()

checkComputeRequiresWorkgroupSize :: IO ()
checkComputeRequiresWorkgroupSize =
  case compileInline defaultOpts computeWithoutWorkgroupSizeShader of
    Left err ->
      unless ("@workgroup_size is required for @compute" `isInfixOf` err.ceMessage) $
        fail ("compute-workgroup-size-missing: unexpected error: " <> show err)
    Right _ -> fail "compute-workgroup-size-missing: expected failure for @compute without @workgroup_size"

checkSuperImportContainment :: IO ()
checkSuperImportContainment =
  bracket createRootDirectory removePathForcibly $ \rootDir -> do
    let libFile = rootDir </> "lib.wesl"
        rootFile = rootDir </> "main.wesl"
        validSource =
          unlines
            [ "import super::lib;"
            , "@fragment"
            , "fn main() -> @location(0) vec4<f32> {"
            , "  return vec4(0.0);"
            , "}"
            ]
        invalidSource =
          unlines
            [ "import super::super::lib;"
            , "@fragment"
            , "fn main() -> @location(0) vec4<f32> {"
            , "  return vec4(0.0);"
            , "}"
            ]
        libSource = "fn lib() -> f32 { return 1.0; }"

    writeFile libFile libSource
    writeFile rootFile validSource
    resultOk <- compileFile rootFile
    case resultOk of
      Left err -> fail ("super-containment: valid single super import failed: " <> show err)
      Right _ -> pure ()
    writeFile rootFile invalidSource
    resultBad <- compileFile rootFile
    case resultBad of
      Left err ->
        unless ("import path escapes package root" `isInfixOf` err.ceMessage) $
          fail ("super-containment: expected path escape message, got: " <> show err)
      Right _ -> fail "super-containment: expected rejected super import path that escapes root"
  where
    createRootDirectory = do
      temporaryDirectory <- getTemporaryDirectory
      bracketOnError
        (openTempFile temporaryDirectory "spirdo-super-containment")
        cleanupTemporaryFile
        $ \(path, handle) -> do
            hClose handle
            removeFile path
            createDirectoryIfMissing True path
            pure path

checkWorkgroupSizeOverrideReject :: IO ()
checkWorkgroupSizeOverrideReject =
  case compileInline defaultOpts workgroupOverrideShader of
    Left err ->
      fail ("workgroup-size-overrides: expected success for override-dependent workgroup_size, got " <> show err)
    Right _ -> pure ()

checkImportCompile :: Maybe FilePath -> IO ()
checkImportCompile spirvVal = do
  let path = "test" </> "fixtures" </> "main.wesl"
  result <- compileFile path
  case result of
    Left err -> fail ("import-compile: " <> show err)
    Right (SomeShader shader) -> assertSpirv spirvVal "import-compile" (shaderSpirv shader)

checkImportItemCompile :: Maybe FilePath -> IO ()
checkImportItemCompile spirvVal = do
  let path = "test" </> "fixtures" </> "main_item.wesl"
  result <- compileFile path
  case result of
    Left err -> fail ("import-item: " <> show err)
    Right (SomeShader shader) -> assertSpirv spirvVal "import-item" (shaderSpirv shader)

checkImportAliasCompile :: Maybe FilePath -> IO ()
checkImportAliasCompile spirvVal = do
  let path = "test" </> "fixtures" </> "main_alias.wesl"
  result <- compileFile path
  case result of
    Left err -> fail ("import-alias: " <> show err)
    Right (SomeShader shader) -> assertSpirv spirvVal "import-alias" (shaderSpirv shader)

checkImportStructZeroCtorCompile :: Maybe FilePath -> IO ()
checkImportStructZeroCtorCompile spirvVal = do
  let path = "test" </> "fixtures" </> "import_struct_main.wesl"
  result <- compileFile path
  case result of
    Left err -> fail ("import-struct-zero-ctor: " <> show err)
    Right (SomeShader shader) -> assertSpirv spirvVal "import-struct-zero-ctor" (shaderSpirv shader)

checkImportQualifiedConstCompile :: Maybe FilePath -> IO ()
checkImportQualifiedConstCompile spirvVal = do
  let path = "test" </> "fixtures" </> "import_const_main.wesl"
  result <- compileFileWith [OptEnableFeature "uniform_buffer_standard_layout"] path
  case result of
    Left err -> fail ("import-qualified-const: " <> show err)
    Right (SomeShader shader) -> assertSpirv spirvVal "import-qualified-const" (shaderSpirv shader)

loadParityManifest :: FilePath -> IO [ParityCase]
loadParityManifest manifestPath = do
  exists <- doesFileExist manifestPath
  unless exists $
    fail ("parity-manifest: missing " <> manifestPath)
  raw <- readFile manifestPath
  let rows =
        [ (lineNo, line0)
        | (lineNo, line0) <- zip [1 :: Int ..] (lines raw)
        , let view = trim line0
        , not (null view)
        , not ("#" `isPrefixOf` view)
        ]
  case rows of
    [] -> fail ("parity-manifest: no rows in " <> manifestPath)
    ((headerLineNo, header) : bodyRows) -> do
      let expectedHeader =
            "id\tdomain\tspec_ref\tkind\texpected\tsource\torigin\torigin_ref\terror_contains\toracles\toptions\towner\texit_criteria"
      unless (map toLower (trim header) == map toLower expectedHeader) $
        fail
          ( "parity-manifest:"
              <> manifestPath
              <> ":"
              <> show headerLineNo
              <> ": expected header "
              <> show expectedHeader
          )
      mapM (parseRow (takeDirectory manifestPath)) bodyRows
  where
    parseRow manifestDir (lineNo, line) =
      case splitBy '\t' line of
        [caseId, caseDomain, caseSpecRef, caseKind, caseExpectedRaw, sourceRaw, originRaw, originRefRaw, errContainsRaw, oraclesRaw, optionsRaw, ownerRaw, exitRaw] -> do
          caseExpected <- parseExpected lineNo caseExpectedRaw
          caseSource <- parseSource manifestDir lineNo sourceRaw
          let caseKind' = map toLower (trim caseKind)
          caseOrigin <- parseOrigin lineNo originRaw
          let caseOriginRef = trim originRefRaw
          when (null caseOriginRef) $
            fail
              ( "parity-manifest:"
                  <> manifestPath
                  <> ":"
                  <> show lineNo
                  <> ": origin_ref must not be empty"
              )
          when (caseOrigin == "cts" && caseKind' `notElem` ["backlog", "backlog-unmapped"] && isInlineSource caseSource) $
            fail
              ( "parity-manifest:"
                  <> manifestPath
                  <> ":"
                  <> show lineNo
                  <> ": cts origin rows must use file-backed sources"
              )
          caseOptions <- parseOptions lineNo optionsRaw
          let caseOracles = map (map toLower . trim) (filter (not . null . trim) (splitBy ',' oraclesRaw))
          let unknownOracles = filter (`notElem` ["spirv-val", "naga-pass", "naga-fail"]) caseOracles
          unless (null unknownOracles) $
            fail
              ( "parity-manifest:"
                  <> manifestPath
                  <> ":"
                  <> show lineNo
                  <> ": unknown oracle(s): "
                  <> show unknownOracles
              )
          let caseOwner = toMaybeField ownerRaw
          let caseExit = toMaybeField exitRaw
          when (caseExpected == ParityXFail && isNothing caseOwner) $
            fail
              ( "parity-manifest:"
                  <> manifestPath
                  <> ":"
                  <> show lineNo
                  <> ": xfail row requires owner"
              )
          when (caseExpected == ParityXFail && isNothing caseExit) $
            fail
              ( "parity-manifest:"
                  <> manifestPath
                  <> ":"
                  <> show lineNo
                  <> ": xfail row requires exit_criteria"
              )
          when (caseKind' `elem` ["backlog", "backlog-unmapped"] && caseExpected /= ParityXFail) $
            fail
              ( "parity-manifest:"
                  <> manifestPath
                  <> ":"
                  <> show lineNo
                  <> ": backlog rows require expected=xfail"
              )
          pure
            ParityCase
              { id = trim caseId
              , domain = trim caseDomain
              , specRef = trim caseSpecRef
              , kind = caseKind'
              , expected = caseExpected
              , source = caseSource
              , origin = caseOrigin
              , originRef = caseOriginRef
              , options = caseOptions
              , errContains = toMaybeField errContainsRaw
              , oracles = caseOracles
              , owner = caseOwner
              , exitCriteria = caseExit
              }
        _ ->
          fail
            ( "parity-manifest:"
                <> manifestPath
                <> ":"
                <> show lineNo
                <> ": expected 13 tab-separated columns"
            )

    parseSource manifestDir lineNo rawSource =
      let src = trim rawSource
      in case stripPrefix "inline:" src of
          Just inlineKey ->
            case lookupInlineParitySource inlineKey of
              Just inlineSrc -> pure (ParityInline inlineKey inlineSrc)
              Nothing ->
                fail
                  ( "parity-manifest:"
                      <> manifestPath
                      <> ":"
                      <> show lineNo
                      <> ": unknown inline source key: "
                      <> show inlineKey
                  )
          Nothing -> do
            let sourcePath =
                  if isAbsolute src
                    then src
                    else manifestDir </> src
            sourceExists <- doesFileExist sourcePath
            unless sourceExists $
              fail
                ( "parity-manifest:"
                    <> manifestPath
                    <> ":"
                    <> show lineNo
                    <> ": source file not found: "
                    <> sourcePath
                )
            pure (ParityFile sourcePath)

    parseOrigin lineNo rawOrigin =
      case map toLower (trim rawOrigin) of
        "manual" -> pure "manual"
        "cts" -> pure "cts"
        other ->
          fail
            ( "parity-manifest:"
                <> manifestPath
                <> ":"
                <> show lineNo
                <> ": unknown origin: "
                <> show other
            )

    parseOptions lineNo rawOptions = do
      let tokens = map trim (filter (not . null . trim) (splitBy ',' rawOptions))
      let hasBase = "base" `elem` tokens
      let hasNoBase = "no-base" `elem` tokens
      when (hasBase && hasNoBase) $
        fail
          ( "parity-manifest:"
              <> manifestPath
              <> ":"
              <> show lineNo
              <> ": options cannot include both base and no-base"
          )
      let seed = if hasNoBase then [] else defaultOpts
      let optionTokens = filter (\tok -> tok /= "base" && tok /= "no-base") tokens
      foldM (appendOption lineNo) seed optionTokens

    appendOption lineNo acc token =
      case token of
        "sampler:combined" -> pure (acc <> [OptSamplerMode SamplerCombined])
        "sampler:separate" -> pure (acc <> [OptSamplerMode SamplerSeparate])
        "spec:strict" -> pure (acc <> [OptOverrideSpecMode SpecStrict])
        "spec:parity" -> pure (acc <> [OptOverrideSpecMode SpecParity])
        _ ->
          case stripPrefix "feature:" token of
            Just feat | not (null feat) -> pure (acc <> [OptEnableFeature feat])
            _ ->
              fail
                ( "parity-manifest:"
                    <> manifestPath
                    <> ":"
                    <> show lineNo
                    <> ": unknown option token: "
                    <> show token
                )

    isInlineSource paritySource =
      case paritySource of
        ParityInline {} -> True
        ParityFile {} -> False

    parseExpected lineNo raw =
      case map toLower (trim raw) of
        "pass" -> pure ParityPass
        "fail" -> pure ParityFail
        "xfail" -> pure ParityXFail
        other ->
          fail
            ( "parity-manifest:"
                <> manifestPath
                <> ":"
                <> show lineNo
                <> ": unknown expected value: "
                <> show other
                )

loadParityRules :: FilePath -> IO [String]
loadParityRules rulesPath = do
  exists <- doesFileExist rulesPath
  unless exists $
    fail ("parity-rules: missing " <> rulesPath)
  raw <- readFile rulesPath
  let entries =
        [ trim line0
        | line0 <- lines raw
        , let line = trim line0
        , not (null line)
        , not ("#" `isPrefixOf` line)
        , map toLower line /= "spec_ref"
        ]
  when (null entries) $
    fail ("parity-rules: no entries in " <> rulesPath)
  pure entries

assertParityRuleCoverage :: [String] -> [ParityCase] -> IO ()
assertParityRuleCoverage ruleRefs cases = do
  let rulesSet = Set.fromList ruleRefs
  let mappedCases = filter (not . isUnmappedBacklogCase) cases
  let refsFromCases = Set.fromList (map (.specRef) mappedCases)
  let missingInCases = Set.toList (rulesSet `Set.difference` refsFromCases)
  unless (null missingInCases) $
    fail
      ( "parity-rules: missing test cases for rule(s): "
          <> show missingInCases
      )
  let unknownRefs = Set.toList (refsFromCases `Set.difference` rulesSet)
  unless (null unknownRefs) $
    fail
      ( "parity-rules: manifest has spec_ref values not listed in rules.tsv: "
          <> show unknownRefs
      )

isBacklogCase :: ParityCase -> Bool
isBacklogCase = isMappedBacklogCase

isMappedBacklogCase :: ParityCase -> Bool
isMappedBacklogCase parityCase = map toLower parityCase.kind == "backlog"

isUnmappedBacklogCase :: ParityCase -> Bool
isUnmappedBacklogCase parityCase = map toLower parityCase.kind == "backlog-unmapped"

runParityCase :: Maybe FilePath -> Maybe FilePath -> ParityCase -> IO ()
runParityCase spirvVal nagaExe parityCase = do
  result <- compileParityCase parityCase
  srcText <- loadParitySourceText parityCase.source
  case parityCase.expected of
    ParityPass ->
      case result of
        Left err ->
          fail
            ( "parity-pass:"
                <> parityCase.id
                <> ": expected success, got "
                <> show err
            )
        Right (SomeShader shader) -> do
          when ("spirv-val" `elem` parityCase.oracles) $
            assertSpirv spirvVal ("parity:" <> parityCase.id) (shaderSpirv shader)
    ParityFail ->
      expectCompileFailure "parity-fail" parityCase result
    ParityXFail ->
      expectCompileFailure "parity-xfail" parityCase result
  checkNagaOracles nagaExe parityCase srcText

compileParityCase :: ParityCase -> IO (Either CompileError SomeShader)
compileParityCase parityCase =
  case parityCase.source of
    ParityFile path ->
      compileFileWith parityCase.options path
    ParityInline key src ->
      pure (compileWith parityCase.options (SourceInline ("<inline:" <> key <> ">") src))

loadParitySourceText :: ParitySource -> IO String
loadParitySourceText paritySource =
  case paritySource of
    ParityFile path -> readFile path
    ParityInline _ inlineSrc -> pure inlineSrc

expectCompileFailure :: String -> ParityCase -> Either CompileError SomeShader -> IO ()
expectCompileFailure tag parityCase result =
  case result of
    Left err ->
      case parityCase.errContains of
        Nothing -> pure ()
        Just needle ->
          unless (needle `isInfixOf` err.ceMessage) $
            fail
              ( tag
                  <> ":"
                  <> parityCase.id
                  <> ": expected error containing "
                  <> show needle
                  <> ", got "
                  <> show err
              )
    Right _ ->
      fail
        ( tag
            <> ":"
            <> parityCase.id
            <> ": expected compile failure for "
            <> paritySourceLabel parityCase.source
        )

checkNagaOracles :: Maybe FilePath -> ParityCase -> String -> IO ()
checkNagaOracles mNagaExe parityCase src = do
  let wantsPass = "naga-pass" `elem` parityCase.oracles
  let wantsFail = "naga-fail" `elem` parityCase.oracles
  when (wantsPass && wantsFail) $
    fail ("parity-manifest:" <> parityCase.id <> ": cannot request both naga-pass and naga-fail")
  when (wantsPass || wantsFail) $
    case mNagaExe of
      Nothing -> pure ()
      Just nagaExe -> do
        (ok, logMsg) <- runNagaCheck nagaExe ("parity:" <> parityCase.id) src
        let expectedOk = wantsPass
        unless (ok == expectedOk) $
          fail
            ( "parity-manifest:"
                <> parityCase.id
                <> ": naga expected "
                <> show expectedOk
                <> ", got "
                <> show ok
                <> "\n"
                <> logMsg
            )

paritySourceLabel :: ParitySource -> String
paritySourceLabel paritySource =
  case paritySource of
    ParityFile path -> path
    ParityInline key _ -> "inline:" <> key

toMaybeField :: String -> Maybe String
toMaybeField raw =
  let val = trim raw
  in if null val then Nothing else Just val

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

splitBy :: Char -> String -> [String]
splitBy sep str =
  case break (== sep) str of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitBy sep rest

checkSwitchConstValidation :: IO ()
checkSwitchConstValidation =
  case compileBytes defaultOpts badSwitchShader of
    Left _ -> pure ()
    Right _ -> fail "switch-const: expected failure for non-const selector"

checkConstAssertValidation :: IO ()
checkConstAssertValidation =
  case compileBytes defaultOpts badConstAssertShader of
    Left _ -> pure ()
    Right _ -> fail "const-assert: expected failure for false const_assert"

checkOverrideSpecialization :: Maybe FilePath -> IO ()
checkOverrideSpecialization spirvVal = do
  let opts = [OptOverrides [("scale", OVI32 4)]]
  case compileBytes opts overrideSpecShader of
    Left err -> fail ("override-specialization: " <> show err)
    Right bytes -> assertSpirv spirvVal "override-specialization" bytes

checkOverrideDefault :: Maybe FilePath -> IO ()
checkOverrideDefault spirvVal =
  case compileInline defaultOpts overrideDefaultShader of
    Left err -> fail ("override-default: " <> show err)
    Right (SomeShader shader) -> do
      assertSpirv spirvVal "override-default" (shaderSpirv shader)
      let overrides = (shaderInterface shader).siOverrides
      case find (\o -> o.oiName == "scale") overrides of
        Nothing -> fail "override-default: missing scale override"
        Just info -> do
          case info.oiType of
            TLScalar scalar _ _ ->
              unless (show scalar == "I32") $
                fail "override-default: scale should be i32"
            _ -> fail "override-default: scale should be scalar"
          unless (isJust info.oiSpecId) $
            fail "override-default: scale should be runtime-specializable"
      case find (\o -> o.oiName == "mode") overrides of
        Nothing -> fail "override-default: missing mode override"
        Just info -> do
          unless (info.oiId == Just 7) $
            fail "override-default: expected @id(7) for mode"
          unless (info.oiSpecId == Just 7) $
            fail "override-default: expected spec id 7 for mode"
          case info.oiType of
            TLScalar scalar _ _ ->
              unless (show scalar == "U32") $
                fail "override-default: mode should be u32"
            _ -> fail "override-default: mode should be scalar"

checkOverrideMissing :: IO ()
checkOverrideMissing =
  case compileInline defaultOpts overrideShader of
    Left err -> fail ("override-missing: " <> show err)
    Right (SomeShader shader) -> do
      let overrides = (shaderInterface shader).siOverrides
      unless (any (\o -> o.oiName == "scale") overrides) $
        fail "override-missing: expected scale override in interface"

checkOverrideDependency :: Maybe FilePath -> IO ()
checkOverrideDependency spirvVal =
  case compileInline defaultOpts overrideSpecOpShader of
    Left err -> fail ("override-dependency: " <> show err)
    Right (SomeShader shader) -> do
      assertSpirv spirvVal "override-dependency" (shaderSpirv shader)
      let overrides = (shaderInterface shader).siOverrides
      case find (\o -> o.oiName == "scale") overrides of
        Nothing -> fail "override-dependency: missing scale override"
        Just info ->
          unless (isNothing info.oiSpecId) $
            fail "override-dependency: derived override should not be runtime-specializable"

checkOverrideParityMode :: IO ()
checkOverrideParityMode = do
  let opts = [OptOverrideSpecMode SpecParity]
  case compileInline opts overrideSpecOpShader of
    Left err -> fail ("override-parity: " <> show err)
    Right (SomeShader shader) -> do
      let overrides = (shaderInterface shader).siOverrides
      case find (\o -> o.oiName == "scale") overrides of
        Nothing -> fail "override-parity: missing scale override"
        Just info ->
          unless (isJust info.oiSpecId) $
            fail "override-parity: expected derived override to be runtime-specializable"

checkDiagnosticOverride :: IO ()
checkDiagnosticOverride =
  case compileBytesWithDiagnostics defaultOpts diagnosticShader of
    Left err -> fail ("diagnostic-override: " <> show err)
    Right (_, diags) ->
      unless (null diags) $
        fail "diagnostic-override: expected no diagnostics when off"

checkDiagnosticWarning :: IO ()
checkDiagnosticWarning =
  case compileBytesWithDiagnostics defaultOpts diagnosticWarnShader of
    Left err -> fail ("diagnostic-warning: " <> show err)
    Right (_, diags) ->
      case find (\d -> d.diagRule == "const_assert" && d.diagSeverity == DiagWarning) diags of
        Nothing -> fail "diagnostic-warning: expected warning diagnostic"
        Just d ->
          unless (isJust d.diagLine && isJust d.diagColumn) $
            fail "diagnostic-warning: expected source location on diagnostic"

checkDiagnosticUnreachable :: IO ()
checkDiagnosticUnreachable =
  case compileBytesWithDiagnostics defaultOpts diagnosticUnreachableShader of
    Left err -> fail ("diagnostic-unreachable: " <> show err)
    Right (_, diags) ->
      unless (any (\d -> d.diagRule == "unreachable_code" && d.diagSeverity == DiagWarning) diags) $
        fail "diagnostic-unreachable: expected unreachable_code warning"

checkDiagnosticUnusedExpr :: IO ()
checkDiagnosticUnusedExpr =
  case compileBytesWithDiagnostics defaultOpts diagnosticUnusedExprShader of
    Left err -> fail ("diagnostic-unused-expr: " <> show err)
    Right (_, diags) ->
      unless (any (\d -> d.diagRule == "unused_expression" && d.diagSeverity == DiagWarning) diags) $
        fail "diagnostic-unused-expr: expected unused_expression warning"

checkDiagnosticUnusedVar :: IO ()
checkDiagnosticUnusedVar =
  case compileBytesWithDiagnostics defaultOpts diagnosticUnusedVarShader of
    Left err -> fail ("diagnostic-unused-var: " <> show err)
    Right (_, diags) ->
      unless (any (\d -> d.diagRule == "unused_variable" && d.diagSeverity == DiagWarning) diags) $
        fail "diagnostic-unused-var: expected unused_variable warning"

checkDiagnosticUnusedParam :: IO ()
checkDiagnosticUnusedParam =
  case compileBytesWithDiagnostics defaultOpts diagnosticUnusedParamShader of
    Left err -> fail ("diagnostic-unused-param: " <> show err)
    Right (_, diags) ->
      unless (any (\d -> d.diagRule == "unused_parameter" && d.diagSeverity == DiagWarning) diags) $
        fail "diagnostic-unused-param: expected unused_parameter warning"

checkDiagnosticShadowing :: IO ()
checkDiagnosticShadowing =
  case compileBytesWithDiagnostics defaultOpts diagnosticShadowingShader of
    Left err -> fail ("diagnostic-shadowing: " <> show err)
    Right (_, diags) ->
      unless (any (\d -> d.diagRule == "shadowing" && d.diagSeverity == DiagWarning) diags) $
        fail "diagnostic-shadowing: expected shadowing warning"

checkDiagnosticConstantCondition :: IO ()
checkDiagnosticConstantCondition =
  case compileBytesWithDiagnostics defaultOpts diagnosticConstantCondShader of
    Left err -> fail ("diagnostic-constant-condition: " <> show err)
    Right (_, diags) ->
      unless (any (\d -> d.diagRule == "constant_condition" && d.diagSeverity == DiagWarning) diags) $
        fail "diagnostic-constant-condition: expected constant_condition warning"

checkDiagnosticDuplicateCase :: IO ()
checkDiagnosticDuplicateCase =
  case compileBytesWithDiagnostics defaultOpts diagnosticDuplicateCaseShader of
    Left err -> fail ("diagnostic-duplicate-case: " <> show err)
    Right (_, diags) ->
      unless (any (\d -> d.diagRule == "duplicate_case" && d.diagSeverity == DiagWarning) diags) $
        fail "diagnostic-duplicate-case: expected duplicate_case warning"


combinedInputShader :: Shader 'SamplerCombined
  '[ 'Binding "params" 'BUniform 0 0 ('TStruct '[ 'Field "v" ('TVec 4 'SF32)])
   , 'Binding "tex" 'BTexture2D 0 1 ('TTexture2D 'SF32)
   ]
combinedInputShader =
  $(spirv defaultCompileOptions imports [wesl|
struct Params { v: vec4<f32>; };

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var tex: texture_2d<f32>;
@group(0) @binding(2) var samp: sampler;

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x / 640.0, frag_coord.y / 480.0);
  return textureSample(tex, samp, uv);
}
|])
