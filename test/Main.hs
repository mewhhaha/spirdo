{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Executable entry point.
module Main (main) where

import Control.Monad (forM_, unless, replicateM, when)
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.Foldable (toList)
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.Word (Word16, Word32, Word64)
import GHC.Float (castFloatToWord32)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getTemporaryDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Exit (ExitCode(..))
import System.IO (hClose, openBinaryTempFile)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck (Gen, arbitrary, generate)
import Unsafe.Coerce (unsafeCoerce)

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
defaultOpts = [OptEnableFeature "f16"]

separateOpts :: [Option]
separateOpts = [OptSamplerMode SamplerSeparate]

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

main :: IO ()
main = do
  spirvVal <- findExecutable "spirv-val"
  nagaExe <- findExecutable "naga"
  requireValidators <- fmap isTruthy <$> lookupEnv "SPIRDO_REQUIRE_VALIDATORS"
  when (requireValidators == Just True) $ do
    when (isNothing spirvVal) $
      fail "SPIRDO_REQUIRE_VALIDATORS=1 but spirv-val was not found in PATH"
    when (isNothing nagaExe) $
      fail "SPIRDO_REQUIRE_VALIDATORS=1 but naga was not found in PATH"
  let tests =
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
  forM_ tests $ \(label, src) -> do
    bytes <- case compileBytes defaultOpts src of
      Left err -> fail (label <> ": " <> show err)
      Right bs -> pure bs
    assertSpirv spirvVal label bytes

  checkIfTranslation
  checkImportCompile spirvVal
  checkImportItemCompile spirvVal
  checkImportAliasCompile spirvVal
  checkImportStructZeroCtorCompile spirvVal
  checkImportQualifiedConstCompile spirvVal
  checkCtsFixtures spirvVal
  checkSwitchConstValidation
  checkConstAssertValidation
  checkMalformedHexLiteral
  checkStructFieldSeparators
  checkNonIfStatementAttrsRejected
  checkNonIfSwitchCaseAttrsRejected
  checkNonIfLoopAttrsRejected
  checkModuleConstDecl
  checkInvalidMatrixDimensionsRejected
  checkNegativeI32Range
  checkComputeRequiresWorkgroupSize
  checkSuperImportContainment
  checkOverrideSpecialization spirvVal
  checkOverrideDefault spirvVal
  checkOverrideMissing
  checkWorkgroupSizeOverrideReject
  checkOverrideDependency spirvVal
  checkOverrideParityMode
  checkDiagnosticOverride
  checkDiagnosticWarning
  checkDiagnosticUnreachable
  checkDiagnosticUnusedExpr
  checkDiagnosticUnusedVar
  checkDiagnosticUnusedParam
  checkDiagnosticShadowing
  checkDiagnosticConstantCondition
  checkDiagnosticDuplicateCase
  checkNagaOracleParity nagaExe
  checkTextureBarrierStage
  checkBlendSrcEnabled spirvVal
  checkBlendSrcRequiresEnable
  checkBlendSrcPairRules
  checkInterpolateIntegerRule
  checkInvariantRule
  checkLocationIoTypeRule
  checkF16RequiresEnable
  checkStorageWriteAccessRejected
  checkPointerParamAddressSpaceRules
  checkEntryPointerParamRejected
  checkFragmentReturnBindingRequired
  checkNonEntryParamIoAttrsRejected
  checkNegativeLocationRejected
  checkLocationTooManyArgsRejected
  checkStageAttrWithArgsRejected
  checkDuplicateLocationAttrRejected
  checkDuplicateBuiltinAttrRejected
  checkDuplicateGroupBindingAttrsRejected
  checkDuplicateStageAttrsRejected
  checkNonEntryFunctionAttrAccepted
  checkSamplerInterface
  checkCombinedSamplerInterface
  checkSamplerValueCombinedError
  checkPackUniformLayout
  checkPackUniformErrors
  checkPackUniformFrom
  checkUniformStorable
  checkVertexAttributes
  checkBindingPlan
  checkInputOrdering
  checkInputsCombinedMissingSampler
  checkInputsCombinedOk
  checkInputsMissingBindingsRejected
  checkInputsSeparateModeRejectsSampledTexture
  checkInputsDuplicateBuilder
  checkQuickCheck
  checkDuplicateBindings
  checkGoldenSpirv
  putStrLn "All tests passed."

assertSpirv :: Maybe FilePath -> String -> BS.ByteString -> IO ()
assertSpirv spirvVal label bytes = do
  unless (BS.length bytes `mod` 4 == 0) $
    fail (label <> ": SPIR-V size not multiple of 4")
  let magic = word32At bytes 0
  unless (magic == 0x07230203) $
    fail (label <> ": bad SPIR-V magic")
  validateSpirvVal spirvVal label bytes

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
      (path, handle) <- openBinaryTempFile tmpDir (safeLabel <> ".spv")
      BS.hPut handle bytes
      hClose handle
      (code, _out, err) <- readProcessWithExitCode exe [path] ""
      removeFile path
      case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> fail (label <> ": spirv-val failed: " <> err)
  where
    sanitize c
      | c == '/' || c == '\\' = '_'
      | otherwise = c


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
  let safeLabel = map sanitize label
      srcPath = tmpDir </> (safeLabel <> ".wgsl")
      outPath = tmpDir </> (safeLabel <> ".spv")
  writeFile srcPath src
  (code, out, err) <- readProcessWithExitCode nagaExe [srcPath, outPath, "--input-kind", "wgsl"] ""
  removeIfExists srcPath
  removeIfExists outPath
  case code of
    ExitSuccess -> pure (True, out <> err)
    ExitFailure _ -> pure (False, out <> err)
  where
    sanitize c
      | c == '/' || c == '\\' || c == ':' = '_'
      | otherwise = c

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
  case validateUniformStorable layout (Proxy @Float) of
    Left err -> fail ("uniform-storable: unexpected failure: " <> err)
    Right () -> pure ()
  case validateUniformStorable layout (Proxy @Word64) of
    Left _ -> pure ()
    Right () -> fail "uniform-storable: expected size mismatch for Word64"
  packed <- packUniformStorable layout (1.0 :: Float)
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

checkInputsCombinedMissingSampler :: IO ()
checkInputsCombinedMissingSampler =
  case inputsFor combinedInputShader
        (Inputs.uniform @"params" (ParamsU (V4 1 2 3 4) :: ParamsU)
          <> Inputs.texture @"tex" (TextureHandle 9)) of
    Left err ->
      unless ("missing sampler for textures" `isInfixOf` err.ieMessage) $
        fail ("inputs-combined-missing: unexpected error: " <> err.ieMessage)
    Right _ ->
      fail "inputs-combined-missing: expected error for missing sampler"

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
      in case inputsFor
            shader
            (Inputs.sampledTexture @"tex" (TextureHandle 9) (SamplerHandle 3)) of
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
  forM_ [1 :: Int .. 200] $ \_ -> do
    value <- generate arbitrary
    checkPackScalarF32 value
  forM_ [1 :: Int .. 200] $ \_ -> do
    value <- generate genV2
    checkPackVec2F32 value
  forM_ [1 :: Int .. 200] $ \_ -> do
    value <- generate genV3
    checkPackVec3F32 value
  forM_ [1 :: Int .. 200] $ \_ -> do
    value <- generate genV4
    checkPackVec4F32 value
  forM_ [1 :: Int .. 200] $ \_ -> do
    value <- generate genM2
    checkPackMat2F32 value
  checkPackUniformExtendedRandom
  checkPackUniformExtended2

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
      size = 16 :: Word32
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
        let paddingBytes = [BS.index bytes (12 + i) | i <- [0 .. 3]]
        unless (all (== 0) paddingBytes) $
          fail "quickcheck: packVec3F32 padding not zeroed"

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

genV2 :: Gen (V2 Float)
genV2 = V2 <$> arbitrary <*> arbitrary

genV3 :: Gen (V3 Float)
genV3 = V3 <$> arbitrary <*> arbitrary <*> arbitrary

genM2 :: Gen (M2 Float)
genM2 = M2 <$> genV2 <*> genV2

genM3 :: Gen (M3 Float)
genM3 = M3 <$> genV3 <*> genV3 <*> genV3

genM4 :: Gen (M4 Float)
genM4 = M4 <$> genV4 <*> genV4 <*> genV4 <*> genV4

genInner :: Gen (V2 Float, Float)
genInner = (,) <$> genV2 <*> arbitrary

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
          forM_ [1 :: Int .. 80] $ \_ -> do
            m3 <- generate genM3
            m4 <- generate genM4
            arr2 <- generate (sequence [genV2, genV2, genV2, genV2])
            arr3 <- generate (sequence [genV3, genV3, genV3])
            (innerA, innerB) <- generate genInner
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
          forM_ [1 :: Int .. 60] $ \_ -> do
            m34Vals <- replicateM 12 (generate arbitrary)
            m43Vals <- replicateM 12 (generate arbitrary)
            mats <- generate (sequence [genM2, genM2])
            inners <- replicateM 2 (generate ((,) <$> genV3 <*> arbitrary))
            h0 <- generate arbitrary
            h1 <- generate arbitrary
            h2 <- generate arbitrary
            h3 <- generate arbitrary
            h4 <- generate arbitrary
            h5 <- generate arbitrary
            let m34 = m3x4FromList m34Vals
            let m43 = m4x3FromList m43Vals
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
          M3x4 (V4 a b c d) (V4 e f g h) (V4 i j k l)
        _ -> error "expected 12 values for M3x4"

    m4x3FromList vals =
      case vals of
        [a,b,c,d,e,f,g,h,i,j,k,l] ->
          M4x3 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l)
        _ -> error "expected 12 values for M4x3"

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
checkSuperImportContainment = do
  tmp <- getTemporaryDirectory
  let rootDir = tmp </> "spirdo-super-containment"
      libFile = rootDir </> "lib.wesl"
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

  createDirectoryIfMissing True rootDir
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

checkWorkgroupSizeOverrideReject :: IO ()
checkWorkgroupSizeOverrideReject =
  case compileInline defaultOpts workgroupOverrideShader of
    Left err ->
      unless ("runtime specialization" `isInfixOf` err.ceMessage) $
        fail ("workgroup-size-overrides: unexpected error: " <> show err)
    Right _ -> fail "workgroup-size-overrides: expected failure for override-dependent workgroup_size"

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
  result <- compileFile path
  case result of
    Left err -> fail ("import-qualified-const: " <> show err)
    Right (SomeShader shader) -> assertSpirv spirvVal "import-qualified-const" (shaderSpirv shader)

checkCtsFixtures :: Maybe FilePath -> IO ()
checkCtsFixtures spirvVal = do
  positive <- collectCtsFixtures ("test" </> "cts" </> "positive")
  negative <- collectCtsFixtures ("test" </> "cts" </> "negative")
  forM_ positive $ \path -> do
    result <- compileFile path
    case result of
      Left err -> fail ("cts-positive: " <> path <> ": " <> show err)
      Right (SomeShader shader) ->
        assertSpirv spirvVal ("cts-positive:" <> path) (shaderSpirv shader)
  forM_ negative $ \path -> do
    result <- compileFile path
    case result of
      Left err ->
        case expectedCtsNegativeMessage path of
          Nothing ->
            fail ("cts-negative: missing expected error mapping for fixture " <> path)
          Just needle ->
            unless (needle `isInfixOf` err.ceMessage) $
              fail
                ( "cts-negative: unexpected error for "
                    <> path
                    <> "\nexpected to contain: "
                    <> show needle
                    <> "\nactual: "
                    <> show err
                )
      Right _ -> fail ("cts-negative: expected failure for " <> path)

collectCtsFixtures :: FilePath -> IO [FilePath]
collectCtsFixtures dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      let exts = [".wesl", ".wgsl"]
      pure [dir </> e | e <- entries, takeExtension e `elem` exts, not ("_" `isPrefixOf` e)]

expectedCtsNegativeMessage :: FilePath -> Maybe String
expectedCtsNegativeMessage path =
  case takeFileName path of
    "import_duplicate_alias.wesl" -> Just "duplicate import aliases"
    "import_duplicate_target.wesl" -> Just "duplicate imports:"
    "missing_workgroup_size.wesl" -> Just "@workgroup_size is required for @compute"
    "override_cycle.wesl" -> Just "override dependency cycle"
    "scope_out_of_scope.wesl" -> Just "unknown identifier: inner"
    "unknown_identifier.wesl" -> Just "unknown identifier: missing_value"
    "const_assert_type_mismatch.wesl" -> Just "const_assert comparison requires matching types"
    "const_assert_pointer_compare.wesl" -> Just "const int expression references a composite value"
    "const_fn_switch_fallthrough.wesl" -> Just "non-void function must return a value"
    _ -> Nothing

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

malformedHexLiteralShader :: String
malformedHexLiteralShader =
  unlines
    [ "let bad = 0x;"
    , "let bad2 = 0x__;"
    , "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = bad + bad2;"
    , "}"
    ]

badStructFieldShader :: String
badStructFieldShader =
  unlines
    [ "struct Params {"
    , "  a: i32"
    , "  b: i32;"
    , "}"
    , "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {}"
    ]

nonIfStatementAttrShader :: String
nonIfStatementAttrShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  @group(0) let x = i32(1);"
    , "}"
    ]

nonIfSwitchCaseAttrShader :: String
nonIfSwitchCaseAttrShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  switch (gid.x) {"
    , "    @location(0) case 0: {}"
    , "  }"
    , "}"
    ]

nonIfLoopAttrShader :: String
nonIfLoopAttrShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  loop {"
    , "    @id(0) break;"
    , "  }"
    , "}"
    ]

negativeI32RangeShader :: String
negativeI32RangeShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _: i32 = -2147483648;"
    , "}"
    ]

computeWithoutWorkgroupSizeShader :: String
computeWithoutWorkgroupSizeShader =
  unlines
    [ "@compute"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid;"
    , "}"
    ]

workgroupOverrideShader :: String
workgroupOverrideShader =
  unlines
    [ "override scale: i32 = 1;"
    , "@compute @workgroup_size(scale)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {}"
    ]

moduleConstDeclShader :: String
moduleConstDeclShader =
  unlines
    [ "const SCALE: i32 = 1;"
    , "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = SCALE + i32(gid.x);"
    , "}"
    ]

invalidMatrixDimensionsShader :: String
invalidMatrixDimensionsShader =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  let m = mat5x5<f32>(1.0);"
    , "  return vec4<f32>(m[0][0]);"
    , "}"
    ]

computeShader :: String
computeShader =
  unlines
    [ "let scale = 2.0;"
    , "struct Data {"
    , "  values: array<u32, 4>;"
    , "  counter: atomic<u32>;"
    , "  accum: atomic<u32>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<storage, read_write> data: Data;"
    , "fn bump(v: f32) -> f32 {"
    , "  return v * v;"
    , "}"
    , "fn bump(v: vec2<f32>) -> vec2<f32> {"
    , "  return vec2(v.x * v.x, v.y * v.y);"
    , "}"
    , "@compute @workgroup_size(8, 8, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let idx = gid.x;"
    , "  let _tmp = scale;"
    , "  let v = vec4(1.0, 2.0, 3.0, 4.0);"
    , "  let sw = v.zw;"
    , "  let m = mat2x2(vec2(1.0, 0.0), vec2(0.0, 1.0));"
    , "  let col = m[0];"
    , "  let s = col.y;"
    , "  let arr = array(1, 2, 3, 4);"
    , "  var sum = arr[0] + arr[1];"
    , "  var j = 0;"
    , "  while (j < 4) {"
    , "    j = j + 1;"
    , "    if (j == 2) {"
    , "      continue;"
    , "    }"
    , "    sum = sum + j;"
    , "  }"
    , "  var k = 0;"
    , "  for (var i = 0; i < 3; i = i + 1) {"
    , "    k = k + i;"
    , "  }"
    , "  let ok = (idx == 0) || (idx != 1 && !(idx > 2));"
    , "  if (ok) {"
    , "    let v0 = abs(-1.0);"
    , "    let v1 = clamp(v0, 0.0, 1.0);"
    , "    let v2 = mix(v1, 1.0, 0.25);"
    , "    let v3 = max(v2, sqrt(4.0));"
    , "    let v4 = min(v3, 2.0);"
    , "    let v5 = cos(v4) + sin(v4);"
    , "    let v6 = pow(v5, 2.0);"
    , "    let v7 = dot(vec3(v6, v6, v6), normalize(vec3(1.0, 2.0, 3.0)));"
    , "    let _len = length(vec2(sw.x, sw.y));"
    , "    let _cast = u32(v7);"
    , "    let _mat = m[1];"
    , "    let _scalar = m[1][0];"
    , "    let _b0 = bump(v4);"
    , "    let _b1 = bump(vec2(0.5, 1.5));"
    , "    let _s0 = s + f32(k);"
    , "  }"
    , "  if (idx < 4) {"
    , "    let old = atomicAdd(data.counter, 1);"
    , "    let cur = atomicLoad(data.counter);"
    , "    atomicStore(data.accum, cur + 1);"
    , "    let _m0 = atomicMax(data.accum, old);"
    , "    let _m1 = atomicMin(data.accum, 0);"
    , "    let _m2 = atomicXor(data.accum, 1);"
    , "    let _m3 = atomicOr(data.accum, 2);"
    , "    let _m4 = atomicAnd(data.accum, 3);"
    , "    let _m5 = atomicExchange(data.accum, 4);"
    , "    data.values[idx] = data.values[idx] + u32(sum);"
    , "  }"
    , "}"
    ]

barrierShader :: String
barrierShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(local_invocation_index) li: u32) {"
    , "  if (li == 0u) {"
    , "    workgroupBarrier();"
    , "    storageBarrier();"
    , "    textureBarrier();"
    , "  }"
    , "}"
    ]

textureBarrierFragmentShader :: String
textureBarrierFragmentShader =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  textureBarrier();"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

atomicCompareExchangeShader :: String
atomicCompareExchangeShader =
  unlines
    [ "struct Data {"
    , "  value: atomic<u32>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<storage, read_write> data: Data;"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  let r = atomicCompareExchangeWeak(data.value, 0u, 1u);"
    , "  if (r.exchanged) {"
    , "    atomicStore(data.value, r.old_value);"
    , "  }"
    , "}"
    ]

typedCtorShader :: String
typedCtorShader =
  unlines
    [ "enable f16;"
    , "struct Z {"
    , "  a: f32;"
    , "  b: vec2<f32>;"
    , "};"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let a = vec4<f32>(1.0);"
    , "  let b = vec2<f32>(0.5);"
    , "  let c = vec3<f32>(a.x, b.x, b.y);"
    , "  let d = vec3<f32>(b, 1.0);"
    , "  let sf: vec2f = vec2f(1.0);"
    , "  let si = vec2i(1i, 2i);"
    , "  let su = vec3u(1u, 2u, 3u);"
    , "  let sh = vec2h(1.0h);"
    , "  let m = mat2x2<f32>(1.0, 0.0, 0.0, 1.0);"
    , "  let m2 = mat2x2<f32>(1.0);"
    , "  let z0 = vec4<f32>();"
    , "  let z1 = mat2x2<f32>();"
    , "  let arr = array<vec2<f32>, 2>(vec2<f32>(0.25, 0.5), vec2<f32>(0.75, 1.0));"
    , "  let zarr = array<vec2<f32>, 2>();"
    , "  let zstructs = array<Z, 2>();"
    , "  let zi = i32();"
    , "  let zf = f32();"
    , "  let zs = Z();"
    , "  let _ = m[0][0] + m2[1][1] + sf.x + f32(si.x) + f32(su.x) + f32(sh.x) + arr[1].y + z0.x + z1[0][0] + zarr[1].y + zstructs[1].a + f32(zi) + zf + zs.a + zs.b.x;"
    , "  return vec4(d.x, d.y, d.z, a.w);"
    , "}"
    ]

sampleMaskShader :: String
sampleMaskShader =
  unlines
    [ "struct FragOut {"
    , "  @location(0) color: vec4<f32>;"
    , "  @builtin(sample_mask) mask: u32;"
    , "};"
    , "@fragment"
    , "fn main(@builtin(sample_mask) inMask: u32) -> FragOut {"
    , "  return FragOut(vec4<f32>(1.0, 0.0, 0.0, 1.0), inMask);"
    , "}"
    ]

fragmentShader :: String
fragmentShader =
  unlines
    [ "struct Params {"
    , "  time_res: vec4<f32>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params: Params;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let res = vec2(params.time_res.y, params.time_res.z);"
    , "  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);"
    , "  let dx = dpdx(uv.x);"
    , "  let dy = dpdy(uv.y);"
    , "  let w = fwidth(uv.x);"
    , "  let edge = clamp(abs(dx) + abs(dy), 0.0, 1.0);"
    , "  let t0 = fract(uv.x * 3.0 + params.time_res.x * 0.1);"
    , "  let glow = smoothstep(0.2, 0.8, t0);"
    , "  let dist = distance(uv, vec2(0.5, 0.5));"
    , "  let refl = reflect(normalize(vec3(uv.x - 0.5, uv.y - 0.5, 1.0)), normalize(vec3(0.2, 0.8, 0.6)));"
    , "  let tint = mix(vec3(0.2, 0.4, 0.8), vec3(1.0, 0.2, 0.1), vec3(edge, edge, edge));"
    , "  return vec4(tint.x + w + glow * 0.1 + dist * 0.2 + refl.x * 0.05, tint.y, tint.z, 1.0);"
    , "}"
    ]

vertexIoAttrShader :: String
vertexIoAttrShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) @invariant pos: vec4<f32>;"
    , "  @location(0) @interpolate(flat) id: u32;"
    , "  @location(1) @interpolate(linear, centroid) uv: vec2<f32>;"
    , "};"
    , "@vertex"
    , "fn main(@builtin(vertex_index) idx: u32) -> VsOut {"
    , "  let x = f32(idx & 1u);"
    , "  return VsOut(vec4<f32>(x, 0.0, 0.0, 1.0), idx, vec2<f32>(x, 0.0));"
    , "}"
    ]

vertexShader :: String
vertexShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) position: vec4<f32>;"
    , "  @location(0) uv: vec2<f32>;"
    , "};"
    , "@vertex"
    , "fn main(@location(0) in_pos: vec2<f32>) -> VsOut {"
    , "  let pos = vec4(in_pos.x, in_pos.y, 0.0, 1.0);"
    , "  return VsOut(pos, in_pos);"
    , "}"
    ]

fragmentBlendSrcShader :: String
fragmentBlendSrcShader =
  unlines
    [ "enable dual_source_blending;"
    , "struct BlendOut {"
    , "  @location(0) @blend_src(0) c0: vec4<f32>;"
    , "  @location(0) @blend_src(1) c1: vec4<f32>;"
    , "};"
    , "@fragment"
    , "fn main() -> BlendOut {"
    , "  return BlendOut(vec4<f32>(1.0, 0.0, 0.0, 1.0), vec4<f32>(0.0, 1.0, 0.0, 1.0));"
    , "}"
    ]

fragmentBlendSrcNoEnableShader :: String
fragmentBlendSrcNoEnableShader =
  unlines
    [ "struct BlendOut {"
    , "  @location(0) @blend_src(0) c0: vec4<f32>;"
    , "  @location(0) @blend_src(1) c1: vec4<f32>;"
    , "};"
    , "@fragment"
    , "fn main() -> BlendOut {"
    , "  return BlendOut(vec4<f32>(1.0, 0.0, 0.0, 1.0), vec4<f32>(0.0, 1.0, 0.0, 1.0));"
    , "}"
    ]

fragmentBlendSrcOnlyOneShader :: String
fragmentBlendSrcOnlyOneShader =
  unlines
    [ "enable dual_source_blending;"
    , "struct BlendOut {"
    , "  @location(0) @blend_src(1) c1: vec4<f32>;"
    , "};"
    , "@fragment"
    , "fn main() -> BlendOut {"
    , "  return BlendOut(vec4<f32>(0.0, 1.0, 0.0, 1.0));"
    , "}"
    ]

fragmentBlendSrcLocationOneShader :: String
fragmentBlendSrcLocationOneShader =
  unlines
    [ "enable dual_source_blending;"
    , "struct BlendOut {"
    , "  @location(1) @blend_src(0) c0: vec4<f32>;"
    , "  @location(1) @blend_src(1) c1: vec4<f32>;"
    , "};"
    , "@fragment"
    , "fn main() -> BlendOut {"
    , "  return BlendOut(vec4<f32>(1.0, 0.0, 0.0, 1.0), vec4<f32>(0.0, 1.0, 0.0, 1.0));"
    , "}"
    ]

vertexInterpolateIntegerShader :: String
vertexInterpolateIntegerShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) pos: vec4<f32>;"
    , "  @location(0) @interpolate(linear) id: u32;"
    , "};"
    , "@vertex"
    , "fn main(@builtin(vertex_index) idx: u32) -> VsOut {"
    , "  return VsOut(vec4<f32>(0.0, 0.0, 0.0, 1.0), idx);"
    , "}"
    ]

vertexInvariantLocationShader :: String
vertexInvariantLocationShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) pos: vec4<f32>;"
    , "  @location(0) @invariant uv: vec2<f32>;"
    , "};"
    , "@vertex"
    , "fn main(@builtin(vertex_index) idx: u32) -> VsOut {"
    , "  let x = f32(idx & 1u);"
    , "  return VsOut(vec4<f32>(x, 0.0, 0.0, 1.0), vec2<f32>(x, 0.0));"
    , "}"
    ]

vertexBoolLocationShader :: String
vertexBoolLocationShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) pos: vec4<f32>;"
    , "  @location(0) b: bool;"
    , "};"
    , "@vertex"
    , "fn main() -> VsOut {"
    , "  return VsOut(vec4<f32>(0.0, 0.0, 0.0, 1.0), true);"
    , "}"
    ]

f16NoEnableShader :: String
f16NoEnableShader =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f16> {"
    , "  return vec4<f16>(1.0h);"
    , "}"
    ]

nagaTextureBarrierFragment :: String
nagaTextureBarrierFragment =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  textureBarrier();"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaStorageWriteBuffer :: String
nagaStorageWriteBuffer =
  unlines
    [ "struct Data { v: u32, }"
    , "@group(0) @binding(0)"
    , "var<storage, write> data: Data;"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  data.v = 1u;"
    , "}"
    ]

nagaBlendSrcEnabled :: String
nagaBlendSrcEnabled =
  unlines
    [ "enable dual_source_blending;"
    , "struct Out {"
    , "  @location(0) @blend_src(0) a: vec4<f32>,"
    , "  @location(0) @blend_src(1) b: vec4<f32>,"
    , "}"
    , "@fragment"
    , "fn main() -> Out {"
    , "  return Out(vec4<f32>(1.0), vec4<f32>(0.0));"
    , "}"
    ]

nagaBlendSrcNoEnable :: String
nagaBlendSrcNoEnable =
  unlines
    [ "struct Out {"
    , "  @location(0) @blend_src(0) a: vec4<f32>,"
    , "  @location(0) @blend_src(1) b: vec4<f32>,"
    , "}"
    , "@fragment"
    , "fn main() -> Out {"
    , "  return Out(vec4<f32>(1.0), vec4<f32>(0.0));"
    , "}"
    ]

nagaF16NoEnable :: String
nagaF16NoEnable =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f16> {"
    , "  return vec4<f16>(1.0h);"
    , "}"
    ]

nagaF16Enable :: String
nagaF16Enable =
  unlines
    [ "enable f16;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f16> {"
    , "  return vec4<f16>(1.0h);"
    , "}"
    ]

nagaInterpolateIntLinear :: String
nagaInterpolateIntLinear =
  unlines
    [ "struct Out {"
    , "  @builtin(position) pos: vec4<f32>,"
    , "  @location(0) @interpolate(linear) id: u32,"
    , "}"
    , "@vertex"
    , "fn main(@builtin(vertex_index) i: u32) -> Out {"
    , "  return Out(vec4<f32>(0.0, 0.0, 0.0, 1.0), i);"
    , "}"
    ]

nagaInterpolateFloatLinear :: String
nagaInterpolateFloatLinear =
  unlines
    [ "struct Out {"
    , "  @builtin(position) pos: vec4<f32>,"
    , "  @location(0) @interpolate(linear, centroid) uv: vec2<f32>,"
    , "}"
    , "@vertex"
    , "fn main(@builtin(vertex_index) i: u32) -> Out {"
    , "  let x = f32(i & 1u);"
    , "  return Out(vec4<f32>(0.0, 0.0, 0.0, 1.0), vec2<f32>(x, 0.0));"
    , "}"
    ]

nagaEntryPointerParam :: String
nagaEntryPointerParam =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(p: ptr<function, i32>) {"
    , "}"
    ]

nagaFragmentReturnNoBinding :: String
nagaFragmentReturnNoBinding =
  unlines
    [ "@fragment"
    , "fn main() -> vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaNonEntryParamIoAttrs :: String
nagaNonEntryParamIoAttrs =
  unlines
    [ "fn helper(@location(0) x: f32) -> f32 {"
    , "  return x;"
    , "}"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(helper(1.0));"
    , "}"
    ]

nagaNegativeLocation :: String
nagaNegativeLocation =
  unlines
    [ "@vertex"
    , "fn main(@location(-1) x: vec4<f32>) -> @builtin(position) vec4<f32> {"
    , "  return x;"
    , "}"
    ]

nagaLocationTooManyArgs :: String
nagaLocationTooManyArgs =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0, 1) vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaDuplicateLocationReturn :: String
nagaDuplicateLocationReturn =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) @location(1) vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaDuplicateBuiltinReturn :: String
nagaDuplicateBuiltinReturn =
  unlines
    [ "@vertex"
    , "fn main() -> @builtin(position) @builtin(position) vec4<f32> {"
    , "  return vec4<f32>(0.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

nagaDuplicateGroupAttr :: String
nagaDuplicateGroupAttr =
  unlines
    [ "struct U {"
    , "  v: vec4<f32>,"
    , "}"
    , "@group(0) @group(1) @binding(0)"
    , "var<uniform> u: U;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return u.v;"
    , "}"
    ]

nagaDuplicateBindingAttr :: String
nagaDuplicateBindingAttr =
  unlines
    [ "struct U {"
    , "  v: vec4<f32>,"
    , "}"
    , "@group(0) @binding(0) @binding(1)"
    , "var<uniform> u: U;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return u.v;"
    , "}"
    ]

nagaDuplicateFragmentStageAttr :: String
nagaDuplicateFragmentStageAttr =
  unlines
    [ "@fragment @fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaDuplicateWorkgroupSizeAttr :: String
nagaDuplicateWorkgroupSizeAttr =
  unlines
    [ "@compute @workgroup_size(1) @workgroup_size(2)"
    , "fn main() {"
    , "}"
    ]

nagaStageAttrWithArgs :: String
nagaStageAttrWithArgs =
  unlines
    [ "@fragment(1)"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaNonEntryFunctionAttr :: String
nagaNonEntryFunctionAttr =
  unlines
    [ "@workgroup_size(1)"
    , "fn helper() -> i32 {"
    , "  return 1;"
    , "}"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(f32(helper()));"
    , "}"
    ]

pointerParamWorkgroupShader :: String
pointerParamWorkgroupShader =
  unlines
    [ "var<workgroup> g: i32;"
    , "fn bump(p: ptr<workgroup, i32>) -> i32 {"
    , "  *p = *p + 1;"
    , "  return *p;"
    , "}"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  let y = bump(&g);"
    , "  if (y == 0) { }"
    , "}"
    ]

pointerParamStorageShader :: String
pointerParamStorageShader =
  unlines
    [ "struct Data { v: u32, }"
    , "@group(0) @binding(0)"
    , "var<storage, read_write> data: Data;"
    , "fn bump(p: ptr<storage, u32, read_write>) -> u32 {"
    , "  *p = *p + 1u;"
    , "  return *p;"
    , "}"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  let y = bump(&data.v);"
    , "  if (y == 0u) { }"
    , "}"
    ]

storageTextureShader :: String
storageTextureShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var<storage> out_tex: texture_storage_2d<rgba8unorm, write>;"
    , "@group(0) @binding(1)"
    , "var<storage> in_tex: texture_storage_2d<rgba8unorm, read>;"
    , "@compute @workgroup_size(8, 8, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let x = i32(gid.x);"
    , "  let y = i32(gid.y);"
    , "  let coord = vec2(x, y);"
    , "  let src = textureLoad(in_tex, coord);"
    , "  textureStore(out_tex, coord, vec4(src.x, src.y, src.z, src.w));"
    , "}"
    ]

storageWriteAccessShader :: String
storageWriteAccessShader =
  unlines
    [ "struct Data {"
    , "  values: array<u32, 4>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<storage, write> data: Data;"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  data.values[0] = 1u;"
    , "}"
    ]

samplerShader :: String
samplerShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex: texture_2d<f32>;"
    , "@group(0) @binding(1)"
    , "var samp: sampler;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = vec2(frag_coord.x / 640.0, frag_coord.y / 480.0);"
    , "  return textureSample(tex, samp, uv);"
    , "}"
    ]

samplerValueShader :: String
samplerValueShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex: texture_2d<f32>;"
    , "@group(0) @binding(1)"
    , "var samp: sampler;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let _ = samp;"
    , "  let uv = vec2(frag_coord.x / 640.0, frag_coord.y / 480.0);"
    , "  let c = textureSample(tex, samp, uv);"
    , "  return c;"
    , "}"
    ]

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

bitwiseShader :: String
bitwiseShader =
  unlines
    [ "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var a = u32(1);"
    , "  var b = u32(3);"
    , "  a = a % u32(2);"
    , "  a = a << u32(1);"
    , "  a = a >> u32(1);"
    , "  a = a & b;"
    , "  a = a | b;"
    , "  a = a ^ b;"
    , "  a += b;"
    , "  a -= b;"
    , "  a *= b;"
    , "  a /= b;"
    , "  a %= b;"
    , "  a &= b;"
    , "  a |= b;"
    , "  a ^= b;"
    , "  a <<= b;"
    , "  a >>= b;"
    , "  a++;"
    , "  --a;"
    , "  var c = i32(4);"
    , "  c = c % i32(3);"
    , "  c <<= i32(1);"
    , "  c >>= i32(1);"
    , "  c++;"
    , "  --c;"
    , "  if (gid.x == 0) {"
    , "    a = a + u32(c);"
    , "  }"
    , "}"
    ]

builtinExtraShader :: String
builtinExtraShader =
  unlines
    [ "struct Params {"
    , "  time: f32;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params: Params;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = vec2(frag_coord.x / 640.0, frag_coord.y / 480.0);"
    , "  let p = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);"
    , "  let a = sign(p.x);"
    , "  let ang = radians(params.time);"
    , "  let _t = tan(ang);"
    , "  let _rn = round(p.x * 2.0);"
    , "  let _tr = trunc(p.y * 2.0);"
    , "  let _inv = inverseSqrt(abs(p.x) + 0.25);"
    , "  let _fm = fma(p.x, p.y, 0.5);"
    , "  let _ch = cosh(p.x);"
    , "  let _sh = sinh(p.y);"
    , "  let _at = atan2(p.y, p.x);"
    , "  let _rt = saturate(p.x * 1.5);"
    , "  let b0 = p.x > 0.0;"
    , "  let b1 = p.y > 0.0;"
    , "  let b2 = (p.x + p.y) > 0.0;"
    , "  let cond = vec3(b0, b1, b2);"
    , "  let anyHit = any(cond);"
    , "  let allHit = all(cond);"
    , "  let base = select(vec3(0.1, 0.2, 0.3), vec3(0.9, 0.3, 0.1), cond);"
    , "  let m = mat2x2(vec2(1.0, 0.0), vec2(0.0, 1.0));"
    , "  let _det = determinant(m);"
    , "  let _mt = transpose(m);"
    , "  let _mi = inverse(m);"
    , "  let _cr = cross(vec3(1.0, 0.0, 0.0), vec3(0.0, 1.0, 0.0));"
    , "  let _ff = faceForward(vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, 1.0));"
    , "  let _rf = refract(vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, 1.0), 0.66);"
    , "  let mf = modf(p.x);"
    , "  let fx = frexp(p.x);"
    , "  let _ld = ldexp(mf.fract, fx.exp);"
    , "  let pk = pack4x8unorm(vec4(0.1, 0.2, 0.3, 0.4));"
    , "  let up = unpack4x8unorm(pk);"
    , "  let pk2 = pack2x16float(vec2(0.3, 0.7));"
    , "  let up2 = unpack2x16float(pk2);"
    , "  let _lead = firstLeadingBit(pk);"
    , "  let _trail = firstTrailingBit(pk);"
    , "  let _q = quantizeToF16(p.x);"
    , "  let ix = u32(frag_coord.x);"
    , "  let iy = u32(frag_coord.y);"
    , "  let bits = ix ^ (iy << u32(1));"
    , "  let _cnt = countOneBits(bits);"
    , "  let _clz = countLeadingZeros(bits);"
    , "  let _ctz = countTrailingZeros(bits);"
    , "  let rev = reverseBits(bits);"
    , "  let ext = extractBits(rev, u32(4), u32(6));"
    , "  let ins = insertBits(bits, ext, u32(8), u32(6));"
    , "  let packed = (i32(255) << i32(24)) | (i32(1) << i32(16)) | (i32(2) << i32(8)) | i32(3);"
    , "  let _dotu = dot4U8Packed(u32(packed), u32(packed));"
    , "  let _doti = dot4I8Packed(packed, packed);"
    , "  let _bc = bitcast<u32>(f32(1.0));"
    , "  let mask = f32(ins & u32(255)) / 255.0;"
    , "  let v = abs(vec3(p.x, p.y, a));"
    , "  let c = clamp(v, vec3(0.0, 0.0, 0.0), vec3(1.0, 1.0, 1.0));"
    , "  let gain = select(0.2, 1.0, allHit);"
    , "  let glow = select(0.2, 0.8, anyHit);"
    , "  return vec4((base.x + c.x) * gain + mask * 0.2 + up.x + up2.x + _q * 0.0 + _rt * 0.0, base.y + c.y + up.y, base.z + c.z + glow * 0.1, 1.0);"
    , "}"
    ]

runtimeArrayLengthShader :: String
runtimeArrayLengthShader =
  unlines
    [ "struct Data {"
    , "  values: array<u32>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<storage, read_write> data: Data;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let len = arrayLength(data.values);"
    , "  if (gid.x == 0 && len > 0) {"
    , "    data.values[0] = len;"
    , "  }"
    , "}"
    ]

aliasOverrideShader :: String
aliasOverrideShader =
  unlines
    [ "alias Color = vec4<f32>;"
    , "override factor: u32 = 2;"
    , "const_assert(factor == 2);"
    , "struct Params {"
    , "  tint: Color;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params: Params;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) Color {"
    , "  let c = params.tint;"
    , "  return vec4(c.x, c.y, c.z, 1.0);"
    , "}"
    ]

switchLoopShader :: String
switchLoopShader =
  unlines
    [ "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var acc = 0;"
    , "  switch (gid.x) {"
    , "    case 0: { acc = acc + 1; }"
    , "    case 1, 2: { acc = acc + 2; }"
    , "    default: { acc = acc + 3; }"
    , "  }"
    , "  loop {"
    , "    acc = acc + 1;"
    , "    break if (acc > 4);"
    , "    continuing {"
    , "      acc = acc + 1;"
    , "    }"
    , "  }"
    , "}"
    ]

badConstAssertShader :: String
badConstAssertShader =
  unlines
    [ "const_assert(false);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x;"
    , "}"
    ]

constFnShader :: String
constFnShader =
  unlines
    [ "fn add(a: i32, b: i32) -> i32 {"
    , "  let sum = a + b;"
    , "  sum + 0;"
    , "  if (sum > 4) {"
    , "    return sum;"
    , "  }"
    , "  return sum - 1;"
    , "}"
    , "fn pick(n: i32) -> i32 {"
    , "  switch (n) {"
    , "    case 0: { return 1; }"
    , "    case 1: { return 2; }"
    , "    default: { return 3; }"
    , "  }"
    , "  return 3;"
    , "}"
    , "fn greater(a: i32, b: i32) -> bool {"
    , "  if (a > b) {"
    , "    return true;"
    , "  }"
    , "  return false;"
    , "}"
    , "fn make(v: f32) -> vec2<f32> {"
    , "  return vec2(v, v + 1.0);"
    , "}"
    , "fn sum(n: i32) -> i32 {"
    , "  var acc = 0;"
    , "  var i = 0;"
    , "  while (i < n) {"
    , "    acc = acc + i;"
    , "    i++;"
    , "  }"
    , "  return acc;"
    , "}"
    , "fn sum_for(n: i32) -> i32 {"
    , "  var acc = 0;"
    , "  for (var i = 0; i < n; i = i + 1) {"
    , "    acc += i;"
    , "  }"
    , "  return acc;"
    , "}"
    , "const_assert(add(2, 3) == 5);"
    , "const_assert(add(1, 1) == 1);"
    , "const_assert(pick(1) == 2);"
    , "const_assert(pick(3) == 3);"
    , "const_assert(greater(4, 1));"
    , "const_assert(make(1.5).y == 2.5);"
    , "const_assert(sum(4) == 6);"
    , "const_assert(sum_for(4) == 6);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x;"
    , "}"
    ]

overrideShader :: String
overrideShader =
  unlines
    [ "override scale: i32;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x + u32(scale);"
    , "}"
    ]

overrideSpecShader :: String
overrideSpecShader =
  unlines
    [ "override scale: i32;"
    , "const_assert(scale == 4);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x + u32(scale);"
    , "}"
    ]

overrideDefaultShader :: String
overrideDefaultShader =
  unlines
    [ "override scale: i32 = 2 + 3;"
    , "@id(7) override mode: u32 = 1;"
    , "const_assert(scale == 5);"
    , "const_assert(mode == 1);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x + u32(scale) + mode;"
    , "}"
    ]

overrideSpecOpShader :: String
overrideSpecOpShader =
  unlines
    [ "override base: i32 = 2;"
    , "override scale: i32 = base * 3 + 1;"
    , "const_assert(scale == 7);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x + u32(scale);"
    , "}"
    ]

diagnosticShader :: String
diagnosticShader =
  unlines
    [ "diagnostic(off, const_assert);"
    , "const_assert(false);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x;"
    , "}"
    ]

diagnosticWarnShader :: String
diagnosticWarnShader =
  unlines
    [ "diagnostic(warning, const_assert);"
    , "const_assert(false);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x;"
    , "}"
    ]

diagnosticUnreachableShader :: String
diagnosticUnreachableShader =
  unlines
    [ "diagnostic(warning, unreachable_code);"
    , "fn helper() {"
    , "  return;"
    , "  let _ = 1;"
    , "}"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  discard;"
    , "  let _ = 0.5;"
    , "  return vec4(1.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

diagnosticUnusedExprShader :: String
diagnosticUnusedExprShader =
  unlines
    [ "diagnostic(warning, unused_expression);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  gid.x;"
    , "  let _ = gid.x;"
    , "}"
    ]

diagnosticUnusedVarShader :: String
diagnosticUnusedVarShader =
  unlines
    [ "diagnostic(warning, unused_variable);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let unused = gid.x;"
    , "  var unused2 = gid.x;"
    , "  let _keep = unused2;"
    , "  let _ = gid.x;"
    , "}"
    ]

diagnosticUnusedParamShader :: String
diagnosticUnusedParamShader =
  unlines
    [ "diagnostic(warning, unused_parameter);"
    , "fn helper(a: i32, b: i32) -> i32 {"
    , "  return a;"
    , "}"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = helper(i32(gid.x), 2);"
    , "}"
    ]

diagnosticShadowingShader :: String
diagnosticShadowingShader =
  unlines
    [ "diagnostic(warning, shadowing);"
    , "fn helper(base: i32) -> i32 {"
    , "  var out = base;"
    , "  if (out > 0) {"
    , "    let base = out + 1;"
    , "    out = base;"
    , "  }"
    , "  return out;"
    , "}"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = helper(i32(gid.x));"
    , "}"
    ]

diagnosticConstantCondShader :: String
diagnosticConstantCondShader =
  unlines
    [ "diagnostic(warning, constant_condition);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  if (true) {"
    , "    let _ = gid.x;"
    , "  }"
    , "  while (false) {"
    , "    let _ = gid.x;"
    , "  }"
    , "  for (; false; ) {"
    , "    let _ = gid.x;"
    , "  }"
    , "}"
    ]

diagnosticDuplicateCaseShader :: String
diagnosticDuplicateCaseShader =
  unlines
    [ "diagnostic(warning, duplicate_case);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var acc = 0;"
    , "  switch (gid.x) {"
    , "    case 1: { acc = acc + 1; }"
    , "    case 1, 2: { acc = acc + 2; }"
    , "    default: { acc = acc + 3; }"
    , "  }"
    , "  let _ = acc;"
    , "}"
    ]

switchFallthroughShader :: String
switchFallthroughShader =
  unlines
    [ "let MODE = 1;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var acc = 0;"
    , "  switch (gid.x) {"
    , "    case 0: { acc = acc + 1; fallthrough; }"
    , "    case MODE: { acc = acc + 2; }"
    , "    default: { acc = acc + 3; }"
    , "  }"
    , "}"
    ]

constArithShader :: String
constArithShader =
  unlines
    [ "const_assert((1 + 2 * 3) == 7);"
    , "const_assert(((8 >> 1) + (3 << 1)) == 10);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var acc = 0;"
    , "  switch (gid.x) {"
    , "    case 1 + 2: { acc = acc + 1; }"
    , "    case (8 >> 1): { acc = acc + 2; }"
    , "    default: { acc = acc + 3; }"
    , "  }"
    , "  if (acc > 4) {"
    , "    acc = acc - 1;"
    , "  }"
    , "}"
    ]

constFloatShader :: String
constFloatShader =
  unlines
    [ "const_assert((0.5 + 0.25) == 0.75);"
    , "const_assert(abs(-1.5) == 1.5);"
    , "const_assert(min(0.5, 0.25) == 0.25);"
    , "const_assert(max(0.5, 0.25) == 0.5);"
    , "const_assert(clamp(2.0, 0.0, 1.0) == 1.0);"
    , "const_assert(mix(0.0, 2.0, 0.25) == 0.5);"
    , "const_assert(select(0.0, 1.0, true) == 1.0);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main() {"
    , "}"
    ]

constCompositeShader :: String
constCompositeShader =
  unlines
    [ "struct Pair {"
    , "  a: i32;"
    , "  b: vec2<f32>;"
    , "};"
    , "const_assert(vec2(1.0, 2.0).x == 1.0);"
    , "const_assert(vec3(1.0, 2.0, 3.0).yz.x == 2.0);"
    , "const_assert(mat2x2(vec2(1.0, 2.0), vec2(3.0, 4.0))[1].y == 4.0);"
    , "const_assert(array(1, 2, 3)[2] == 3);"
    , "const_assert(Pair(7, vec2(0.5, 1.5)).b.y == 1.5);"
    , "const_assert(vec2<f32>().x == 0.0);"
    , "const_assert(mat2x2<f32>()[1].y == 0.0);"
    , "const_assert(array<i32, 2>()[1] == 0);"
    , "const_assert(Pair().a == 0);"
    , "const_assert(i32() == 0);"
    , "const_assert(f32() == 0.0);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main() {"
    , "}"
    ]

discardShader :: String
discardShader =
  unlines
    [ "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  if (pos.x < 0.0) {"
    , "    discard;"
    , "  }"
    , "  return vec4(1.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

pointerShader :: String
pointerShader =
  unlines
    [ "fn inc(x: i32) -> i32 {"
    , "  return x + 1;"
    , "}"
    , "fn make() -> i32 {"
    , "  return 1;"
    , "}"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main() {"
    , "  var x = i32(0);"
    , "  let px = &x;"
    , "  *px = *px + 1;"
    , "  let y = inc(1);"
    , "  x = x + y;"
    , "  let z = make();"
    , "  x = x + z;"
    , "}"
    ]

textureVariantsShader :: String
textureVariantsShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex_arr: texture_2d_array<f32>;"
    , "@group(0) @binding(1)"
    , "var tex_3d: texture_3d<f32>;"
    , "@group(0) @binding(2)"
    , "var tex_cube: texture_cube<f32>;"
    , "@group(0) @binding(3)"
    , "var samp: sampler;"
    , "@group(0) @binding(4)"
    , "var depth_tex: texture_depth_2d;"
    , "@group(0) @binding(5)"
    , "var samp_cmp: sampler_comparison;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = vec2(frag_coord.x / 640.0, frag_coord.y / 480.0);"
    , "  let layer = i32(1);"
    , "  let col_arr = textureSample(tex_arr, samp, uv, layer);"
    , "  let col3 = textureSample(tex_3d, samp, vec3(uv.x, uv.y, 0.5));"
    , "  let colc = textureSample(tex_cube, samp, vec3(uv.x, uv.y, 1.0));"
    , "  let depth = textureSampleCompare(depth_tex, samp_cmp, uv, 0.5);"
    , "  return vec4(col_arr.x + col3.y + colc.z + depth, col_arr.y, col_arr.z, 1.0);"
    , "}"
    ]

textureLoadShader :: String
textureLoadShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex: texture_2d<f32>;"
    , "@group(0) @binding(1)"
    , "var tex_arr: texture_2d_array<f32>;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let coord = vec2(i32(gid.x), i32(gid.y));"
    , "  let _a = textureLoad(tex, coord, 0);"
    , "  let _b = textureLoad(tex_arr, coord, 1, 0);"
    , "}"
    ]

storageTextureArrayShader :: String
storageTextureArrayShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex_arr: texture_storage_2d_array<rgba8unorm, read_write>;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let coord = vec3(i32(gid.x), i32(gid.y), i32(gid.z));"
    , "  let src = textureLoad(tex_arr, coord);"
    , "  textureStore(tex_arr, coord, vec4(src.x, src.y, src.z, src.w));"
    , "}"
    ]

textureAdvancedShader :: String
textureAdvancedShader =
  unlines
    [ "struct Params {"
    , "  time: f32;"
    , "};"
    , "@group(0) @binding(0) var<uniform> params: Params;"
    , "@group(0) @binding(1) var samp: sampler;"
    , "@group(0) @binding(2) var samp_cmp: sampler_comparison;"
    , "@group(0) @binding(3) var tex1d: texture_1d<f32>;"
    , "@group(0) @binding(4) var tex1d_arr: texture_1d_array<f32>;"
    , "@group(0) @binding(5) var tex_cube_arr: texture_cube_array<f32>;"
    , "@group(0) @binding(6) var tex_msaa: texture_multisampled_2d<f32>;"
    , "@group(0) @binding(7) var tex_depth_cube: texture_depth_cube;"
    , "@group(0) @binding(8) var tex_depth_cube_arr: texture_depth_cube_array;"
    , "@group(0) @binding(9) var tex_depth_msaa: texture_depth_multisampled_2d;"
    , "@group(0) @binding(10) var tex_store_1d: texture_storage_1d<rgba8unorm, read_write>;"
    , "@group(0) @binding(11) var tex_store_3d: texture_storage_3d<rgba8unorm, read_write>;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = vec2(frag_coord.x * 0.01, frag_coord.y * 0.01);"
    , "  let lod = 0.5 + params.time * 0.0;"
    , "  let dim0 = textureDimensions(tex1d);"
    , "  let dim1 = textureDimensions(tex1d_arr);"
    , "  let layers = textureNumLayers(tex1d_arr);"
    , "  let levels = textureNumLevels(tex1d);"
    , "  let samples = textureNumSamples(tex_msaa);"
    , "  let c0 = textureSampleLevel(tex1d, samp, uv.x, lod);"
    , "  let c1 = textureSampleLevel(tex1d_arr, samp, uv.x, 1, lod);"
    , "  let c2 = textureSampleBias(tex1d, samp, uv.x, 0.25);"
    , "  let c3 = textureSampleGrad(tex1d, samp, uv.x, 0.01, 0.01);"
    , "  let g0 = textureGather(tex_cube_arr, samp, vec3(uv.x, uv.y, 1.0), 0);"
    , "  let d0 = textureSampleCompareLevel(tex_depth_cube, samp_cmp, vec3(uv.x, uv.y, 1.0), 0.5, lod);"
    , "  let d1 = textureSampleCompareLevel(tex_depth_cube_arr, samp_cmp, vec3(uv.x, uv.y, 1.0), 0, 0.5, lod);"
    , "  let ms = textureLoad(tex_msaa, vec2(0, 0), 0);"
    , "  let msd = textureLoad(tex_depth_msaa, vec2(0, 0), 0);"
    , "  textureStore(tex_store_1d, 0, vec4(uv.x, uv.y, d0, 1.0));"
    , "  textureStore(tex_store_3d, vec3(0, 0, 0), vec4(uv.y, uv.x, 0.0, 1.0));"
    , "  let dims_acc = f32(dim0 + dim1 + layers + levels + samples);"
    , "  return vec4(c0.x + c1.y + c2.z + c3.w + g0.x + d0 + d1 + ms.x + msd + dims_acc * 0.0, 0.2, 0.3, 1.0);"
    , "}"
    ]

globalsShader :: String
globalsShader =
  unlines
    [ "enable f16;"
    , "var<private> bias: f32 = 0.25;"
    , "var<workgroup> shared: array<u32, 4>;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let v = f16(1.0);"
    , "  let w = f32(v) + bias;"
    , "  if (gid.x == 0) {"
    , "    shared[0] = u32(w);"
    , "  }"
    , "}"
    ]

layoutAttrShader :: String
layoutAttrShader =
  unlines
    [ "struct Stuff {"
    , "  @align(16) a: vec3<f32>,"
    , "  @size(32) b: vec4<f32>,"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> stuff: Stuff;"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let v = stuff.a.x + stuff.b.x;"
    , "  return vec4(v, 0.0, 0.0, 1.0);"
    , "}"
    ]

packUniformShader :: String
packUniformShader =
  unlines
    [ "struct Payload {"
    , "  a: f32;"
    , "  b: vec3<f32>;"
    , "  c: mat3x3<f32>;"
    , "  d: array<vec2<f32>, 2>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> payload: Payload;"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let _ = payload.a + payload.b.x + payload.c[0][0] + payload.d[0].x;"
    , "  return vec4(0.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

packUniformExtendedShader :: String
packUniformExtendedShader =
  unlines
    [ "struct Inner {"
    , "  a: vec2<f32>;"
    , "  b: f32;"
    , "};"
    , "struct Params {"
    , "  m3: mat3x3<f32>;"
    , "  m4: mat4x4<f32>;"
    , "  arr2: array<vec2<f32>, 4>;"
    , "  arr3: array<vec3<f32>, 3>;"
    , "  inner: Inner;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params: Params;"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let _ = params.m3[0][0] + params.m4[0][0] + params.arr2[0].x + params.arr3[0].x + params.inner.a.x + params.inner.b;"
    , "  return vec4(0.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

packUniformExtendedShader2 :: String
packUniformExtendedShader2 =
  unlines
    [ "enable f16;"
    , "struct Inner2 {"
    , "  v: vec3<f32>;"
    , "  w: f32;"
    , "};"
    , "struct Outer2 {"
    , "  inners: array<Inner2, 2>;"
    , "};"
    , "struct Params2 {"
    , "  m34: mat3x4<f32>;"
    , "  m43: mat4x3<f32>;"
    , "  mats: array<mat2x2<f32>, 2>;"
    , "  nested: Outer2;"
    , "  h: f16;"
    , "  hv: vec3<f16>;"
    , "  hv4: vec4<f16>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params2: Params2;"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let _ = params2.m34[0][0] + params2.m43[0][0] + params2.mats[0][0][0] + params2.nested.inners[0].v.x + f32(params2.h) + f32(params2.hv.x) + f32(params2.hv4.x);"
    , "  return vec4(0.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

goldenComputeShader :: String
goldenComputeShader =
  unlines
    [ "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var x = 1;"
    , "  x = x + i32(gid.x);"
    , "}"
    ]

goldenFragmentShader :: String
goldenFragmentShader =
  unlines
    [ "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = pos.xy / vec2(800.0, 600.0);"
    , "  return vec4(uv.x, uv.y, 0.2, 1.0);"
    , "}"
    ]

badSwitchShader :: String
badSwitchShader =
  unlines
    [ "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  switch (gid.x) {"
    , "    case gid.x: { }"
    , "    default: { }"
    , "  }"
    , "}"
    ]

ifShader :: String
ifShader =
  unlines
    [ "@if(FOO) let scale = 2.0;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _tmp = scale;"
    , "}"
    ]
