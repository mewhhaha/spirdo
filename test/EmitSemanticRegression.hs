module EmitSemanticRegression (checks) where

import Control.Exception (finally)
import Control.Monad (forM_, unless, when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word16, Word32)
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, openBinaryTempFile)
import System.Process (readProcessWithExitCode)

import qualified Spirdo.Wesl as Wesl

checks :: [(String, IO ())]
checks =
  [ ("logical operators short-circuit", checkLogicalShortCircuit)
  , ("logical loop conditions preserve structured control flow", checkLogicalLoopConditions)
  , ("pointer parameters remain SSA pointer values", checkPointerParameterEmission)
  , ("pointer-containing type shapes are rejected", checkPointerTypeShapes)
  , ("pointer call aliases follow WGSL restrictions", checkPointerCallAliases)
  , ("pointer return types are rejected", checkPointerReturnTypes)
  , ("single entry honors requested name", checkSingleEntrySelection)
  , ("negative i32 constants emit", checkNegativeI32Constants)
  , ("finite float-to-integer boundary casts emit", checkFloatIntegerCastBoundaries)
  , ("invalid float-to-integer casts are rejected", checkInvalidFloatIntegerCasts)
  , ("non-finite float constants are rejected", checkFloatConstantOverflow)
  , ("abstract floats retain binary64 precision", checkAbstractFloatPrecision)
  , ("float literals materialize with ties-to-even", checkFloatLiteralMaterialization)
  , ("contextual floats materialize in their target type", checkContextualFloatMaterialization)
  , ("contextual float range is checked before rounding", checkContextualFloatRange)
  , ("near-limit array layout emits", checkNearLimitArrayLayout)
  , ("array layout overflow is rejected", checkArrayLayoutOverflow)
  , ("oversized array composites are rejected", checkArrayCompositeLimit)
  , ("layout round-up overflow is rejected", checkLayoutRoundUpOverflow)
  , ("struct layout overflow is rejected", checkStructLayoutOverflow)
  , ("nested uniform layout overflow is rejected", checkNestedUniformLayoutOverflow)
  , ("supported SPIR-V version endpoints emit", checkSupportedSpirvVersions)
  , ("unsupported SPIR-V versions are rejected", checkUnsupportedSpirvVersions)
  , ("structured scopes restore outer bindings", checkStructuredScopeBindings)
  , ("continuing break if emits the loop back-edge branch", checkContinuingBreakIf)
  , ("override workgroup size uses OpExecutionModeId", checkWorkgroupSizeExecutionMode)
  , ("derived overrides use Shader specialization operations", checkDerivedOverrideOperations)
  , ("strict composite overrides omit specialization IDs", checkStrictCompositeOverrideIds)
  , ("defaultless workgroup size overrides remain specializable", checkDefaultlessWorkgroupOverride)
  , ("workgroup size parameters use one integer type", checkWorkgroupSizeParameterTypes)
  , ("combined samplers still check sampler types", checkCombinedSamplerType)
  ]

checkLogicalShortCircuit :: IO ()
checkLogicalShortCircuit = do
  bytes <- compileBytes [] logicalShortCircuitSource
  instructions <- decodeSpirv bytes
  let opcodes = map fst instructions
  when (166 `elem` opcodes || 167 `elem` opcodes) $
    fail "logical short-circuit emitted eager OpLogicalOr/OpLogicalAnd"
  unless (length (filter (== 247) opcodes) >= 2) $
    fail "logical short-circuit did not emit structured selections"
  let branchTargets =
        Set.fromList
          [ target
          | (250, _condition : targets) <- instructions
          , target <- targets
          ]
  let callLabels = functionCallLabels instructions
  unless (length callLabels == 2) $
    fail ("expected two RHS function calls, got " <> show (length callLabels))
  unless (all (`Set.member` branchTargets) callLabels) $
    fail "logical RHS function call was not isolated in a conditional branch"
  validateSpirv "logical-short-circuit" bytes

checkLogicalLoopConditions :: IO ()
checkLogicalLoopConditions = do
  bytes <- compileBytes [] logicalLoopConditionsSource
  instructions <- decodeSpirv bytes
  unless (length (filter ((== 246) . fst) instructions) == 4) $
    fail "logical loop conditions did not emit all four structured loop headers"
  validateSpirv "logical-loop-conditions" bytes
  validateSpirvWithOptions "logical-loop-conditions-vulkan" ["--target-env", "vulkan1.3"] bytes

checkPointerParameterEmission :: IO ()
checkPointerParameterEmission = do
  bytes <- compileBytes [] functionPointerParameterSource
  validateSpirv "function-pointer-parameter" bytes
  validateSpirvWithOptions "function-pointer-parameter-vulkan" ["--target-env", "vulkan1.3"] bytes
  privateBytes <- compileBytes [] privatePointerParameterSource
  validateSpirv "private-pointer-parameter" privateBytes
  validateSpirvWithOptions "private-pointer-parameter-vulkan" ["--target-env", "vulkan1.3"] privateBytes
  unrestrictedBytes <-
    compileBytes
      [Wesl.OptEnableFeature "unrestricted_pointer_parameters"]
      storagePointerParameterSource
  validateSpirv "storage-pointer-parameter" unrestrictedBytes
  validateSpirvWithOptions "storage-pointer-parameter-vulkan" ["--target-env", "vulkan1.3"] unrestrictedBytes
  runtimeArrayBytes <-
    compileBytes
      [Wesl.OptEnableFeature "unrestricted_pointer_parameters"]
      runtimeArrayPointerParameterSource
  runtimeArrayInstructions <- decodeSpirv runtimeArrayBytes
  unless (length (filter ((== 68) . fst) runtimeArrayInstructions) == 2) $
    fail "runtime-array pointer parameters did not derive both hidden lengths from their containing structs"
  validateSpirv "runtime-array-pointer-parameter" runtimeArrayBytes
  validateSpirvWithOptions "runtime-array-pointer-parameter-vulkan" ["--target-env", "vulkan1.3"] runtimeArrayBytes
  workgroupBytes <-
    compileBytes
      [Wesl.OptEnableFeature "unrestricted_pointer_parameters"]
      workgroupPointerParameterSource
  validateSpirv "workgroup-pointer-parameter" workgroupBytes
  validateSpirvWithOptions "workgroup-pointer-parameter-vulkan" ["--target-env", "vulkan1.3"] workgroupBytes
  expectCompileErrorWith
    [Wesl.OptEnableFeature "unrestricted_pointer_parameters"]
    ["function pointer parameters cannot use ptr<uniform,...> with SPIR-V Logical addressing"]
    uniformPointerParameterSource
  legacyBytes <-
    compileBytes
      [ Wesl.OptEnableFeature "unrestricted_pointer_parameters"
      , Wesl.OptSpirvVersion 0x00010000
      ]
      storagePointerParameterSource
  validateSpirv "storage-pointer-parameter-spirv10" legacyBytes
  validateSpirvWithOptions "storage-pointer-parameter-vulkan10" ["--target-env", "vulkan1.0"] legacyBytes

checkPointerReturnTypes :: IO ()
checkPointerReturnTypes = do
  expectCompileError ["function return types cannot be pointers"] pointerReturnSource
  expectCompileError ["entry point return types cannot be pointers"] entryPointerReturnSource

checkPointerTypeShapes :: IO ()
checkPointerTypeShapes = do
  bytes <- compileBytes [] pointerAliasPointeeSource
  validateSpirv "pointer-alias-pointee" bytes
  expectCompileError
    ["parameter of function invalid pointer store type cannot contain pointers"]
    pointerToPointerSource
  expectCompileError
    ["parameter of function invalid cannot contain pointer types"]
    arrayOfPointersSource
  expectCompileError
    ["arrays of pointer types are not supported"]
    inferredArrayOfPointersSource
  expectCompileError ["type alias cycle"] pointerAliasCycleSource

checkPointerCallAliases :: IO ()
checkPointerCallAliases = do
  expectCompileError
    ["pointer argument 1 to read_element", "must point to a whole variable root"]
    partialPointerArgumentSource
  expectCompileErrorWith
    [Wesl.OptEnableFeature "unrestricted_pointer_parameters"]
    ["pointer argument 1 to read_element", "must point to a whole variable root"]
    partialPointerArgumentSource
  expectCompileErrorWith
    [Wesl.OptEnableFeature "unrestricted_pointer_parameters"]
    ["unsupported by SPIR-V Logical addressing", "require storage or workgroup"]
    unrestrictedPartialPointerArgumentSource
  expectCompileError
    ["aliased pointer arguments 1 and 2 to mutate", "either parameter may write"]
    aliasedWritePointerArgumentsSource
  readOnlyBytes <- compileBytes [] aliasedReadOnlyPointerArgumentsSource
  validateSpirv "aliased-read-only-pointer-arguments" readOnlyBytes
  overloadedBytes <- compileBytes [] exactPointerOverloadSource
  validateSpirv "exact-pointer-overload" overloadedBytes
  expectCompileError
    ["aliased pointer arguments 1 and 2 to forward", "either parameter may write"]
    transitiveAliasedWriteSource
  expectCompileError
    ["pointer argument 1 to f6", "aliases module-scope variable value"]
    moduleGlobalPointerAliasSource
  expectCompileErrorWith
    [Wesl.OptEnableFeature "unrestricted_pointer_parameters"]
    ["pointer argument 1 to inspect", "aliases module-scope variable values"]
    runtimeArrayLengthGlobalAliasSource

checkSingleEntrySelection :: IO ()
checkSingleEntrySelection = do
  result <- Wesl.compile [Wesl.OptEntryPoint "missing"] (Wesl.sourceText singleEntrySource)
  case result of
    Left err ->
      unless ("entry point not found: missing" `isInfixOf` Wesl.renderCompileError err) $
        fail ("unexpected entry-point error: " <> Wesl.renderCompileError err)
    Right _ -> fail "a missing requested entry point selected the module's only entry"

checkNegativeI32Constants :: IO ()
checkNegativeI32Constants = do
  bytes <- compileBytes [] negativeI32ConstantSource
  instructions <- decodeSpirv bytes
  let constantWords =
        [ literal
        | (43, [_typeId, _resultId, literal]) <- instructions
        ]
  unless (0xffffffff `elem` constantWords) $
    fail "constant arithmetic result -1 was not emitted as i32 two's complement"
  unless (0x80000000 `elem` constantWords) $
    fail "i32 minimum constant was not emitted"
  validateSpirv "negative-i32-constants" bytes

checkFloatIntegerCastBoundaries :: IO ()
checkFloatIntegerCastBoundaries = do
  bytes <- compileBytes [Wesl.OptEnableFeature "f16"] floatIntegerCastBoundariesSource
  validateSpirv "float-integer-cast-boundaries" bytes

checkInvalidFloatIntegerCasts :: IO ()
checkInvalidFloatIntegerCasts =
  forM_ invalidFloatIntegerCastCases $ \(options, expectedFragments, source) ->
    expectCompileErrorWith options expectedFragments source

checkFloatConstantOverflow :: IO ()
checkFloatConstantOverflow = do
  let options = [Wesl.OptEnableFeature "f16"]
  expectCompileError ["f32 literal overflows its target type"] f32ConstantOverflowSource
  expectCompileErrorWith options ["constant f16 conversion is out of range"] f16ConstantOverflowSource
  expectCompileErrorWith options ["constant f16 conversion is out of range"] f16OverrideOverflowSource

checkAbstractFloatPrecision :: IO ()
checkAbstractFloatPrecision = do
  bytes <- compileBytes [] abstractFloatPrecisionSource
  validateSpirv "abstract-float-precision" bytes
  expectCompileError ["float operation", "finite value"] abstractFloatOperationOverflowSource
  expectCompileError ["constant f32 conversion is out of range"] contextualF32OverflowSource
  expectCompileErrorWith
    [Wesl.OptEnableFeature "f16"]
    ["constant f16 conversion is out of range"]
    contextualF16OverflowSource
  expectCompileErrorWith
    [Wesl.OptEnableFeature "f16"]
    ["constant f16 conversion is out of range"]
    constructorF16OverflowSource

checkFloatLiteralMaterialization :: IO ()
checkFloatLiteralMaterialization = do
  bytes <- compileBytes [Wesl.OptEnableFeature "f16"] floatLiteralMaterializationSource
  instructions <- decodeSpirv bytes
  let floatWidths =
        Map.fromList
          [ (resultId, width)
          | (22, [resultId, width]) <- instructions
          ]
      literals =
        [ (width, literal)
        | (opcode, [typeId, _resultId, literal]) <- instructions
        , opcode == 43 || opcode == 50
        , Just width <- [Map.lookup typeId floatWidths]
        ]
      expectLiteral width literal description =
        unless ((width, literal) `elem` literals) $
          fail (description <> " was not emitted; float literals were " <> show literals)
  expectLiteral 32 0x3f800000 "f32 midpoint tie-to-even"
  expectLiteral 32 0x3f800001 "f32 just-above-midpoint rounding"
  expectLiteral 32 0x7f7fffff "maximum finite f32"
  expectLiteral 16 0x3c00 "f16 midpoint tie-to-even"
  expectLiteral 16 0x3c01 "f16 direct binary64 just-above-midpoint rounding"
  expectLiteral 16 0x0000 "f16 subnormal midpoint tie-to-even"
  expectLiteral 16 0x0001 "f16 subnormal just-above-midpoint rounding"
  expectLiteral 16 0x8000 "f16 signed zero"
  validateSpirv "float-literal-materialization" bytes
  specBytes <- compileBytes [Wesl.OptEnableFeature "f16"] floatSpecLiteralMaterializationSource
  specInstructions <- decodeSpirv specBytes
  let specFloatWidths =
        Map.fromList
          [ (resultId, width)
          | (22, [resultId, width]) <- specInstructions
          ]
      specLiterals =
        [ (width, literal)
        | (50, [typeId, _resultId, literal]) <- specInstructions
        , Just width <- [Map.lookup typeId specFloatWidths]
        ]
  unless ((32, 0x3f800001) `elem` specLiterals) $
    fail ("f32 specialization literal lost just-above-midpoint rounding: " <> show specLiterals)
  unless ((16, 0x3c01) `elem` specLiterals) $
    fail ("f16 specialization literal was not rounded directly from binary64: " <> show specLiterals)
  validateSpirv "float-spec-literal-materialization" specBytes

checkContextualFloatMaterialization :: IO ()
checkContextualFloatMaterialization = do
  bytes <- compileBytes [Wesl.OptEnableFeature "f16"] contextualFloatMaterializationSource
  instructions <- decodeSpirv bytes
  let floatWidths =
        Map.fromList
          [ (resultId, width)
          | (22, [resultId, width]) <- instructions
          ]
      literals =
        [ (width, literal)
        | (opcode, [typeId, _resultId, literal]) <- instructions
        , opcode == 43 || opcode == 50
        , Just width <- [Map.lookup typeId floatWidths]
        ]
  unless ((16, 0x3c01) `elem` literals) $
    fail ("contextual f16 literal was not rounded directly from binary64: " <> show literals)
  unless ((16, 0x8000) `elem` literals) $
    fail ("contextual f16 negative zero lost its sign: " <> show literals)
  unless ((32, 0x80000000) `elem` literals) $
    fail ("contextual f32 negative zero lost its sign: " <> show literals)
  validateSpirv "contextual-float-materialization" bytes
  entryBytes <- compileBytes [Wesl.OptEnableFeature "f16"] contextualF16EntryReturnSource
  validateSpirv "contextual-f16-entry-return" entryBytes
  validateSpirvWithOptions "contextual-f16-entry-return-vulkan" ["--target-env", "vulkan1.3"] entryBytes

checkContextualFloatRange :: IO ()
checkContextualFloatRange = do
  maxBytes <- compileBytes [Wesl.OptEnableFeature "f16"] contextualFloatMaximumSource
  validateSpirv "contextual-float-maximum" maxBytes
  expectCompileErrorWith
    [Wesl.OptEnableFeature "f16"]
    ["constant f16 conversion is out of range"]
    contextualF16AboveMaximumSource
  expectCompileError
    ["constant f32 conversion is out of range"]
    contextualF32AboveMaximumSource

checkNearLimitArrayLayout :: IO ()
checkNearLimitArrayLayout = do
  bytes <- compileBytes [] nearLimitArrayLayoutSource
  validateSpirv "near-limit-array-layout" bytes

checkArrayLayoutOverflow :: IO ()
checkArrayLayoutOverflow =
  expectCompileError ["array layout size exceeds Word32", "4 * 1073741824 = 4294967296"] arrayLayoutOverflowSource

checkArrayCompositeLimit :: IO ()
checkArrayCompositeLimit = do
  bytes <- compileBytes [] arrayCompositeBoundarySource
  validateSpirv "array-composite-boundary" bytes
  expectCompileError
    ["array constructor length 65533 exceeds SPIR-V composite limit 65532"]
    arrayCompositeLimitSource
  expectCompileError
    ["override default array length 65533 exceeds SPIR-V composite limit 65532"]
    overrideArrayCompositeLimitSource

checkLayoutRoundUpOverflow :: IO ()
checkLayoutRoundUpOverflow =
  expectCompileError
    [ "struct field c offset exceeds Word32"
    , "2147483652 rounded to alignment 2147483648 = 4294967296"
    ]
    layoutRoundUpOverflowSource

checkStructLayoutOverflow :: IO ()
checkStructLayoutOverflow =
  expectCompileError ["struct field b end exceeds Word32", "4294967292 + 4 = 4294967296"] structLayoutOverflowSource

checkNestedUniformLayoutOverflow :: IO ()
checkNestedUniformLayoutOverflow =
  expectCompileError
    ["struct field tail end exceeds Word32", "4294967292 + 4 = 4294967296"]
    nestedUniformLayoutOverflowSource

checkSupportedSpirvVersions :: IO ()
checkSupportedSpirvVersions =
  forM_ [("spirv-1.0", 0x00010000), ("spirv-1.6", 0x00010600)] $ \(label, version) -> do
    bytes <- compileBytes [Wesl.OptSpirvVersion version] singleEntrySource
    unless (word32At bytes 4 == version) $
      fail (label <> ": emitted header version " <> show (word32At bytes 4) <> ", expected " <> show version)
    validateSpirv label bytes

checkUnsupportedSpirvVersions :: IO ()
checkUnsupportedSpirvVersions =
  forM_ [0x00000000, 0x00010601, 0x00010700, 0x00020000] $ \version ->
    expectCompileErrorWith
      [Wesl.OptSpirvVersion version]
      ["unsupported SPIR-V version word " <> show version, "1.0 through 1.6"]
      singleEntrySource

checkStructuredScopeBindings :: IO ()
checkStructuredScopeBindings = do
  bytes <- compileBytes [] structuredScopeSource
  validateSpirv "structured-scope-bindings" bytes

checkContinuingBreakIf :: IO ()
checkContinuingBreakIf = do
  bytes <- compileBytes [] continuingBreakIfSource
  instructions <- decodeSpirv bytes
  when (any ((== 247) . fst) instructions) $
    fail "continuing break if emitted a nested OpSelectionMerge"
  validateSpirv "continuing-break-if" bytes

checkWorkgroupSizeExecutionMode :: IO ()
checkWorkgroupSizeExecutionMode = do
  expectCompileErrorWith
    [Wesl.OptSpirvVersion 0x00010000]
    ["override-dependent workgroup_size requires SPIR-V 1.2 or newer"]
    overrideWorkgroupSizeSource
  bytes <- compileBytes [] overrideWorkgroupSizeSource
  instructions <- decodeSpirv bytes
  unless (any isLocalSizeId instructions) $
    fail "override workgroup size did not emit OpExecutionModeId LocalSizeId"
  when (any isInvalidLocalSize instructions) $
    fail "override workgroup size emitted LocalSizeId through OpExecutionMode"
  validateSpirv "override-workgroup-size" bytes
  validateSpirvWithOptions "override-workgroup-size-vulkan" ["--target-env", "vulkan1.3"] bytes
  specializedBytes <-
    compileBytes
      [ Wesl.OptOverrides
          [ ("x", Wesl.OVU32 2)
          , ("y", Wesl.OVU32 3)
          , ("z", Wesl.OVU32 4)
          ]
      ]
      overrideWorkgroupSizeSource
  specializedInstructions <- decodeSpirv specializedBytes
  unless (any isSpecializedLocalSize specializedInstructions) $
    fail "host-specialized workgroup size did not emit literal OpExecutionMode LocalSize"
  when (any isLocalSizeId specializedInstructions) $
    fail "host-specialized workgroup size still emitted OpExecutionModeId LocalSizeId"
  validateSpirv "specialized-workgroup-size" specializedBytes

  oneDimensionalBytes <- compileBytes [] oneDimensionalWorkgroupSizeSource
  oneDimensionalInstructions <- decodeSpirv oneDimensionalBytes
  when (any isSpecConstantBitcast oneDimensionalInstructions) $
    fail "one-dimensional workgroup size padded dimensions through OpSpecConstantOp Bitcast"
  unless (length (filter ((== 50) . fst) oneDimensionalInstructions) >= 3) $
    fail "one-dimensional workgroup size did not use direct specialization constants for omitted dimensions"
  validateSpirv "one-dimensional-workgroup-size" oneDimensionalBytes
  validateSpirvWithOptions "one-dimensional-workgroup-size-vulkan" ["--target-env", "vulkan1.3"] oneDimensionalBytes
  where
    isLocalSizeId (331, [_entryId, 38, _xId, _yId, _zId]) = True
    isLocalSizeId _ = False
    isInvalidLocalSize (16, _entryId : 38 : _) = True
    isInvalidLocalSize _ = False
    isSpecializedLocalSize (16, [_entryId, 17, 2, 3, 4]) = True
    isSpecializedLocalSize _ = False
    isSpecConstantBitcast (52, _typeId : _resultId : 124 : _) = True
    isSpecConstantBitcast _ = False

checkDerivedOverrideOperations :: IO ()
checkDerivedOverrideOperations = do
  bytes <- compileBytes [] derivedIntegerOverrideSource
  validateSpirv "derived-integer-override" bytes
  validateSpirvWithOptions "derived-integer-override-vulkan" ["--target-env", "vulkan1.3"] bytes
  abstractIntegerBytes <- compileBytes [] derivedAbstractIntegerOverrideSource
  validateSpirv "derived-abstract-integer-override" abstractIntegerBytes
  validateSpirvWithOptions "derived-abstract-integer-override-vulkan" ["--target-env", "vulkan1.3"] abstractIntegerBytes
  selectBytes <- compileBytes [] derivedSelectOverrideSource
  selectInstructions <- decodeSpirv selectBytes
  let fourIds = [resultId | (50, [_typeId, resultId, 4]) <- selectInstructions]
      eightIds = [resultId | (50, [_typeId, resultId, 8]) <- selectInstructions]
      selectOperands =
        [ (trueId, falseId)
        | (52, [_typeId, _resultId, 169, _conditionId, trueId, falseId]) <- selectInstructions
        ]
  unless
    ( or
        [ trueId `elem` eightIds && falseId `elem` fourIds
        | (trueId, falseId) <- selectOperands
        ]
    ) $
    fail "derived select override did not preserve WGSL true/false operand order"
  validateSpirv "derived-select-override" selectBytes
  validateSpirvWithOptions "derived-select-override-vulkan" ["--target-env", "vulkan1.3"] selectBytes
  expectCompileError
    ["SPIR-V Shader specialization constants do not support opcode 129"]
    derivedFloatOverrideSource
  expectCompileError
    ["derived override conversion from I32 to U32", "not supported for SPIR-V Shader specialization constants"]
    derivedConversionOverrideSource
  vectorSelectBytes <-
    compileBytes
      [Wesl.OptSpirvVersion 0x00010000]
      vectorSelectOverrideSource
  validateSpirv "vector-select-override-spirv10" vectorSelectBytes
  validateSpirvWithOptions "vector-select-override-vulkan10" ["--target-env", "vulkan1.0"] vectorSelectBytes
  expectCompileError
    ["select specialization constant result must be a scalar or vector"]
    matrixSelectOverrideSource

checkStrictCompositeOverrideIds :: IO ()
checkStrictCompositeOverrideIds = do
  result <- Wesl.compile [] (Wesl.sourceText strictCompositeOverrideSource)
  shader <-
    case result of
      Left err -> fail (Wesl.renderCompileError err)
      Right compiled -> pure compiled
  unless (all hasNoSpecId (Wesl.shaderOverrides shader)) $
    fail ("strict composite override reflection retained specialization IDs: " <> show (Wesl.shaderOverrides shader))
  let bytes = Wesl.shaderSpirv shader
  instructions <- decodeSpirv bytes
  when (any isSpecIdDecoration instructions) $
    fail "strict composite override SPIR-V retained SpecId decorations"
  validateSpirv "strict-composite-overrides" bytes
  where
    hasNoSpecId (Wesl.OverrideLayout _ Nothing) = True
    hasNoSpecId _ = False
    isSpecIdDecoration (71, [_target, 1, _specId]) = True
    isSpecIdDecoration _ = False

checkDefaultlessWorkgroupOverride :: IO ()
checkDefaultlessWorkgroupOverride = do
  expectCompileErrorWith
    [Wesl.OptSpirvVersion 0x00010000]
    ["override-dependent workgroup_size requires SPIR-V 1.2 or newer"]
    defaultlessWorkgroupOverrideSource
  bytes <- compileBytes [] defaultlessWorkgroupOverrideSource
  instructions <- decodeSpirv bytes
  case [(xId, yId, zId) | (331, [_entryId, 38, xId, yId, zId]) <- instructions] of
    [(xId, yId, zId)] -> do
      let zeroSpecConstants = [resultId | (50, [_typeId, resultId, 0]) <- instructions]
          oneSpecConstants = [resultId | (50, [_typeId, resultId, 1]) <- instructions]
          specIdSevenTargets = [targetId | (71, [targetId, 1, 7]) <- instructions]
      unless (xId `elem` zeroSpecConstants && xId `elem` specIdSevenTargets) $
        fail "defaultless workgroup override did not emit a zero OpSpecConstant carrying SpecId 7"
      unless (yId `elem` oneSpecConstants && zId `elem` oneSpecConstants) $
        fail "defaultless one-dimensional workgroup size did not pad omitted dimensions with specialization constants 1"
    localSizes ->
      fail ("defaultless workgroup override emitted unexpected LocalSizeId instructions: " <> show localSizes)
  validateSpirv "defaultless-workgroup-override" bytes
  validateSpirvWithOptions "defaultless-workgroup-override-vulkan" ["--target-env", "vulkan1.3"] bytes

  mixedAbstractBytes <- compileBytes [] defaultlessWorkgroupWithAbstractDimensionSource
  validateSpirv "defaultless-workgroup-abstract-dimension" mixedAbstractBytes
  validateSpirvWithOptions "defaultless-workgroup-abstract-dimension-vulkan" ["--target-env", "vulkan1.3"] mixedAbstractBytes

  expressionBytes <- compileBytes [] defaultlessWorkgroupExpressionSource
  validateSpirv "defaultless-workgroup-expression" expressionBytes
  validateSpirvWithOptions "defaultless-workgroup-expression-vulkan" ["--target-env", "vulkan1.3"] expressionBytes

  specializedExpressionBytes <-
    compileBytes
      [Wesl.OptOverrides [("x", Wesl.OVU32 2)]]
      defaultlessWorkgroupExpressionSource
  specializedExpressionInstructions <- decodeSpirv specializedExpressionBytes
  unless (any isUnitLocalSize specializedExpressionInstructions) $
    fail "host-specialized workgroup expression did not emit literal LocalSize 1 1 1"

  specializedBytes <-
    compileBytes
      [Wesl.OptOverrides [("x", Wesl.OVU32 4)]]
      defaultlessWorkgroupOverrideSource
  specializedInstructions <- decodeSpirv specializedBytes
  unless (any isSpecializedLocalSize specializedInstructions) $
    fail "host-specialized defaultless workgroup override did not emit literal LocalSize"
  when (any isLocalSizeId specializedInstructions) $
    fail "host-specialized defaultless workgroup override still emitted LocalSizeId"
  validateSpirv "host-provided-workgroup-override" specializedBytes
  validateSpirvWithOptions "host-provided-workgroup-override-vulkan" ["--target-env", "vulkan1.3"] specializedBytes

  expectCompileErrorWith
    [Wesl.OptOverrides [("x", Wesl.OVU32 0)]]
    ["@workgroup_size values must be positive"]
    defaultlessWorkgroupOverrideSource
  expectCompileErrorWith
    [Wesl.OptOverrides [("x", Wesl.OVI32 (-1))]]
    ["@workgroup_size values must be positive"]
    defaultlessI32WorkgroupOverrideSource
  expectCompileErrorWith
    [Wesl.OptOverrides [("x", Wesl.OVU32 (fromIntegral (maxBound :: Word32) + 1))]]
    ["integer literal is out of range"]
    defaultlessWorkgroupOverrideSource
  expectCompileError
    ["@workgroup_size values must be positive"]
    defaultlessWorkgroupWithKnownZeroSource
  where
    isLocalSizeId (331, [_entryId, 38, _xId, _yId, _zId]) = True
    isLocalSizeId _ = False
    isSpecializedLocalSize (16, [_entryId, 17, 4, 1, 1]) = True
    isSpecializedLocalSize _ = False
    isUnitLocalSize (16, [_entryId, 17, 1, 1, 1]) = True
    isUnitLocalSize _ = False

checkWorkgroupSizeParameterTypes :: IO ()
checkWorkgroupSizeParameterTypes = do
  expectCompileError
    ["@workgroup_size values must all have the same i32 or u32 type"]
    mixedWorkgroupSizeTypesSource
  expectCompileError
    ["@workgroup_size values must be i32 or u32 scalars"]
    floatWorkgroupSizeTypeSource

checkCombinedSamplerType :: IO ()
checkCombinedSamplerType =
  expectCompileError ["type mismatch"] combinedSamplerTypeSource

compileBytes :: [Wesl.Option] -> String -> IO BS.ByteString
compileBytes options source = do
  result <- Wesl.compile options (Wesl.sourceText source)
  case result of
    Left err -> fail (Wesl.renderCompileError err)
    Right shader -> pure (Wesl.shaderSpirv shader)

expectCompileError :: [String] -> String -> IO ()
expectCompileError = expectCompileErrorWith []

expectCompileErrorWith :: [Wesl.Option] -> [String] -> String -> IO ()
expectCompileErrorWith options expectedFragments source = do
  result <- Wesl.compile options (Wesl.sourceText source)
  case result of
    Left err -> do
      let message = Wesl.renderCompileError err
      unless (all (`isInfixOf` message) expectedFragments) $
        fail ("unexpected compile error: " <> message <> "\nsource:\n" <> source)
    Right _ -> fail "expected compilation to fail"

decodeSpirv :: BS.ByteString -> IO [(Word16, [Word32])]
decodeSpirv bytes =
  case go 20 [] of
    Left message -> fail message
    Right instructions -> pure instructions
  where
    byteLength = BS.length bytes

    go offset acc
      | offset == byteLength = Right (reverse acc)
      | offset + 4 > byteLength = Left "truncated SPIR-V instruction header"
      | otherwise =
          let instructionWord = word32At bytes offset
              wordCount = fromIntegral (instructionWord `shiftR` 16)
              opcode = fromIntegral (instructionWord .&. 0xffff)
              nextOffset = offset + wordCount * 4
          in if wordCount == 0 || nextOffset > byteLength
              then Left ("invalid SPIR-V instruction at byte " <> show offset)
              else
                let operands = [word32At bytes (offset + ix * 4) | ix <- [1 .. wordCount - 1]]
                in go nextOffset ((opcode, operands) : acc)

functionCallLabels :: [(Word16, [Word32])] -> [Word32]
functionCallLabels = reverse . snd . foldl step (Nothing, [])
  where
    step (_, acc) (248, [label]) = (Just label, acc)
    step (currentLabel, acc) (57, _operands) =
      case currentLabel of
        Nothing -> (currentLabel, acc)
        Just label -> (currentLabel, label : acc)
    step state _ = state

word32At :: BS.ByteString -> Int -> Word32
word32At bytes offset =
  let byte0 = fromIntegral (BS.index bytes offset)
      byte1 = fromIntegral (BS.index bytes (offset + 1))
      byte2 = fromIntegral (BS.index bytes (offset + 2))
      byte3 = fromIntegral (BS.index bytes (offset + 3))
  in byte0 .|. (byte1 `shiftL` 8) .|. (byte2 `shiftL` 16) .|. (byte3 `shiftL` 24)

validateSpirv :: String -> BS.ByteString -> IO ()
validateSpirv label = validateSpirvWithOptions label []

validateSpirvWithOptions :: String -> [String] -> BS.ByteString -> IO ()
validateSpirvWithOptions label options bytes = do
  validator <- findExecutable "spirv-val"
  case validator of
    Nothing -> pure ()
    Just executable -> do
      tempDirectory <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile tempDirectory (label <> ".spv")
      BS.hPut handle bytes
      hClose handle
      (do
          (exitCode, _stdout, stderr) <- readProcessWithExitCode executable (options <> [path]) ""
          case exitCode of
            ExitSuccess -> pure ()
            ExitFailure _ -> fail (label <> ": spirv-val failed: " <> stderr)
        ) `finally` removeFile path

logicalShortCircuitSource :: String
logicalShortCircuitSource = unlines
  [ "var<private> calls: u32;"
  , "fn side_effect() -> bool {"
  , "  calls = calls + 1u;"
  , "  return true;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  let and_result = false && side_effect();"
  , "  let or_result = true || side_effect();"
  , "}"
  ]

logicalLoopConditionsSource :: String
logicalLoopConditionsSource = unlines
  [ "fn below_limit(value: i32) -> bool { return value < 2; }"
  , "fn exercise_function_loops() {"
  , "  var first = 0;"
  , "  while (first < 2 && below_limit(first)) { first = first + 1; }"
  , "  for (var second = 0; second >= 2 || below_limit(second); second = second + 1) {}"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  exercise_function_loops();"
  , "  var first = 0;"
  , "  while (first < 2 && below_limit(first)) { first = first + 1; }"
  , "  for (var second = 0; second >= 2 || below_limit(second); second = second + 1) {}"
  , "}"
  ]

functionPointerParameterSource :: String
functionPointerParameterSource = unlines
  [ "fn bump(value: ptr<function, i32>) -> i32 {"
  , "  *value = *value + 1;"
  , "  return *value;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  var value = 0;"
  , "  let result = bump(&value);"
  , "}"
  ]

privatePointerParameterSource :: String
privatePointerParameterSource = unlines
  [ "var<private> value: i32;"
  , "fn bump(pointer: ptr<private, i32>) -> i32 {"
  , "  *pointer = *pointer + 1;"
  , "  return *pointer;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() { let result = bump(&value); }"
  ]

storagePointerParameterSource :: String
storagePointerParameterSource = unlines
  [ "enable unrestricted_pointer_parameters;"
  , "struct Values { value: u32, }"
  , "@group(0) @binding(0) var<storage, read_write> values: Values;"
  , "fn bump(value: ptr<storage, u32, read_write>) -> u32 {"
  , "  *value = *value + 1u;"
  , "  return *value;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() { let result = bump(&values.value); }"
  ]

runtimeArrayPointerParameterSource :: String
runtimeArrayPointerParameterSource = unlines
  [ "enable unrestricted_pointer_parameters;"
  , "struct Values { first: u32, values: array<u32>, }"
  , "@group(0) @binding(0) var<storage, read> values: Values;"
  , "@group(0) @binding(1) var<storage, read> direct: array<u32>;"
  , "fn runtime_length(pointer: ptr<storage, array<u32>, read>) -> u32 {"
  , "  return arrayLength(pointer);"
  , "}"
  , "fn forward_length(pointer: ptr<storage, array<u32>, read>) -> u32 {"
  , "  return runtime_length(pointer);"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  let struct_length = forward_length(&values.values);"
  , "  let direct_length = forward_length(&direct);"
  , "}"
  ]

workgroupPointerParameterSource :: String
workgroupPointerParameterSource = unlines
  [ "enable unrestricted_pointer_parameters;"
  , "var<workgroup> value: i32;"
  , "fn bump(pointer: ptr<workgroup, i32>) -> i32 {"
  , "  *pointer = *pointer + 1;"
  , "  return *pointer;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() { let result = bump(&value); }"
  ]

uniformPointerParameterSource :: String
uniformPointerParameterSource = unlines
  [ "enable unrestricted_pointer_parameters;"
  , "struct Values { value: u32, }"
  , "@group(0) @binding(0) var<uniform> values: Values;"
  , "fn read(pointer: ptr<uniform, u32>) -> u32 { return *pointer; }"
  , "@compute @workgroup_size(1)"
  , "fn main() { let result = read(&values.value); }"
  ]

pointerReturnSource :: String
pointerReturnSource = unlines
  [ "fn identity(value: ptr<function, i32>) -> ptr<function, i32> { return value; }"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

entryPointerReturnSource :: String
entryPointerReturnSource = unlines
  [ "@compute @workgroup_size(1)"
  , "fn main() -> ptr<function, i32> {"
  , "  var value = 0;"
  , "  return &value;"
  , "}"
  ]

pointerAliasPointeeSource :: String
pointerAliasPointeeSource = unlines
  [ "alias Element = i32;"
  , "fn bump(value: ptr<function, Element>) -> i32 {"
  , "  *value = *value + 1;"
  , "  return *value;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() { var value = 0; let result = bump(&value); }"
  ]

pointerToPointerSource :: String
pointerToPointerSource = unlines
  [ "fn invalid(value: ptr<function, ptr<function, i32>>) {}"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

arrayOfPointersSource :: String
arrayOfPointersSource = unlines
  [ "fn invalid(values: array<ptr<function, i32>, 1>) {}"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

inferredArrayOfPointersSource :: String
inferredArrayOfPointersSource = unlines
  [ "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  var value = 0;"
  , "  let pointer = &value;"
  , "  let pointers = array(pointer);"
  , "}"
  ]

pointerAliasCycleSource :: String
pointerAliasCycleSource = unlines
  [ "alias First = ptr<function, Second>;"
  , "alias Second = First;"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

partialPointerArgumentSource :: String
partialPointerArgumentSource = unlines
  [ "fn read_element(value: ptr<function, i32>) -> i32 { return *value; }"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  var values: array<i32, 2>;"
  , "  let result = read_element(&values[0]);"
  , "}"
  ]

unrestrictedPartialPointerArgumentSource :: String
unrestrictedPartialPointerArgumentSource =
  "enable unrestricted_pointer_parameters;\n" <> partialPointerArgumentSource

aliasedWritePointerArgumentsSource :: String
aliasedWritePointerArgumentsSource = unlines
  [ "fn mutate(first: ptr<function, i32>, second: ptr<function, i32>) {"
  , "  *first = 1;"
  , "  let observed = *second;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() { var value = 0; mutate(&value, &value); }"
  ]

aliasedReadOnlyPointerArgumentsSource :: String
aliasedReadOnlyPointerArgumentsSource = unlines
  [ "fn sum(first: ptr<function, i32>, second: ptr<function, i32>) -> i32 {"
  , "  return *first + *second;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() { var value = 1; let result = sum(&value, &value); }"
  ]

exactPointerOverloadSource :: String
exactPointerOverloadSource = unlines
  [ "fn inspect(first: ptr<function, i32>, second: ptr<function, i32>) {"
  , "  *first = *second;"
  , "}"
  , "fn inspect(first: ptr<function, u32>, second: ptr<function, u32>) {"
  , "  let observed = *first + *second;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() { var value = 1u; inspect(&value, &value); }"
  ]

transitiveAliasedWriteSource :: String
transitiveAliasedWriteSource = unlines
  [ "fn store(value: ptr<function, i32>) { *value = 1; }"
  , "fn forward(first: ptr<function, i32>, second: ptr<function, i32>) {"
  , "  store(first);"
  , "  let observed = *second;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() { var value = 0; forward(&value, &value); }"
  ]

moduleGlobalPointerAliasSource :: String
moduleGlobalPointerAliasSource = unlines
  [ "var<private> value: i32;"
  , "fn f6(pointer: ptr<private, i32>) {"
  , "  *pointer = 1;"
  , "  let observed = value;"
  , "}"
  , "@compute @workgroup_size(1) fn main() { f6(&value); }"
  ]

runtimeArrayLengthGlobalAliasSource :: String
runtimeArrayLengthGlobalAliasSource = unlines
  [ "enable unrestricted_pointer_parameters;"
  , "struct Values { first: u32, values: array<u32>, }"
  , "@group(0) @binding(0) var<storage, read_write> values: Values;"
  , "fn inspect(pointer: ptr<storage, array<u32>, read_write>) -> u32 {"
  , "  values.first = 1u;"
  , "  return arrayLength(pointer);"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() { let length = inspect(&values.values); }"
  ]

singleEntrySource :: String
singleEntrySource = "@compute @workgroup_size(1) fn main() {}"

negativeI32ConstantSource :: String
negativeI32ConstantSource = unlines
  [ "const negative: i32 = 1 - 2;"
  , "const minimum: i32 = -2147483648;"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  let a: i32 = negative;"
  , "  let b: i32 = minimum;"
  , "}"
  ]

floatIntegerCastBoundariesSource :: String
floatIntegerCastBoundariesSource = unlines
  [ "enable f16;"
  , "const i_min: i32 = i32(-2147483648.0f);"
  , "const i_max: i32 = i32(2147483520.0f);"
  , "const u_max: u32 = u32(4294967040.0f);"
  , "const h_max: i32 = i32(65504.0h);"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  let a = i_min;"
  , "  let b = i_max;"
  , "  let c = u_max;"
  , "  let d = h_max;"
  , "}"
  ]

invalidFloatIntegerCastCases :: [([Wesl.Option], [String], String)]
invalidFloatIntegerCastCases =
  [ ([], ["out of range for i32"], constantCastSource "const value: i32 = i32(2147483648.0f);")
  , ([], ["out of range for i32"], constantCastSource "const value: i32 = i32(-2147483904.0f);")
  , ([], ["out of range for u32"], constantCastSource "const value: u32 = u32(-1.0f);")
  , ([], ["out of range for u32"], constantCastSource "const value: u32 = u32(4294967296.0f);")
  , ([], ["f32 literal overflows its target type"], constantCastSource "const value: i32 = i32(1e39f);")
  , ( [Wesl.OptEnableFeature "f16"]
    , ["i32"]
    , enableF16 (constantCastSource "const value: i32 = i32(1e10h);")
    )
  ]
  where
    enableF16 source = "enable f16;\n" <> source

constantCastSource :: String -> String
constantCastSource declaration = unlines
  [ declaration
  , "@compute @workgroup_size(1)"
  , "fn main() { let result = value; }"
  ]

f32ConstantOverflowSource :: String
f32ConstantOverflowSource = unlines
  [ "const value: f32 = 1e39f;"
  , "@compute @workgroup_size(1) fn main() { let result = value; }"
  ]

f16ConstantOverflowSource :: String
f16ConstantOverflowSource = unlines
  [ "enable f16;"
  , "const value: f16 = f16(1e10f);"
  , "@compute @workgroup_size(1) fn main() { let result = value; }"
  ]

f16OverrideOverflowSource :: String
f16OverrideOverflowSource = unlines
  [ "enable f16;"
  , "override value: f16 = f16(1e10f);"
  , "@compute @workgroup_size(1) fn main() { let result = value; }"
  ]

abstractFloatPrecisionSource :: String
abstractFloatPrecisionSource = unlines
  [ "const_assert(1e100 / 1e100 == 1.0);"
  , "const_assert(1.0000000000000002 > 1.0);"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

abstractFloatOperationOverflowSource :: String
abstractFloatOperationOverflowSource = unlines
  [ "const_assert(1e308 * 1e308 > 0.0);"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

contextualF32OverflowSource :: String
contextualF32OverflowSource = unlines
  [ "@compute @workgroup_size(1)"
  , "fn main() { let value: f32 = 1e100; }"
  ]

contextualF16OverflowSource :: String
contextualF16OverflowSource = unlines
  [ "enable f16;"
  , "@compute @workgroup_size(1)"
  , "fn main() { let value: f16 = 1e10; }"
  ]

constructorF16OverflowSource :: String
constructorF16OverflowSource = unlines
  [ "enable f16;"
  , "@compute @workgroup_size(1)"
  , "fn main() { let value = vec2<f16>(1e10); }"
  ]

floatLiteralMaterializationSource :: String
floatLiteralMaterializationSource = unlines
  [ "enable f16;"
  , "const negative_zero: f16 = -0.0h;"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  let f32_tie = 1.000000059604644775390625f;"
  , "  let f32_above = 1.0000000596046449f;"
  , "  let f32_max = 3.4028234663852886e38f;"
  , "  let f16_tie = 1.00048828125h;"
  , "  let f16_above = 1.0004882812500002h;"
  , "  let f16_subnormal_tie = 2.98023223876953125e-8h;"
  , "  let f16_subnormal_above = 2.980232238769532e-8h;"
  , "  let use_negative_zero = negative_zero;"
  , "}"
  ]

floatSpecLiteralMaterializationSource :: String
floatSpecLiteralMaterializationSource = unlines
  [ "enable f16;"
  , "override f32_above: f32 = 1.0000000596046449f;"
  , "override f16_above: f16 = 1.0004882812500002h;"
  , "@compute @workgroup_size(1)"
  , "fn main() { let a = f32_above; let b = f16_above; }"
  ]

contextualFloatMaterializationSource :: String
contextualFloatMaterializationSource = unlines
  [ "enable f16;"
  , "struct HalfPair { first: f16, second: f16, }"
  , "const module_value: f16 = 1.0004882812500002;"
  , "fn accept(value: f16) -> f16 { return value; }"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  let negative_zero: f16 = -0.0;"
  , "  let negative_zero_f32: f32 = -0.0;"
  , "  let local_value: f16 = 1.0004882812500002;"
  , "  let expression_value: f16 = 1.0004882812500002 + 0.0;"
  , "  let unsigned_expression: u32 = 1 + 2;"
  , "  let vector_value: vec2<f16> = vec2(1.0004882812500002, 1.0);"
  , "  let array_value = array<f16, 2>(1.0004882812500002, 1.0);"
  , "  let struct_value = HalfPair(1.0004882812500002, 1.0);"
  , "  let call_value = accept(1.0004882812500002);"
  , "  let module_copy = module_value;"
  , "}"
  ]

contextualF16EntryReturnSource :: String
contextualF16EntryReturnSource = unlines
  [ "enable f16;"
  , "@fragment"
  , "fn main() -> @location(0) vec4<f16> {"
  , "  return vec4(1.0004882812500002);"
  , "}"
  ]

contextualFloatMaximumSource :: String
contextualFloatMaximumSource = unlines
  [ "enable f16;"
  , "const f16_max: f16 = 65504.0;"
  , "const f32_max: f32 = 3.4028234663852886e38;"
  , "@compute @workgroup_size(1)"
  , "fn main() { let half_value = f16_max; let float_value = f32_max; }"
  ]

contextualF16AboveMaximumSource :: String
contextualF16AboveMaximumSource = unlines
  [ "enable f16;"
  , "@compute @workgroup_size(1)"
  , "fn main() { let value: f16 = 65505.0; }"
  ]

contextualF32AboveMaximumSource :: String
contextualF32AboveMaximumSource = unlines
  [ "@compute @workgroup_size(1)"
  , "fn main() { let value: f32 = 3.4028235e38; }"
  ]

nearLimitArrayLayoutSource :: String
nearLimitArrayLayoutSource = unlines
  [ "struct NearLimit { values: array<u32, 1073741823>, }"
  , "@group(0) @binding(0) var<storage, read> near_limit: NearLimit;"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

arrayLayoutOverflowSource :: String
arrayLayoutOverflowSource = unlines
  [ "struct Huge { values: array<u32, 1073741824>, }"
  , "@group(0) @binding(0) var<storage, read> huge: Huge;"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

arrayCompositeLimitSource :: String
arrayCompositeLimitSource = unlines
  [ "@compute @workgroup_size(1)"
  , "fn main() { let values = array<u32, 65533>(); }"
  ]

arrayCompositeBoundarySource :: String
arrayCompositeBoundarySource = unlines
  [ "@compute @workgroup_size(1)"
  , "fn main() { let values = array<u32, 65532>(); }"
  ]

overrideArrayCompositeLimitSource :: String
overrideArrayCompositeLimitSource = unlines
  [ "override values: array<u32, 65533>;"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

layoutRoundUpOverflowSource :: String
layoutRoundUpOverflowSource = unlines
  [ "struct Huge {"
  , "  @size(2147483648) a: u32,"
  , "  @align(2147483648) b: u32,"
  , "  @align(2147483648) c: u32,"
  , "}"
  , "@group(0) @binding(0) var<storage, read> huge: Huge;"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

structLayoutOverflowSource :: String
structLayoutOverflowSource = unlines
  [ "struct Huge {"
  , "  @size(4294967292) a: u32,"
  , "  b: u32,"
  , "}"
  , "@group(0) @binding(0) var<storage, read> huge: Huge;"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

nestedUniformLayoutOverflowSource :: String
nestedUniformLayoutOverflowSource = unlines
  [ "struct NearLimit { @size(4294967292) value: u32, }"
  , "struct Overflow { nested: NearLimit, tail: u32, }"
  , "@group(0) @binding(0) var<uniform> overflow: Overflow;"
  , "@compute @workgroup_size(1) fn main() {}"
  ]

structuredScopeSource :: String
structuredScopeSource = unlines
  [ "diagnostic(warning, shadowing);"
  , "fn pick() -> i32 {"
  , "  var result = 1;"
  , "  if (true) { let result = true; }"
  , "  result = result + 1;"
  , "  loop {"
  , "    let result = true;"
  , "    break;"
  , "  }"
  , "  result = result + 1;"
  , "  for (var index = 0; index < 1; index = index + 1) {"
  , "    let index = true;"
  , "  }"
  , "  var values = array<i32, 1>(7);"
  , "  let selected = &values[0];"
  , "  if (true) {"
  , "    var selected = 2;"
  , "    selected = selected + 1;"
  , "  }"
  , "  return result;"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  let value = pick();"
  , "}"
  ]

continuingBreakIfSource :: String
continuingBreakIfSource = unlines
  [ "fn repeat_twice() {"
  , "  var index = 0;"
  , "  loop {"
  , "    index = index + 1;"
  , "    continuing {"
  , "      index = index + 1;"
  , "      break if (index >= 2);"
  , "    }"
  , "  }"
  , "}"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  repeat_twice();"
  , "  var index = 0;"
  , "  loop {"
  , "    index = index + 1;"
  , "    continuing {"
  , "      index = index + 1;"
  , "      break if (index >= 2);"
  , "    }"
  , "  }"
  , "}"
  ]

overrideWorkgroupSizeSource :: String
overrideWorkgroupSizeSource = unlines
  [ "@id(7) override x: u32 = 8u;"
  , "@id(8) override y: u32 = 4u;"
  , "@id(9) override z: u32 = 1u;"
  , "@compute @workgroup_size(x, y, z) fn main() {}"
  ]

oneDimensionalWorkgroupSizeSource :: String
oneDimensionalWorkgroupSizeSource = unlines
  [ "@id(7) override x: i32 = 8;"
  , "@compute @workgroup_size(x) fn main() {}"
  ]

derivedIntegerOverrideSource :: String
derivedIntegerOverrideSource = unlines
  [ "override base: u32 = 4u;"
  , "override derived: u32 = base + 2u;"
  , "@compute @workgroup_size(1) fn main() { let value = derived; }"
  ]

derivedAbstractIntegerOverrideSource :: String
derivedAbstractIntegerOverrideSource = unlines
  [ "override base: u32 = 4u;"
  , "override derived: u32 = base + 1;"
  , "@compute @workgroup_size(1) fn main() { let value = derived; }"
  ]

derivedSelectOverrideSource :: String
derivedSelectOverrideSource = unlines
  [ "override condition: bool = false;"
  , "override derived: u32 = select(4u, 8u, condition);"
  , "@compute @workgroup_size(derived) fn main() {}"
  ]

vectorSelectOverrideSource :: String
vectorSelectOverrideSource = unlines
  [ "override condition: bool = false;"
  , "override base: u32 = 1u;"
  , "override derived: vec2<u32> = select(vec2<u32>(base), vec2<u32>(2u), condition);"
  , "@compute @workgroup_size(1) fn main() { let value = derived; }"
  ]

matrixSelectOverrideSource :: String
matrixSelectOverrideSource = unlines
  [ "override condition: bool = false;"
  , "override base: f32 = 1.0;"
  , "override derived: mat2x2<f32> = select(mat2x2<f32>(base), mat2x2<f32>(2.0), condition);"
  , "@compute @workgroup_size(1) fn main() { let value = derived; }"
  ]

strictCompositeOverrideSource :: String
strictCompositeOverrideSource = unlines
  [ "struct Pair { first: u32, second: u32, }"
  , "@id(7) override vector_value: vec2<u32> = vec2<u32>(1u, 2u);"
  , "@id(8) override array_value: array<u32, 2> = array<u32, 2>(3u, 4u);"
  , "@id(9) override struct_value: Pair = Pair(5u, 6u);"
  , "@compute @workgroup_size(1)"
  , "fn main() {"
  , "  let vector_copy = vector_value;"
  , "  let array_copy = array_value;"
  , "  let struct_copy = struct_value;"
  , "}"
  ]

derivedFloatOverrideSource :: String
derivedFloatOverrideSource = unlines
  [ "override base: f32 = 1.0;"
  , "override derived: f32 = base + 2.0;"
  , "@compute @workgroup_size(1) fn main() { let value = derived; }"
  ]

derivedConversionOverrideSource :: String
derivedConversionOverrideSource = unlines
  [ "override base: i32 = 1;"
  , "override derived: u32 = u32(base);"
  , "@compute @workgroup_size(1) fn main() { let value = derived; }"
  ]

defaultlessWorkgroupOverrideSource :: String
defaultlessWorkgroupOverrideSource = unlines
  [ "@id(7) override x: u32;"
  , "@compute @workgroup_size(x) fn main() {}"
  ]

defaultlessWorkgroupWithAbstractDimensionSource :: String
defaultlessWorkgroupWithAbstractDimensionSource = unlines
  [ "override x: u32;"
  , "@compute @workgroup_size(x, 1) fn main() {}"
  ]

defaultlessWorkgroupExpressionSource :: String
defaultlessWorkgroupExpressionSource = unlines
  [ "override x: u32;"
  , "@compute @workgroup_size(1u / (x - 1u)) fn main() {}"
  ]

defaultlessWorkgroupWithKnownZeroSource :: String
defaultlessWorkgroupWithKnownZeroSource = unlines
  [ "override x: u32;"
  , "@compute @workgroup_size(0u, x) fn main() {}"
  ]

defaultlessI32WorkgroupOverrideSource :: String
defaultlessI32WorkgroupOverrideSource = unlines
  [ "override x: i32;"
  , "@compute @workgroup_size(x) fn main() {}"
  ]

mixedWorkgroupSizeTypesSource :: String
mixedWorkgroupSizeTypesSource = unlines
  [ "override x: i32;"
  , "override y: u32;"
  , "@compute @workgroup_size(x, y) fn main() {}"
  ]

floatWorkgroupSizeTypeSource :: String
floatWorkgroupSizeTypeSource = unlines
  [ "override x: f32;"
  , "@compute @workgroup_size(x) fn main() {}"
  ]

combinedSamplerTypeSource :: String
combinedSamplerTypeSource = unlines
  [ "@group(0) @binding(0) var tex: texture_2d<f32>;"
  , "@group(0) @binding(1) var comparison_sampler: sampler_comparison;"
  , "@fragment"
  , "fn main() -> @location(0) vec4<f32> {"
  , "  return textureSample(tex, comparison_sampler, vec2<f32>(0.5));"
  , "}"
  ]
