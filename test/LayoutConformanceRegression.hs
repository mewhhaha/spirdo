module LayoutConformanceRegression (checks) where

import Control.Exception (IOException, bracket, bracketOnError, catch)
import Control.Monad (forM_, unless, when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.List (isInfixOf)
import Data.Word (Word16, Word32)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , findExecutable
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (Handle, hClose, openBinaryTempFile, openTempFile)
import System.Process (readProcessWithExitCode)

import qualified Spirdo.Wesl.Reflection as Wesl
import qualified Spirdo.Wesl.Uniform as Uniform

checks :: [(String, IO ())]
checks =
  [ ("layout-conformance: vec3 occupied size", checkVec3Layouts)
  , ("layout-conformance: size attributes", checkSizeAttributes)
  , ("layout-conformance: buffer address-space legality", checkBindingLegality)
  , ("layout-conformance: runtime arrays", checkRuntimeArrays)
  , ("layout-conformance: SPIR-V block wrappers", checkBlockWrappers)
  , ("layout-conformance: f16 storage capabilities", checkF16Capabilities)
  , ("layout-conformance: uniform layout feature provenance", checkUniformLayoutFeature)
  , ("layout-conformance: Vulkan target environments", checkVulkanVersionTargets)
  ]

checkVec3Layouts :: IO ()
checkVec3Layouts = do
  (_, interface32) <- compileArtifact [] vec3F32Source
  layout32 <- reflectedBindingLayout interface32
  assertStructFields
    "vec3<f32>"
    [ ("v", 0, 16, 12)
    , ("tail", 12, 4, 4)
    ]
    16
    layout32
  (_, interface16) <- compileArtifact [Wesl.OptEnableFeature "f16"] vec3F16Source
  layout16 <- reflectedBindingLayout interface16
  assertStructFields
    "vec3<f16>"
    [ ("v", 0, 8, 6)
    , ("tail", 6, 2, 2)
    ]
    8
    layout16

checkSizeAttributes :: IO ()
checkSizeAttributes = do
  (bytes, interface) <- compileArtifact [] explicitSizeSource
  layout <- reflectedBindingLayout interface
  assertStructFields
    "@size"
    [ ("a", 0, 4, 5)
    , ("b", 8, 4, 4)
    ]
    12
    layout
  case Uniform.packUniform
      layout
      (Uniform.UVStruct
        [ ("a", Uniform.UVScalar (Uniform.SVF32 1))
        , ("b", Uniform.UVScalar (Uniform.SVF32 2))
        ]) of
    Left err -> fail ("@size packing rejected reflected layout: " <> err)
    Right packed -> unless (BS.length packed == 12) $
      fail ("@size packing returned " <> show (BS.length packed) <> " bytes")
  validateSpirv "layout-explicit-size" [] bytes
  (_, constantInterface) <- compileArtifact [] constantLayoutAttributeSource
  constantLayout <- reflectedBindingLayout constantInterface
  assertStructFields
    "constant layout attributes"
    [ ("v", 0, 16, 13)
    , ("tail", 16, 4, 4)
    ]
    32
    constantLayout
  expectCompileError
    []
    "field @size requires a creation-fixed footprint type"
    runtimeArraySizeAttributeSource

checkBindingLegality :: IO ()
checkBindingLegality = do
  expectCompileError [] "uniform bindings must use host-shareable types" nestedUniformBoolSource
  expectCompileError [] "storage bindings must use host-shareable types" storageBoolSource
  expectCompileError [] "uniform bindings cannot contain atomic types" uniformAtomicSource
  expectCompileError [] "read-only storage bindings cannot contain atomic types" storageReadAtomicSource
  forM_
    [ ("storage-rw-atomic", storageReadWriteAtomicSource)
    , ("workgroup-atomic", workgroupAtomicSource)
    ]
    $ \(label, source) -> do
      (bytes, _) <- compileArtifact [] source
      validateSpirv label ["--target-env", "vulkan1.3"] bytes

checkRuntimeArrays :: IO ()
checkRuntimeArrays = do
  (directBytes, directInterface) <- compileArtifact [] directRuntimeArraySource
  directLayout <- reflectedBindingLayout directInterface
  case directLayout of
    layout@(Wesl.TLArray Nothing 4 (Wesl.TLScalar Wesl.U32 4 4) 4 0) ->
      case Uniform.packUniform layout (Uniform.UVArray []) of
        Left err -> unless ("runtime-sized arrays cannot be packed" `isInfixOf` err) $
          fail ("unexpected runtime-array packing error: " <> err)
        Right _ -> fail "runtime-sized array was accepted by uniform packing"
    layout -> fail ("direct runtime array reflection mismatch: " <> show layout)
  validateSpirv "runtime-array-direct" ["--target-env", "vulkan1.3"] directBytes

  (structBytes, structInterface) <- compileArtifact [] structRuntimeArraySource
  structLayout <- reflectedBindingLayout structInterface
  assertStructFields
    "runtime array struct"
    [ ("count", 0, 4, 4)
    , ("values", 4, 4, 0)
    ]
    4
    structLayout
  validateSpirv "runtime-array-struct" ["--target-env", "vulkan1.3"] structBytes
  expectCompileError [] "runtime arrays must be the direct storage type or the final member" nonFinalRuntimeArraySource
  expectCompileError [] "runtime arrays must be the direct storage type or the final member" nestedRuntimeArraySource

checkBlockWrappers :: IO ()
checkBlockWrappers = do
  (bytes, _) <- compileArtifact [] blockWrapperSource
  instructions <- decodeSpirv bytes
  let blockTypes = [target | (71, [target, 2]) <- instructions]
      structTypes = [(target, members) | (30, target : members) <- instructions]
  unless (length blockTypes == 4) $
    fail ("expected four descriptor Block types, got " <> show blockTypes)
  forM_ blockTypes $ \blockType ->
    case lookup blockType structTypes of
      Just [_storeType] -> pure ()
      Just members -> fail ("Block wrapper has " <> show (length members) <> " members")
      Nothing -> fail ("Block decoration target is not an OpTypeStruct: " <> show blockType)
  let colMajorMembers = [(target, member) | (72, [target, member, 5]) <- instructions]
      matrixStrides = [(target, member, stride) | (72, [target, member, 7, stride]) <- instructions]
      arrayStrides = [stride | (71, [_target, 6, stride]) <- instructions]
  unless (length colMajorMembers >= 2) $
    fail "matrix and array-of-matrix members are missing ColMajor decorations"
  unless (length (filter (\(_, _, stride) -> stride == 8) matrixStrides) >= 2) $
    fail ("matrix members are missing MatrixStride 8: " <> show matrixStrides)
  unless (16 `elem` arrayStrides) $
    fail ("array-of-matrix type is missing ArrayStride 16: " <> show arrayStrides)
  validateSpirv "buffer-block-wrappers" ["--target-env", "vulkan1.3"] bytes

checkF16Capabilities :: IO ()
checkF16Capabilities = do
  (storageBytes, _) <- compileArtifact f16Options f16StorageSource
  storageInstructions <- decodeSpirv storageBytes
  let storageCapabilities = [capability | (17, [capability]) <- storageInstructions]
  unless (9 `elem` storageCapabilities && 4433 `elem` storageCapabilities) $
    fail ("storage f16 capabilities are incomplete: " <> show storageCapabilities)
  when (4434 `elem` storageCapabilities) $
    fail ("storage-only shader emitted uniform f16 capability: " <> show storageCapabilities)

  (uniformBytes, _) <- compileArtifact f16Options f16UniformSource
  uniformInstructions <- decodeSpirv uniformBytes
  let uniformCapabilities = [capability | (17, [capability]) <- uniformInstructions]
  unless (9 `elem` uniformCapabilities && 4434 `elem` uniformCapabilities) $
    fail ("uniform f16 capabilities are incomplete: " <> show uniformCapabilities)
  when (4433 `elem` uniformCapabilities) $
    fail ("uniform f16 shader retained redundant storage-only capability: " <> show uniformCapabilities)

  (stageBytes, _) <- compileArtifact f16Options f16StageIOSource
  stageInstructions <- decodeSpirv stageBytes
  unless (4436 `elem` [capability | (17, [capability]) <- stageInstructions]) $
    fail "f16 stage IO did not emit StorageInputOutput16"

  (spirv10Bytes, _) <- compileArtifact (Wesl.OptSpirvVersion 0x00010000 : f16Options) f16UniformSource
  spirv10Instructions <- decodeSpirv spirv10Bytes
  let extensions = [decodeString operands | (10, operands) <- spirv10Instructions]
  unless ("SPV_KHR_16bit_storage" `elem` extensions) $
    fail ("SPIR-V 1.0 f16 shader is missing SPV_KHR_16bit_storage: " <> show extensions)
  validateSpirv "f16-uniform-spirv10" ["--target-env", "vulkan1.0"] spirv10Bytes

  (storage10Bytes, _) <- compileArtifact (Wesl.OptSpirvVersion 0x00010000 : f16Options) f16StorageSource
  storage10Instructions <- decodeSpirv storage10Bytes
  let storageExtensions = [decodeString operands | (10, operands) <- storage10Instructions]
  unless (all (`elem` storageExtensions) ["SPV_KHR_16bit_storage", "SPV_KHR_storage_buffer_storage_class"]) $
    fail ("SPIR-V 1.0 storage shader extensions are incomplete: " <> show storageExtensions)
  validateSpirv "f16-storage-spirv10" ["--target-env", "vulkan1.0"] storage10Bytes

checkUniformLayoutFeature :: IO ()
checkUniformLayoutFeature = do
  expectCompileError
    [Wesl.OptEnableFeature "uniform_buffer_standard_layout"]
    "uniform binding layout requires enable uniform_buffer_standard_layout"
    uniformNaturalArraySource
  (bytes, _) <- compileArtifact uniformLayoutOptions uniformNaturalArrayEnabledSource
  validateSpirv "uniform-standard-layout" [] bytes
  expectCompileError [] "uniform binding layout requires enable uniform_buffer_standard_layout" uniformMat2Source
  (mat2Bytes, _) <- compileArtifact uniformLayoutOptions uniformMat2EnabledSource
  validateSpirv
    "uniform-standard-layout-mat2"
    ["--target-env", "vulkan1.3", "--uniform-buffer-standard-layout"]
    mat2Bytes
  (mat3Bytes, _) <- compileArtifact [] uniformMat3Source
  validateSpirv "uniform-legacy-layout-mat3" ["--target-env", "vulkan1.3"] mat3Bytes
  checkImportedUniformLayoutFeature

checkVulkanVersionTargets :: IO ()
checkVulkanVersionTargets =
  forM_ vulkanVersionTargets $ \(version, targetEnvironment) -> do
    (bytes, _) <- compileArtifact [Wesl.OptSpirvVersion version] computeEntry
    validateSpirv ("spirv-version-" <> show version) ["--target-env", targetEnvironment] bytes
  where
    vulkanVersionTargets =
      [ (0x00010000, "vulkan1.0")
      , (0x00010100, "vulkan1.1")
      , (0x00010200, "vulkan1.1")
      , (0x00010300, "vulkan1.1")
      , (0x00010400, "vulkan1.1spv1.4")
      , (0x00010500, "vulkan1.2")
      , (0x00010600, "vulkan1.3")
      ]

checkImportedUniformLayoutFeature :: IO ()
checkImportedUniformLayoutFeature =
  bracket createScratchDirectory removePathForcibly $ \directory -> do
    let rootPath = directory </> "main.wesl"
        importedPath = directory </> "layout_feature.wesl"
    writeFile importedPath "enable uniform_buffer_standard_layout;\n"
    writeFile rootPath importedOnlyUniformLayoutSource
    importedOnly <- Wesl.compileFileWith uniformLayoutOptions rootPath
    case importedOnly of
      Left err ->
        unless ("uniform binding layout requires enable uniform_buffer_standard_layout" `isInfixOf` Wesl.renderCompileError err) $
          fail ("unexpected imported-feature error: " <> Wesl.renderCompileError err)
      Right _ -> fail "an imported enable relaxed the root module's uniform layout"
    writeFile importedPath importedUniformBindingSource
    writeFile rootPath ("import layout_feature;\n" <> computeEntry)
    importedBinding <- Wesl.compileFileWith uniformLayoutOptions rootPath
    case importedBinding of
      Left err -> fail ("an imported module's own enabled uniform was rejected: " <> Wesl.renderCompileError err)
      Right _ -> pure ()
    writeFile importedPath "enable uniform_buffer_standard_layout;\n"
    writeFile rootPath rootUniformLayoutSource
    rootEnabled <- Wesl.compileFileWith uniformLayoutOptions rootPath
    case rootEnabled of
      Left err -> fail ("root uniform layout enable was rejected: " <> Wesl.renderCompileError err)
      Right _ -> pure ()
  where
    createScratchDirectory = do
      temporaryDirectory <- getTemporaryDirectory
      bracketOnError
        (openTempFile temporaryDirectory "spirdo-layout-feature-regression")
        cleanupTemporaryFile
        $ \(path, handle) -> do
            hClose handle
            removeFile path
            createDirectoryIfMissing True path
            pure path

reflectedBindingLayout :: Wesl.ShaderInterface -> IO Wesl.TypeLayout
reflectedBindingLayout interface =
  case interface.siBindings of
    [binding] -> pure binding.biType
    bindings -> fail ("expected one reflected binding, got " <> show (length bindings))

assertStructFields :: String -> [(String, Word32, Word32, Word32)] -> Word32 -> Wesl.TypeLayout -> IO ()
assertStructFields label expectedFields expectedSize layout =
  case layout of
    Wesl.TLStruct _ fields _ size -> do
      let actualFields =
            [ (field.flName, field.flOffset, field.flAlign, field.flSize)
            | field <- fields
            ]
      unless (actualFields == expectedFields) $
        fail (label <> " field layout mismatch: " <> show actualFields)
      unless (size == expectedSize) $
        fail (label <> " struct size mismatch: " <> show size)
    _ -> fail (label <> " expected struct reflection, got " <> show layout)

compileArtifact :: [Wesl.Option] -> String -> IO (BS.ByteString, Wesl.ShaderInterface)
compileArtifact options source =
  case Wesl.compileWith options (Wesl.SourceInline "<layout-regression>" source) of
    Left err -> fail (Wesl.renderCompileError err <> "\nsource:\n" <> source)
    Right (Wesl.SomeShader shader) ->
      pure (Wesl.shaderSpirv shader, Wesl.shaderInterface shader)

expectCompileError :: [Wesl.Option] -> String -> String -> IO ()
expectCompileError options expected source =
  case Wesl.compileWith options (Wesl.SourceInline "<layout-regression>" source) of
    Left err ->
      unless (expected `isInfixOf` Wesl.renderCompileError err) $
        fail ("unexpected compile error: " <> Wesl.renderCompileError err <> "\nsource:\n" <> source)
    Right _ -> fail ("expected compilation to fail with: " <> expected)

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
                let operands = [word32At bytes (offset + index * 4) | index <- [1 .. wordCount - 1]]
                in go nextOffset ((opcode, operands) : acc)

word32At :: BS.ByteString -> Int -> Word32
word32At bytes offset =
  let byte0 = fromIntegral (BS.index bytes offset)
      byte1 = fromIntegral (BS.index bytes (offset + 1))
      byte2 = fromIntegral (BS.index bytes (offset + 2))
      byte3 = fromIntegral (BS.index bytes (offset + 3))
  in byte0 .|. (byte1 `shiftL` 8) .|. (byte2 `shiftL` 16) .|. (byte3 `shiftL` 24)

decodeString :: [Word32] -> String
decodeString = takeWhile (/= '\0') . concatMap wordCharacters
  where
    wordCharacters word =
      [ chr (fromIntegral (word .&. 0xff))
      , chr (fromIntegral ((word `shiftR` 8) .&. 0xff))
      , chr (fromIntegral ((word `shiftR` 16) .&. 0xff))
      , chr (fromIntegral ((word `shiftR` 24) .&. 0xff))
      ]

validateSpirv :: String -> [String] -> BS.ByteString -> IO ()
validateSpirv label validatorOptions bytes = do
  validator <- findExecutable "spirv-val"
  case validator of
    Nothing -> pure ()
    Just executable -> do
      temporaryDirectory <- getTemporaryDirectory
      bracket
        (openBinaryTempFile temporaryDirectory (label <> ".spv"))
        cleanupTemporaryFile
        $ \(path, handle) -> do
            BS.hPut handle bytes
            hClose handle
            (exitCode, _stdout, stderr) <- readProcessWithExitCode executable (validatorOptions <> [path]) ""
            case exitCode of
              ExitSuccess -> pure ()
              ExitFailure _ -> fail (label <> ": spirv-val failed: " <> stderr)

cleanupTemporaryFile :: (FilePath, Handle) -> IO ()
cleanupTemporaryFile (path, handle) = do
  hClose handle `catch` ignoreIOException
  exists <- doesFileExist path
  when exists (removeFile path)

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = pure ()

computeEntry :: String
computeEntry = "@compute @workgroup_size(1) fn main() {}\n"

vec3F32Source :: String
vec3F32Source = unlines
  [ "struct Params { v: vec3<f32>, tail: f32, };"
  , "@group(0) @binding(0) var<storage, read> params: Params;"
  , computeEntry
  ]

vec3F16Source :: String
vec3F16Source = unlines
  [ "enable f16;"
  , "struct Params { v: vec3<f16>, tail: f16, };"
  , "@group(0) @binding(0) var<storage, read> params: Params;"
  , computeEntry
  ]

explicitSizeSource :: String
explicitSizeSource = unlines
  [ "struct Params { @size(5) a: f32, b: f32, };"
  , "@group(0) @binding(0) var<storage, read> params: Params;"
  , computeEntry
  ]

constantLayoutAttributeSource :: String
constantLayoutAttributeSource = unlines
  [ "const alignment = 16;"
  , "const footprint = 13;"
  , "struct Params { @align(alignment) @size(footprint) v: vec3<f32>, tail: f32, };"
  , "@group(0) @binding(0) var<storage, read> params: Params;"
  , computeEntry
  ]

runtimeArraySizeAttributeSource :: String
runtimeArraySizeAttributeSource = unlines
  [ "struct Data { @size(16) values: array<u32>, };"
  , "@group(0) @binding(0) var<storage, read> data: Data;"
  , computeEntry
  ]

nestedUniformBoolSource :: String
nestedUniformBoolSource = unlines
  [ "struct Inner { enabled: bool, };"
  , "struct Params { inner: Inner, };"
  , "@group(0) @binding(0) var<uniform> params: Params;"
  , computeEntry
  ]

storageBoolSource :: String
storageBoolSource = unlines
  [ "@group(0) @binding(0) var<storage, read> enabled: bool;"
  , computeEntry
  ]

uniformAtomicSource :: String
uniformAtomicSource = unlines
  [ "@group(0) @binding(0) var<uniform> value: atomic<u32>;"
  , computeEntry
  ]

storageReadAtomicSource :: String
storageReadAtomicSource = unlines
  [ "@group(0) @binding(0) var<storage, read> value: atomic<u32>;"
  , computeEntry
  ]

storageReadWriteAtomicSource :: String
storageReadWriteAtomicSource = unlines
  [ "@group(0) @binding(0) var<storage, read_write> value: atomic<u32>;"
  , "@compute @workgroup_size(1) fn main() { let old = atomicAdd(value, 1u); }"
  ]

workgroupAtomicSource :: String
workgroupAtomicSource =
  "var<workgroup> value: atomic<u32>;\n@compute @workgroup_size(1) fn main() { let old = atomicAdd(value, 1u); }\n"

directRuntimeArraySource :: String
directRuntimeArraySource = unlines
  [ "@group(0) @binding(0) var<storage, read_write> values: array<u32>;"
  , "@compute @workgroup_size(1) fn main() {"
  , "  let count = arrayLength(&values);"
  , "  if (count > 0u) { values[0] = count; }"
  , "}"
  ]

structRuntimeArraySource :: String
structRuntimeArraySource = unlines
  [ "struct Data { count: u32, values: array<u32>, };"
  , "@group(0) @binding(0) var<storage, read_write> data: Data;"
  , "@compute @workgroup_size(1) fn main() {"
  , "  let count = arrayLength(&data.values);"
  , "  if (count > 0u) { data.values[0] = count; }"
  , "}"
  ]

nonFinalRuntimeArraySource :: String
nonFinalRuntimeArraySource = unlines
  [ "struct Data { values: array<u32>, count: u32, };"
  , "@group(0) @binding(0) var<storage, read> data: Data;"
  , computeEntry
  ]

nestedRuntimeArraySource :: String
nestedRuntimeArraySource = unlines
  [ "struct Inner { values: array<u32>, };"
  , "struct Outer { inner: Inner, };"
  , "@group(0) @binding(0) var<storage, read> data: Outer;"
  , computeEntry
  ]

blockWrapperSource :: String
blockWrapperSource = unlines
  [ "struct Inner { x: f32, };"
  , "struct Outer { inner: Inner, };"
  , "@group(0) @binding(0) var<storage, read> first: Inner;"
  , "@group(0) @binding(1) var<storage, read> nested: Outer;"
  , "@group(0) @binding(2) var<storage, read> matrix: mat2x2<f32>;"
  , "@group(0) @binding(3) var<storage, read> matrices: array<mat2x2<f32>, 2>;"
  , computeEntry
  ]

f16Options :: [Wesl.Option]
f16Options = [Wesl.OptEnableFeature "f16"]

f16StorageSource :: String
f16StorageSource = unlines
  [ "enable f16;"
  , "@group(0) @binding(0) var<storage, read> value: vec2<f16>;"
  , computeEntry
  ]

f16UniformSource :: String
f16UniformSource = unlines
  [ "enable f16;"
  , "@group(0) @binding(0) var<uniform> value: f16;"
  , computeEntry
  ]

f16StageIOSource :: String
f16StageIOSource = unlines
  [ "enable f16;"
  , "@fragment fn main(@location(0) value: f16) -> @location(0) f16 { return value; }"
  ]

uniformLayoutOptions :: [Wesl.Option]
uniformLayoutOptions = [Wesl.OptEnableFeature "uniform_buffer_standard_layout"]

uniformNaturalArraySource :: String
uniformNaturalArraySource = unlines
  [ "@group(0) @binding(0) var<uniform> values: array<f32, 4>;"
  , computeEntry
  ]

uniformNaturalArrayEnabledSource :: String
uniformNaturalArrayEnabledSource = "enable uniform_buffer_standard_layout;\n" <> uniformNaturalArraySource

uniformMat2Source :: String
uniformMat2Source = unlines
  [ "@group(0) @binding(0) var<uniform> value: mat2x2<f32>;"
  , computeEntry
  ]

uniformMat2EnabledSource :: String
uniformMat2EnabledSource = "enable uniform_buffer_standard_layout;\n" <> uniformMat2Source

uniformMat3Source :: String
uniformMat3Source = unlines
  [ "@group(0) @binding(0) var<uniform> value: mat3x3<f32>;"
  , computeEntry
  ]

importedOnlyUniformLayoutSource :: String
importedOnlyUniformLayoutSource = "import layout_feature;\n" <> uniformNaturalArraySource

rootUniformLayoutSource :: String
rootUniformLayoutSource = "enable uniform_buffer_standard_layout;\nimport layout_feature;\n" <> uniformNaturalArraySource

importedUniformBindingSource :: String
importedUniformBindingSource =
  unlines
    [ "enable uniform_buffer_standard_layout;"
    , "@group(0) @binding(0) var<uniform> values: array<f32, 4>;"
    ]
