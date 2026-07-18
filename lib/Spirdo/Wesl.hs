-- | Minimal API for compiling WESL to renderer-friendly shader bundles.
module Spirdo.Wesl
  ( -- * Compile
    compile
  , compileWithDiagnostics
  , renderCompileError
  , renderCompileErrorWithSource
  , Source
  , sourceText
  , sourceNamed
  , sourceFile
  , Option(..)
  , SpirvTargetEnvironment(..)
  , OpenGlBindingRemap(..)
  , openGlImmediateBinding
  , OverrideSpecMode(..)
  , OverrideValue(..)
  , SamplerBindingMode(..)
  , CompileError(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)

    -- * Shader bundle
  , ShaderBundle
  , BindingLayout(..)
  , StorageTextureLayout(..)
  , OverrideLayout(..)
  , OverrideType(..)
  , overrideValueFromNumber
  , shaderSpirv
  , shaderSpirvWords
  , shaderStage
  , shaderBindings
  , shaderVertexAttributes
  , shaderOverrides
  , shaderSamplerMode
  , shaderWorkgroupSize
  , shaderImmediateByteLength

    -- * Core enums
  , ShaderStage(..)
  , BindingKind(..)
  , StorageAccess(..)
  , StorageFormat(..)
  , VertexFormat(..)
  , VertexAttribute(..)
  ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import qualified Spirdo.Wesl.Compiler as Compiler
import Spirdo.Wesl.Types
  ( CompileError(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , Option(..)
  , SpirvTargetEnvironment(..)
  , OpenGlBindingRemap(..)
  , openGlImmediateBinding
  , OverrideSpecMode(..)
  , OverrideValue(..)
  , Source(..)
  )
import Spirdo.Wesl.Util (renderErrorWithSource)
import Spirdo.Wesl.Types.Interface
  ( BindingKind(..)
  , SamplerBindingMode(..)
  , ShaderStage(..)
  , VertexAttribute(..)
  , VertexFormat(..)
  )
import qualified Spirdo.Wesl.Types.Interface as Interface
import Spirdo.Wesl.Types.Layout
  ( Scalar(..)
  , StorageAccess(..)
  , StorageFormat(..)
  , TypeLayout(..)
  , layoutSize
  )

-- | Compact renderer-facing shader bundle.
data ShaderBundle = ShaderBundle
  { sbSpirv :: !ByteString
  , sbStage :: !ShaderStage
  , sbBindings :: ![BindingLayout]
  , sbVertexAttributes :: ![VertexAttribute]
  , sbOverrides :: ![OverrideLayout]
  , sbSamplerMode :: !SamplerBindingMode
  , sbWorkgroupSize :: !(Maybe (Word32, Word32, Word32))
  , sbImmediateByteLength :: !(Maybe Word32)
  } deriving (Eq, Show)

-- | Compact binding metadata.
data BindingLayout = BindingLayout
  { blName :: !String
  , blKind :: !BindingKind
  , blGroup :: !Word32
  , blBinding :: !Word32
  , blStorageTexture :: !(Maybe StorageTextureLayout)
  } deriving (Eq, Show, Read)

-- | Format and access required to create a reflected storage texture binding.
data StorageTextureLayout = StorageTextureLayout
  { stlFormat :: !StorageFormat
  , stlAccess :: !StorageAccess
  } deriving (Eq, Show, Read)

-- | Simplified override metadata.
data OverrideLayout = OverrideLayout
  { olName :: !String
  , olSpecId :: !(Maybe Word32)
  , olType :: !OverrideType
  } deriving (Eq, Show, Read)

-- | Scalar shape accepted by a renderer pipeline constant.
data OverrideType
  = OverrideBool
  | OverrideI32
  | OverrideU32
  | OverrideF32
  | OverrideF16
  | OverrideComposite
  deriving (Eq, Show, Read)

-- | Convert a finite renderer pipeline constant to its reflected override type.
overrideValueFromNumber :: OverrideLayout -> Double -> Either String OverrideValue
overrideValueFromNumber layout value
  | isNaN value || isInfinite value =
      Left ("override " <> layout.olName <> " requires a finite value, got " <> show value)
overrideValueFromNumber layout value =
  case layout.olType of
    OverrideBool
      | value == 0 -> Right (OVBool False)
      | value == 1 -> Right (OVBool True)
      | otherwise -> Left ("boolean override " <> layout.olName <> " requires 0 or 1, got " <> show value)
    OverrideI32 -> OVI32 <$> boundedInteger "i32" (-2147483648) 2147483647
    OverrideU32 -> OVU32 <$> boundedInteger "u32" 0 4294967295
    OverrideF32
      | isInfinite convertedFloat -> Left ("f32 override " <> layout.olName <> " is out of range: " <> show value)
      | otherwise -> Right (OVF32 convertedFloat)
    OverrideF16
      | abs value > 65504 -> Left ("f16 override " <> layout.olName <> " is out of range: " <> show value)
      | otherwise -> Right (OVF16 convertedFloat)
    OverrideComposite -> Left ("composite override " <> layout.olName <> " cannot be set from one numeric pipeline constant")
  where
    convertedFloat = realToFrac value

    boundedInteger typeName minimumValue maximumValue
      | value /= fromInteger integerValue =
          Left (typeName <> " override " <> layout.olName <> " requires an integer, got " <> show value)
      | integerValue < minimumValue || integerValue > maximumValue =
          Left (typeName <> " override " <> layout.olName <> " is out of range: " <> show value)
      | otherwise = Right integerValue
      where
        integerValue = truncate value

-- | SPIR-V bytes for a shader bundle.
shaderSpirv :: ShaderBundle -> ByteString
shaderSpirv bundle = bundle.sbSpirv

-- | SPIR-V words for APIs that accept a native word array.
shaderSpirvWords :: ShaderBundle -> [Word32]
shaderSpirvWords bundle =
  [ word32At offset
  | offset <- [0, 4 .. BS.length bundle.sbSpirv - 4]
  ]
  where
    word32At offset =
      fromIntegral (BS.index bundle.sbSpirv offset)
        .|. (fromIntegral (BS.index bundle.sbSpirv (offset + 1)) `shiftL` 8)
        .|. (fromIntegral (BS.index bundle.sbSpirv (offset + 2)) `shiftL` 16)
        .|. (fromIntegral (BS.index bundle.sbSpirv (offset + 3)) `shiftL` 24)

-- | Cached stage for a shader bundle.
shaderStage :: ShaderBundle -> ShaderStage
shaderStage bundle = bundle.sbStage

-- | Binding metadata for a shader bundle.
shaderBindings :: ShaderBundle -> [BindingLayout]
shaderBindings bundle = bundle.sbBindings

-- | Vertex attributes for a shader bundle (empty for non-vertex stages).
shaderVertexAttributes :: ShaderBundle -> [VertexAttribute]
shaderVertexAttributes bundle = bundle.sbVertexAttributes

-- | Override metadata for a shader bundle.
shaderOverrides :: ShaderBundle -> [OverrideLayout]
shaderOverrides bundle = bundle.sbOverrides

-- | Sampler binding mode for a shader bundle.
shaderSamplerMode :: ShaderBundle -> SamplerBindingMode
shaderSamplerMode bundle = bundle.sbSamplerMode

-- | Known/default workgroup size for compute shaders, when available.
shaderWorkgroupSize :: ShaderBundle -> Maybe (Word32, Word32, Word32)
shaderWorkgroupSize bundle = bundle.sbWorkgroupSize

-- | Reflected byte length required by per-dispatch immediate values.
shaderImmediateByteLength :: ShaderBundle -> Maybe Word32
shaderImmediateByteLength bundle = bundle.sbImmediateByteLength

-- | Build an inline source with a default name.
sourceText :: String -> Source
sourceText = SourceInline "<inline>"

-- | Build inline source with a name used in diagnostics.
--
-- Inline sources cannot resolve filesystem imports; use 'sourceFile' when the
-- source imports other modules.
sourceNamed :: FilePath -> String -> Source
sourceNamed = SourceInline

-- | Build a file source (imports resolved from this file).
sourceFile :: FilePath -> Source
sourceFile = SourceFile

-- | Compile a source to a renderer-friendly shader bundle.
compile :: [Option] -> Source -> IO (Either CompileError ShaderBundle)
compile opts src =
  case src of
    SourceInline _ _ ->
      pure (bundleFromSomeShader <$> Compiler.compileWith opts src)
    SourceFile path ->
      fmap (fmap bundleFromSomeShader) (Compiler.compileFileWith opts path)

-- | Compile a source with diagnostics.
compileWithDiagnostics :: [Option] -> Source -> IO (Either CompileError (ShaderBundle, [Diagnostic]))
compileWithDiagnostics opts src =
  case src of
    SourceInline _ _ ->
      pure $
        (\(shader, diags) -> (bundleFromSomeShader shader, diags))
          <$> Compiler.compileWithDiagnostics opts src
    SourceFile path ->
      fmap (fmap (\(shader, diags) -> (bundleFromSomeShader shader, diags))) (Compiler.compileFileWithDiagnostics opts path)

-- | Render a compile error (with any embedded source context).
renderCompileError :: CompileError -> String
renderCompileError (CompileError msg _ _) = msg

-- | Render a compile error using explicit source text.
renderCompileErrorWithSource :: Maybe FilePath -> String -> CompileError -> String
renderCompileErrorWithSource = renderErrorWithSource

bundleFromSomeShader :: Interface.SomeShader -> ShaderBundle
bundleFromSomeShader (Interface.SomeShader shader) =
  let iface = Interface.shaderInterface shader
      plan = Interface.shaderPlan shader
      bindings = map bindingLayoutFromInfo plan.bpBindings
      overrides = map overrideLayoutFromInfo iface.siOverrides
      vattrs = fromMaybe [] (Interface.shaderVertexAttributes shader)
      workgroup = iface.siStageIO >>= (.sioWorkgroupSize)
  in ShaderBundle
      { sbSpirv = Interface.shaderSpirv shader
      , sbStage = Interface.shaderStageCached shader
      , sbBindings = bindings
      , sbVertexAttributes = vattrs
      , sbOverrides = overrides
      , sbSamplerMode = iface.siSamplerMode
      , sbWorkgroupSize = workgroup
      , sbImmediateByteLength = layoutSize <$> iface.siPushConstants
      }

bindingLayoutFromInfo :: Interface.BindingInfo -> BindingLayout
bindingLayoutFromInfo info =
  BindingLayout
    { blName = info.biName
    , blKind = info.biKind
    , blGroup = info.biGroup
    , blBinding = info.biBinding
    , blStorageTexture =
        case info.biType of
          TLStorageTexture1D format access -> Just (StorageTextureLayout format access)
          TLStorageTexture2D format access -> Just (StorageTextureLayout format access)
          TLStorageTexture2DArray format access -> Just (StorageTextureLayout format access)
          TLStorageTexture3D format access -> Just (StorageTextureLayout format access)
          _ -> Nothing
    }

overrideLayoutFromInfo :: Interface.OverrideInfo -> OverrideLayout
overrideLayoutFromInfo info =
  OverrideLayout
    { olName = info.oiName
    , olSpecId = info.oiSpecId
    , olType = overrideTypeFromLayout info.oiType
    }

overrideTypeFromLayout :: TypeLayout -> OverrideType
overrideTypeFromLayout layout =
  case layout of
    TLScalar Bool _ _ -> OverrideBool
    TLScalar I32 _ _ -> OverrideI32
    TLScalar U32 _ _ -> OverrideU32
    TLScalar F32 _ _ -> OverrideF32
    TLScalar F16 _ _ -> OverrideF16
    _ -> OverrideComposite
