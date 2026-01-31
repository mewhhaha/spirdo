-- | Minimal API for compiling WESL to renderer-friendly shader bundles.
module Spirdo.Wesl
  ( -- * Compile
    compile
  , compileWithDiagnostics
  , renderCompileError
  , renderCompileErrorWithSource
  , Source
  , sourceText
  , sourceFile
  , Option(..)
  , CachePolicy(..)
  , OverrideSpecMode(..)
  , OverrideValue(..)
  , SamplerBindingMode(..)
  , CompileError(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)

    -- * Shader bundle
  , ShaderBundle
  , BindingLayout(..)
  , OverrideLayout(..)
  , shaderSpirv
  , shaderStage
  , shaderBindings
  , shaderVertexAttributes
  , shaderOverrides
  , shaderSamplerMode
  , shaderWorkgroupSize

    -- * Core enums
  , ShaderStage(..)
  , BindingKind(..)
  , VertexFormat(..)
  , VertexAttribute(..)
  ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

import qualified Spirdo.Wesl.Compiler as Compiler
import Spirdo.Wesl.Types
  ( CachePolicy(..)
  , CompileError(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , Option(..)
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

-- | Compact renderer-facing shader bundle.
data ShaderBundle = ShaderBundle
  { sbSpirv :: !ByteString
  , sbStage :: !ShaderStage
  , sbBindings :: ![BindingLayout]
  , sbVertexAttributes :: ![VertexAttribute]
  , sbOverrides :: ![OverrideLayout]
  , sbSamplerMode :: !SamplerBindingMode
  , sbWorkgroupSize :: !(Maybe (Word32, Word32, Word32))
  } deriving (Eq, Show, Read)

-- | Simplified binding metadata (no type layout).
data BindingLayout = BindingLayout
  { blName :: !String
  , blKind :: !BindingKind
  , blGroup :: !Word32
  , blBinding :: !Word32
  } deriving (Eq, Show, Read)

-- | Simplified override metadata.
data OverrideLayout = OverrideLayout
  { olName :: !String
  , olSpecId :: !(Maybe Word32)
  } deriving (Eq, Show, Read)

-- | SPIR-V bytes for a shader bundle.
shaderSpirv :: ShaderBundle -> ByteString
shaderSpirv bundle = bundle.sbSpirv

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

-- | Workgroup size for compute shaders.
shaderWorkgroupSize :: ShaderBundle -> Maybe (Word32, Word32, Word32)
shaderWorkgroupSize bundle = bundle.sbWorkgroupSize

-- | Build an inline source with a default name.
sourceText :: String -> Source
sourceText = SourceInline "<inline>"

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
      }

bindingLayoutFromInfo :: Interface.BindingInfo -> BindingLayout
bindingLayoutFromInfo info =
  BindingLayout
    { blName = info.biName
    , blKind = info.biKind
    , blGroup = info.biGroup
    , blBinding = info.biBinding
    }

overrideLayoutFromInfo :: Interface.OverrideInfo -> OverrideLayout
overrideLayoutFromInfo info =
  OverrideLayout
    { olName = info.oiName
    , olSpecId = info.oiSpecId
    }
