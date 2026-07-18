-- | Advanced API: full shader reflection types + raw compile helpers.
module Spirdo.Wesl.Reflection
  ( -- * Compile-time interface types
    BindingKind(..)
  , StorageAccess(..)
  , StorageFormat(..)
  , Scalar(..)
  , ScalarType(..)
  , Field(..)
  , Ty(..)
  , Binding(..)
  , ShaderStage(..)
  , IOParam(..)
  , StageIO(..)
  , BindingPlan(..)
  , BindingSlotCount(..)
  , bindingSlotCounts
  , singleGroupBindingSlotCount
  , bindingInfoFor
  , shaderStage
  , ShaderSource(..)
  , Shader
  , SomeShader(..)
  , shaderSpirv
  , shaderInterface
  , shaderPlan
  , shaderStageCached
  , shaderVertexAttributes
  , shaderSource
  , UniformValue
  , ToUniform(..)
  , uniform
  , packUniform
  , packUniformFrom
  , validateUniformStorableUnchecked
  , packUniformStorableUnchecked
  , V2(..)
  , V3(..)
  , V4(..)
  , M2(..)
  , M3(..)
  , M4(..)
  , M3x4(..)
  , M4x3(..)
  , Half(..)
  , stageIO
  , stageInputs
  , stageOutputs
  , vertexInputs
  , vertexOutputs
  , vertexAttributes
  , VertexFormat(..)
  , VertexAttribute(..)
  , pushConstantLayout
  , layoutSize
  , ShaderInterface(..)
  , OverrideInfo(..)
  , BindingInfo(..)
  , TypeLayout(..)
  , FieldLayout(..)
  , specializableOverrides
  , CompileError(..)
  , renderCompileError
  , renderCompileErrorWithSource
  , Source(..)
  , Option(..)
  , SpirvTargetEnvironment(..)
  , OpenGlBindingRemap(..)
  , CompileOptions
  , SamplerBindingMode(..)
  , OverrideSpecMode(..)
  , OverrideValue(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , defaultCompileOptions
  , defaultOptions
  , applyOptions
  , withSpirvVersion
  , withTargetEnvironment
  , withFeatures
  , withOverrides
  , withOverrideSpecMode
  , withSamplerMode
  , withEntryPoint
  , withCache
  , withCacheDir
  , withCacheVerbose
  , withTimingVerbose
  , compile
  , compileWith
  , compileWithDiagnostics
  , compileFile
  , compileFileWith
  , compileFileWithDiagnostics
  , wesl
  , spirv
  , Imports
  , Import
  , Snoc
  , imports
  , module_
  , (<:)
  ) where

import Spirdo.Wesl.Compiler
import Spirdo.Wesl.Types
import Spirdo.Wesl.Util (renderErrorWithSource)

-- | Render a compile error (with any embedded source context).
renderCompileError :: CompileError -> String
renderCompileError (CompileError msg _ _) = msg

-- | Render a compile error using explicit source text.
renderCompileErrorWithSource :: Maybe FilePath -> String -> CompileError -> String
renderCompileErrorWithSource = renderErrorWithSource
