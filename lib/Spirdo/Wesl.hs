{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Public API for compiling WESL to SPIR-V and reflecting shader interfaces.
module Spirdo.Wesl
  ( -- * Compile-time interface types
    BindingKind(..)
  , StorageAccess(..)
  , StorageFormat(..)
  , Scalar(..)
  , ScalarType(..)
  , Field(..)
  , Ty(..)
  , Binding(..)
  , BindingDesc(..)
  , ShaderStage(..)
  , IOParam(..)
  , StageIO(..)
  , BindingPlan(..)
  , ReflectBindings(..)
  , HasBinding
  , binding
  , shaderStage
  , VertexShader(..)
  , FragmentShader(..)
  , ComputeShader(..)
  , asVertexShader
  , asFragmentShader
  , asComputeShader
  , PreparedShader(..)
  , prepareShader
  , UniformValue(..)
  , ToUniform(..)
  , uniform
  , packUniform
  , packUniformFrom
  , validateUniformStorable
  , packUniformStorable
  , V2(..)
  , V3(..)
  , V4(..)
  , M2(..)
  , M3(..)
  , M4(..)
  , Half(..)
  , stageIO
  , stageInputs
  , stageOutputs
  , vertexInputs
  , vertexOutputs
  , vertexAttributes
  , VertexFormat(..)
  , VertexAttribute(..)
  , bindingPlan
  , isUniformKind
  , isSamplerKind
  , isTextureKind
  , isStorageBufferKind
  , isStorageTextureKind
  , pushConstantLayout
  , samplerBindings
  , uniformBindings
  , storageBufferBindings
  , storageTextureBindings
  , CompiledShader(..)
  , SomeCompiledShader(..)
  , ShaderInterface(..)
  , OverrideInfo(..)
  , BindingInfo(..)
  , BindingMap(..)
  , Layout(..)
  , LayoutBinding(..)
  , layoutFromPrepared
  , layoutFromShader
  , BindingTable(..)
  , bindingTable
  , bindingTableFromPrepared
  , TypeLayout(..)
  , FieldLayout(..)
  , bindingMap
  , bindingInfoFor
  , bindingInfoForMap
  , specializableOverrides
  , CompileError(..)
  , CompileOptions(..)
  , SamplerBindingMode(..)
  , OverrideSpecMode(..)
  , OverrideValue(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , defaultCompileOptions
  , compileWeslToSpirv
  , compileWeslToSpirvWith
  , compileWeslToSpirvFile
  , compileWeslToSpirvFileWith
  , compileWeslToSpirvBytes
  , compileWeslToSpirvBytesWith
  , compileWeslToSpirvWithDiagnostics
  , compileWeslToSpirvFileWithDiagnostics
  , compileWeslToSpirvBytesWithDiagnostics
  , compileWeslToSpirvWithTimings
  , compileWeslToSpirvBytesWithTimings
  , PackageInfo(..)
  , PackageDependency(..)
  , discoverPackageInfo
  , wesl
  , weslWith
  ) where

import Spirdo.Wesl.Compiler
import Spirdo.Wesl.Types
