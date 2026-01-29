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
  , ShaderStage(..)
  , IOParam(..)
  , StageIO(..)
  , BindingPlan(..)
  , shaderStage
  , PreparedShader
  , SomePreparedShader(..)
  , preparedSpirv
  , preparedInterface
  , preparedStage
  , preparedPlan
  , preparedVertexAttributes
  , UniformValue(..)
  , ScalarValue(..)
  , ToScalar(..)
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
  , pushConstantLayout
  , ShaderInterface(..)
  , OverrideInfo(..)
  , BindingInfo(..)
  , TypeLayout(..)
  , FieldLayout(..)
  , specializableOverrides
  , CompileError(..)
  , CompileOptions(..)
  , SamplerBindingMode(..)
  , OverrideSpecMode(..)
  , OverrideValue(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , defaultCompileOptions
  , prepareWesl
  , prepareWeslWith
  , prepareWeslFile
  , prepareWeslFileWith
  , prepareWeslWithDiagnostics
  , prepareWeslFileWithDiagnostics
  , PackageInfo(..)
  , PackageDependency(..)
  , discoverPackageInfo
  , wesl
  , weslWith
  ) where

import Spirdo.Wesl.Compiler
import Spirdo.Wesl.Types
