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

module Spirdo.Wesl
  ( -- * Compile-time interface types
    BindingKind(..)
  , StorageAccess(..)
  , StorageFormat(..)
  , ScalarType(..)
  , Field(..)
  , Ty(..)
  , Binding(..)
  , BindingDesc(..)
  , ReflectBindings(..)
  , Bindings(..)
  , bindingsFor
  , HasBinding
  , binding
  , HList(..)
  , InputFor
  , InputsOf
  , inputsFor
  , inputsForEither
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
  , ShaderInputs(..)
  , SDLUniform(..)
  , SDLSampler(..)
  , SDLTexture(..)
  , SDLStorageBuffer(..)
  , SDLStorageTexture(..)
  , SDLSamplerHandle(..)
  , SDLTextureHandle(..)
  , SDLBufferHandle(..)
  , emptyInputs
  , samplerBindings
  , uniformBindings
  , storageBufferBindings
  , storageTextureBindings
  , samplerBindingsFor
  , uniformBindingsFor
  , storageBufferBindingsFor
  , storageTextureBindingsFor
  , CompiledShader(..)
  , SomeCompiledShader(..)
  , ShaderInterface(..)
  , OverrideInfo(..)
  , BindingInfo(..)
  , TypeLayout(..)
  , FieldLayout(..)
  , specializableOverrides
  , CompileError(..)
  , CompileOptions(..)
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
  , PackageInfo(..)
  , PackageDependency(..)
  , discoverPackageInfo
  , wesl
  ) where

import Spirdo.Wesl.Compiler
import Spirdo.Wesl.Types
