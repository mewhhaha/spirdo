{-# LANGUAGE DataKinds #-}

-- | Public type surface (re-exports of interface, layout, uniform packing).
module Spirdo.Wesl.Types
  ( module Spirdo.Wesl.Types.Interface
  , module Spirdo.Wesl.Types.Layout
  , module Spirdo.Wesl.Types.Uniform
  , TTExpr(..)
  , Attr(..)
  , AttrArg(..)
  , DiagnosticSeverity(..)
  , CompileError(..)
  , Diagnostic(..)
  , CompileOptions(..)
  , SamplerBindingMode(..)
  , OverrideSpecMode(..)
  , OverrideValue(..)
  , defaultCompileOptions
  , FieldDecl(..)
  , StructDecl(..)
  ) where

import Data.Text (Text)
import Data.Word (Word32)

import Spirdo.Wesl.Types.Interface
import Spirdo.Wesl.Types.Layout
import Spirdo.Wesl.Types.Uniform

-- Compile-time attributes / diagnostics

-- | Translate-time boolean expression (used by @\@if@).
data TTExpr
  = TTVar Text
  | TTBool Bool
  | TTNot TTExpr
  | TTAnd TTExpr TTExpr
  | TTOr TTExpr TTExpr
  deriving (Eq, Show)

-- | Parsed attribute.
data Attr
  = Attr !Text ![AttrArg]
  | AttrIf !TTExpr
  deriving (Eq, Show)

-- | Attribute argument.
data AttrArg = AttrInt !Integer | AttrIdent !Text deriving (Eq, Show)

-- | Diagnostic severity.
data DiagnosticSeverity = DiagError | DiagWarning | DiagInfo | DiagOff
  deriving (Eq, Show, Read)

-- | Compiler error with optional source location.
data CompileError = CompileError
  { ceMessage :: !String
  , ceLine :: !(Maybe Int)
  , ceColumn :: !(Maybe Int)
  } deriving (Eq, Show)

-- | Non-fatal diagnostic (warning/info) with optional source location.
data Diagnostic = Diagnostic
  { diagSeverity :: !DiagnosticSeverity
  , diagRule :: !String
  , diagMessage :: !String
  , diagLine :: !(Maybe Int)
  , diagColumn :: !(Maybe Int)
  } deriving (Eq, Show)

-- | Compilation options for WESL → SPIR-V.
data CompileOptions = CompileOptions
  { spirvVersion :: Word32
  , enabledFeatures :: [String]
  , overrideValues :: [(String, OverrideValue)]
  , overrideSpecMode :: OverrideSpecMode
  , samplerBindingMode :: SamplerBindingMode
  , cacheEnabled :: Bool
  , cacheVerbose :: Bool
  , timingVerbose :: Bool
  }

-- | Override specialization mode.
data OverrideSpecMode
  = SpecStrict
  | SpecParity
  deriving (Eq, Show, Read)

-- | Override value used for specialization.
data OverrideValue
  = OVBool Bool
  | OVI32 Integer
  | OVU32 Integer
  | OVF32 Float
  | OVF16 Float
  | OVComposite [OverrideValue]
  deriving (Eq, Show, Read)

-- | Default options (SPIR-V 1.6, combined samplers, caching enabled).
defaultCompileOptions :: CompileOptions
defaultCompileOptions = CompileOptions 0x00010600 [] [] SpecStrict SamplerCombined True False False

-- | Parsed struct field (name, type, attributes).
data FieldDecl = FieldDecl
  { fdName :: !Text
  , fdType :: !Type
  , fdAttrs :: ![Attr]
  } deriving (Eq, Show)

-- | Parsed struct declaration.
data StructDecl = StructDecl
  { sdName :: !Text
  , sdFields :: ![FieldDecl]
  } deriving (Eq, Show)
