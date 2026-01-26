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

data TTExpr
  = TTVar Text
  | TTBool Bool
  | TTNot TTExpr
  | TTAnd TTExpr TTExpr
  | TTOr TTExpr TTExpr
  deriving (Eq, Show)

data Attr
  = Attr !Text ![AttrArg]
  | AttrIf !TTExpr
  deriving (Eq, Show)

data AttrArg = AttrInt !Integer | AttrIdent !Text deriving (Eq, Show)

data DiagnosticSeverity = DiagError | DiagWarning | DiagInfo | DiagOff
  deriving (Eq, Show, Read)

data CompileError = CompileError
  { ceMessage :: !String
  , ceLine :: !(Maybe Int)
  , ceColumn :: !(Maybe Int)
  } deriving (Eq, Show)

data Diagnostic = Diagnostic
  { diagSeverity :: !DiagnosticSeverity
  , diagRule :: !String
  , diagMessage :: !String
  , diagLine :: !(Maybe Int)
  , diagColumn :: !(Maybe Int)
  } deriving (Eq, Show)

data CompileOptions = CompileOptions
  { spirvVersion :: Word32
  , enabledFeatures :: [String]
  , overrideValues :: [(String, OverrideValue)]
  , overrideSpecMode :: OverrideSpecMode
  , cacheEnabled :: Bool
  , cacheVerbose :: Bool
  , timingVerbose :: Bool
  }

data OverrideSpecMode
  = SpecStrict
  | SpecParity
  deriving (Eq, Show, Read)

data OverrideValue
  = OVBool Bool
  | OVI32 Integer
  | OVU32 Integer
  | OVF32 Float
  | OVF16 Float
  | OVComposite [OverrideValue]
  deriving (Eq, Show, Read)

-- Default to SPIR-V 1.6 (0x00010600). Override if needed.
defaultCompileOptions :: CompileOptions
defaultCompileOptions = CompileOptions 0x00010600 [] [] SpecStrict True False False

data FieldDecl = FieldDecl
  { fdName :: !Text
  , fdType :: !Type
  , fdAttrs :: ![Attr]
  } deriving (Eq, Show)

data StructDecl = StructDecl
  { sdName :: !Text
  , sdFields :: ![FieldDecl]
  } deriving (Eq, Show)
