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
  , Source(..)
  , CachePolicy(..)
  , Option(..)
  , defaultCompileOptions
  , defaultOptions
  , applyOptions
  , withSpirvVersion
  , withFeatures
  , withOverrides
  , withOverrideSpecMode
  , withSamplerMode
  , withEntryPoint
  , withCache
  , withCacheDir
  , withCacheVerbose
  , withTimingVerbose
  , FieldDecl(..)
  , StructDecl(..)
  ) where

import Data.Text (Text)
import Data.Word (Word32)
import Data.List (nub)

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
data AttrArg
  = AttrInt !Integer
  | AttrIdent !Text
  | AttrExpr !ConstExpr
  deriving (Eq, Show)

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
  , entryPointName :: Maybe String
  , cacheEnabled :: Bool
  , cacheDir :: FilePath
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

-- | Source input for compilation.
data Source
  = SourceInline
      { sourceName :: FilePath
      , sourceText :: String
      }
  | SourceFile FilePath
  deriving (Eq, Show, Read)

-- | Cache configuration for the compile pipeline.
data CachePolicy
  = CacheDisabled
  | CacheInDir FilePath
  deriving (Eq, Show, Read)

-- | Compile option overrides for ergonomic APIs.
data Option
  = OptSpirvVersion Word32
  | OptEnableFeature String
  | OptOverrides [(String, OverrideValue)]
  | OptOverrideSpecMode OverrideSpecMode
  | OptSamplerMode SamplerBindingMode
  | OptEntryPoint String
  | OptCache CachePolicy
  | OptCacheVerbose Bool
  | OptTimingVerbose Bool
  deriving (Eq, Show, Read)

-- | Default options (SPIR-V 1.6, combined samplers, caching enabled).
defaultCompileOptions :: CompileOptions
defaultCompileOptions =
  CompileOptions
    0x00010600
    []
    []
    SpecStrict
    SamplerCombined
    Nothing
    True
    "dist-newstyle/.wesl-cache"
    False
    False

-- | Default options alias for the ergonomic API.
defaultOptions :: CompileOptions
defaultOptions = defaultCompileOptions

-- | Apply a list of option overrides to compile options.
applyOptions :: [Option] -> CompileOptions -> CompileOptions
applyOptions opts0 base = foldl applyOne base opts0
  where
    applyOne opts opt =
      case opt of
        OptSpirvVersion v -> opts { spirvVersion = v }
        OptEnableFeature feat ->
          let feats = nub (feat : opts.enabledFeatures)
          in opts { enabledFeatures = feats }
        OptOverrides values -> opts { overrideValues = values }
        OptOverrideSpecMode mode -> opts { overrideSpecMode = mode }
        OptSamplerMode mode -> opts { samplerBindingMode = mode }
        OptEntryPoint name -> opts { entryPointName = Just name }
        OptCache CacheDisabled -> opts { cacheEnabled = False }
        OptCache (CacheInDir dir) -> opts { cacheEnabled = True, cacheDir = dir }
        OptCacheVerbose verbose -> opts { cacheVerbose = verbose }
        OptTimingVerbose verbose -> opts { timingVerbose = verbose }

-- | Set the SPIR-V version (default is 1.6).
withSpirvVersion :: Word32 -> CompileOptions -> CompileOptions
withSpirvVersion v opts = opts { spirvVersion = v }

-- | Enable WGSL/WESL feature toggles (e.g. translate-time @\@if@ flags).
withFeatures :: [String] -> CompileOptions -> CompileOptions
withFeatures feats opts = opts { enabledFeatures = feats }

-- | Provide override specialization values.
withOverrides :: [(String, OverrideValue)] -> CompileOptions -> CompileOptions
withOverrides values opts = opts { overrideValues = values }

-- | Choose override specialization mode (validator-friendly vs parity).
withOverrideSpecMode :: OverrideSpecMode -> CompileOptions -> CompileOptions
withOverrideSpecMode mode opts = opts { overrideSpecMode = mode }

-- | Select sampler binding mode (combined vs separate).
withSamplerMode :: SamplerBindingMode -> CompileOptions -> CompileOptions
withSamplerMode mode opts = opts { samplerBindingMode = mode }

-- | Select a specific entry point by name when a module defines multiple entry points.
withEntryPoint :: String -> CompileOptions -> CompileOptions
withEntryPoint name opts = opts { entryPointName = Just name }

-- | Enable or disable the on-disk WESL cache.
withCache :: Bool -> CompileOptions -> CompileOptions
withCache enabled opts = opts { cacheEnabled = enabled }

-- | Set the cache directory used for compile-time caching.
withCacheDir :: FilePath -> CompileOptions -> CompileOptions
withCacheDir dir opts = opts { cacheDir = dir }

-- | Toggle cache logging.
withCacheVerbose :: Bool -> CompileOptions -> CompileOptions
withCacheVerbose verbose opts = opts { cacheVerbose = verbose }

-- | Toggle compiler timing output.
withTimingVerbose :: Bool -> CompileOptions -> CompileOptions
withTimingVerbose verbose opts = opts { timingVerbose = verbose }

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
