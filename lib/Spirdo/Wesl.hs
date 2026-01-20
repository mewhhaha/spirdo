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

import Control.Exception (SomeException, catch)
import Control.Monad (foldM, zipWithM, zipWithM_, when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, ord)
import Data.Either (partitionEithers)
import Data.Int (Int32)
import Data.List (find, intercalate, isPrefixOf, mapAccumL, partition)
import qualified Data.Kind as K
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import GHC.Generics (Generic, Rep, K1(..), M1(..), (:*:)(..), Selector, selName, S, from)
import Language.Haskell.TH (Exp, Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (dropExtension, isRelative, makeRelative, normalise, splitDirectories, takeDirectory, (<.>), (</>))
import Text.Read (readMaybe)

-- Type-level interface representation

data BindingKind
  = BUniform
  | BStorageRead
  | BStorageReadWrite
  | BSampler
  | BSamplerComparison
  | BTexture1D
  | BTexture1DArray
  | BTexture2D
  | BTexture2DArray
  | BTexture3D
  | BTextureCube
  | BTextureCubeArray
  | BTextureMultisampled2D
  | BTextureDepth2D
  | BTextureDepth2DArray
  | BTextureDepthCube
  | BTextureDepthCubeArray
  | BTextureDepthMultisampled2D
  | BStorageTexture1D
  | BStorageTexture2D
  | BStorageTexture2DArray
  | BStorageTexture3D
  deriving (Eq, Show, Read)

data StorageAccess = StorageRead | StorageWrite | StorageReadWrite
  deriving (Eq, Show, Read)

data StorageFormat
  = FormatRgba8Unorm
  | FormatRgba8Snorm
  | FormatRgba8Uint
  | FormatRgba8Sint
  | FormatRgba16Float
  | FormatRgba16Uint
  | FormatRgba16Sint
  | FormatRgba16Unorm
  | FormatRgba16Snorm
  | FormatRgba32Float
  | FormatRgba32Uint
  | FormatRgba32Sint
  | FormatR32Float
  | FormatR32Uint
  | FormatR32Sint
  | FormatR16Float
  | FormatR16Uint
  | FormatR16Sint
  | FormatR16Unorm
  | FormatR16Snorm
  | FormatR8Unorm
  | FormatR8Snorm
  | FormatR8Uint
  | FormatR8Sint
  | FormatRg32Float
  | FormatRg32Uint
  | FormatRg32Sint
  | FormatRg16Float
  | FormatRg16Uint
  | FormatRg16Sint
  | FormatRg16Unorm
  | FormatRg16Snorm
  | FormatRg8Unorm
  | FormatRg8Snorm
  | FormatRg8Uint
  | FormatRg8Sint
  | FormatRgb10a2Unorm
  | FormatRgb10a2Uint
  | FormatRg11b10Float
  deriving (Eq, Show, Read)

data ScalarType = SI32 | SU32 | SF16 | SF32 | SBool
  deriving (Eq, Show, Read)

data Field = Field Symbol Ty

data Ty
  = TScalar ScalarType
  | TVec Nat ScalarType
  | TMatrix Nat Nat ScalarType
  | TArray Nat Ty
  | TRuntimeArray Ty
  | TStruct [Field]
  | TSampler
  | TSamplerComparison
  | TTexture1D ScalarType
  | TTexture1DArray ScalarType
  | TTexture2D ScalarType
  | TTexture2DArray ScalarType
  | TTexture3D ScalarType
  | TTextureCube ScalarType
  | TTextureCubeArray ScalarType
  | TTextureMultisampled2D ScalarType
  | TTextureDepth2D
  | TTextureDepth2DArray
  | TTextureDepthCube
  | TTextureDepthCubeArray
  | TTextureDepthMultisampled2D
  | TStorageTexture1D StorageFormat StorageAccess
  | TStorageTexture2D StorageFormat StorageAccess
  | TStorageTexture2DArray StorageFormat StorageAccess
  | TStorageTexture3D StorageFormat StorageAccess
  | TAtomic ScalarType

-- | Type-level binding descriptor: name, kind, set, binding, type.
data Binding = Binding Symbol BindingKind Nat Nat Ty

data BindingDesc = BindingDesc
  { descName :: !String
  , descKind :: !BindingKind
  , descGroup :: !Word32
  , descBinding :: !Word32
  } deriving (Eq, Show, Read)

class KnownBindingKind (k :: BindingKind) where
  bindingKindVal :: Proxy k -> BindingKind

instance KnownBindingKind 'BUniform where
  bindingKindVal _ = BUniform

instance KnownBindingKind 'BStorageRead where
  bindingKindVal _ = BStorageRead

instance KnownBindingKind 'BStorageReadWrite where
  bindingKindVal _ = BStorageReadWrite

instance KnownBindingKind 'BSampler where
  bindingKindVal _ = BSampler

instance KnownBindingKind 'BSamplerComparison where
  bindingKindVal _ = BSamplerComparison

instance KnownBindingKind 'BTexture1D where
  bindingKindVal _ = BTexture1D

instance KnownBindingKind 'BTexture1DArray where
  bindingKindVal _ = BTexture1DArray

instance KnownBindingKind 'BTexture2D where
  bindingKindVal _ = BTexture2D

instance KnownBindingKind 'BTexture2DArray where
  bindingKindVal _ = BTexture2DArray

instance KnownBindingKind 'BTexture3D where
  bindingKindVal _ = BTexture3D

instance KnownBindingKind 'BTextureCube where
  bindingKindVal _ = BTextureCube

instance KnownBindingKind 'BTextureCubeArray where
  bindingKindVal _ = BTextureCubeArray

instance KnownBindingKind 'BTextureMultisampled2D where
  bindingKindVal _ = BTextureMultisampled2D

instance KnownBindingKind 'BTextureDepth2D where
  bindingKindVal _ = BTextureDepth2D

instance KnownBindingKind 'BTextureDepth2DArray where
  bindingKindVal _ = BTextureDepth2DArray

instance KnownBindingKind 'BTextureDepthCube where
  bindingKindVal _ = BTextureDepthCube

instance KnownBindingKind 'BTextureDepthCubeArray where
  bindingKindVal _ = BTextureDepthCubeArray

instance KnownBindingKind 'BTextureDepthMultisampled2D where
  bindingKindVal _ = BTextureDepthMultisampled2D

instance KnownBindingKind 'BStorageTexture1D where
  bindingKindVal _ = BStorageTexture1D

instance KnownBindingKind 'BStorageTexture2D where
  bindingKindVal _ = BStorageTexture2D

instance KnownBindingKind 'BStorageTexture2DArray where
  bindingKindVal _ = BStorageTexture2DArray

instance KnownBindingKind 'BStorageTexture3D where
  bindingKindVal _ = BStorageTexture3D

class ReflectBinding (b :: Binding) where
  reflectBinding :: Proxy b -> BindingDesc

instance (KnownSymbol name, KnownBindingKind kind, KnownNat set, KnownNat binding) => ReflectBinding ('Binding name kind set binding ty) where
  reflectBinding _ =
    BindingDesc
      { descName = symbolVal (Proxy @name)
      , descKind = bindingKindVal (Proxy @kind)
      , descGroup = fromIntegral (natVal (Proxy @set))
      , descBinding = fromIntegral (natVal (Proxy @binding))
      }

class ReflectBindings (iface :: [Binding]) where
  reflectBindings :: Proxy iface -> [BindingDesc]

instance ReflectBindings '[] where
  reflectBindings _ = []

instance (ReflectBinding b, ReflectBindings bs) => ReflectBindings (b ': bs) where
  reflectBindings _ = reflectBinding (Proxy @b) : reflectBindings (Proxy @bs)

-- | Typed binding list for an interface.
newtype Bindings (iface :: [Binding]) = Bindings
  { unBindings :: [BindingDesc]
  }

bindingsFor :: forall iface. ReflectBindings iface => CompiledShader iface -> Bindings iface
bindingsFor _ = Bindings (reflectBindings (Proxy @iface))

type family HasBinding (name :: Symbol) (iface :: [Binding]) :: Bool where
  HasBinding _ '[] = 'False
  HasBinding name ('Binding name _ _ _ _ ': _) = 'True
  HasBinding name (_ ': rest) = HasBinding name rest

binding :: forall name iface. (KnownSymbol name, HasBinding name iface ~ 'True, ReflectBindings iface) => CompiledShader iface -> BindingDesc
binding _ =
  let key = symbolVal (Proxy @name)
  in case find (\b -> descName b == key) (reflectBindings (Proxy @iface)) of
      Just b -> b
      Nothing -> error ("binding: missing " <> key)

-- Typed inputs derived from the interface.

data HList (xs :: [K.Type]) where
  HNil :: HList '[]
  (:&) :: x -> HList xs -> HList (x ': xs)

infixr 5 :&

data BindingCategory
  = CatUniform
  | CatSampler
  | CatTexture
  | CatStorageBuffer
  | CatStorageTexture

type family CategoryOf (k :: BindingKind) :: BindingCategory where
  CategoryOf 'BUniform = 'CatUniform
  CategoryOf 'BSampler = 'CatSampler
  CategoryOf 'BSamplerComparison = 'CatSampler
  CategoryOf 'BStorageRead = 'CatStorageBuffer
  CategoryOf 'BStorageReadWrite = 'CatStorageBuffer
  CategoryOf 'BStorageTexture1D = 'CatStorageTexture
  CategoryOf 'BStorageTexture2D = 'CatStorageTexture
  CategoryOf 'BStorageTexture2DArray = 'CatStorageTexture
  CategoryOf 'BStorageTexture3D = 'CatStorageTexture
  CategoryOf _ = 'CatTexture

type family InputForCategory (c :: BindingCategory) :: K.Type where
  InputForCategory 'CatUniform = UniformValue
  InputForCategory 'CatSampler = SDLSamplerHandle
  InputForCategory 'CatTexture = SDLTextureHandle
  InputForCategory 'CatStorageBuffer = SDLBufferHandle
  InputForCategory 'CatStorageTexture = SDLTextureHandle

type family InputForKind (k :: BindingKind) :: K.Type where
  InputForKind k = InputForCategory (CategoryOf k)

type family InputFor (b :: Binding) :: K.Type where
  InputFor ('Binding _ kind _ _ _) = InputForKind kind

type family InputsOf (iface :: [Binding]) :: [K.Type] where
  InputsOf '[] = '[]
  InputsOf (b ': bs) = InputFor b ': InputsOf bs

-- | Typed uniform values for packing.
data Half = Half !Word16
  deriving (Eq, Show)

data V2 a = V2 !a !a
  deriving (Eq, Show)

data V3 a = V3 !a !a !a
  deriving (Eq, Show)

data V4 a = V4 !a !a !a !a
  deriving (Eq, Show)

data M2 a = M2 !(V2 a) !(V2 a)
  deriving (Eq, Show)

data M3 a = M3 !(V3 a) !(V3 a) !(V3 a)
  deriving (Eq, Show)

data M4 a = M4 !(V4 a) !(V4 a) !(V4 a) !(V4 a)
  deriving (Eq, Show)

data ScalarValue
  = SVI32 !Int32
  | SVU32 !Word32
  | SVF16 !Word16
  | SVF32 !Float
  | SVBool !Bool
  deriving (Eq, Show)

data UniformValue
  = UVScalar !ScalarValue
  | UVVector !Int ![ScalarValue]
  | UVMatrix !Int !Int ![ScalarValue]
  | UVArray ![UniformValue]
  | UVStruct ![(String, UniformValue)]
  deriving (Eq, Show)

class ToScalar a where
  toScalar :: a -> ScalarValue

instance ToScalar Float where
  toScalar = SVF32

instance ToScalar Int32 where
  toScalar = SVI32

instance ToScalar Word32 where
  toScalar = SVU32

instance ToScalar Bool where
  toScalar = SVBool

instance ToScalar Half where
  toScalar (Half w) = SVF16 w

class ToUniform a where
  toUniform :: a -> UniformValue
  default toUniform :: (Generic a, GUniform (Rep a)) => a -> UniformValue
  toUniform a = UVStruct (gUniform (from a))

uniform :: ToUniform a => a -> UniformValue
uniform = toUniform

instance ToUniform Float where
  toUniform = UVScalar . toScalar

instance ToUniform Int32 where
  toUniform = UVScalar . toScalar

instance ToUniform Word32 where
  toUniform = UVScalar . toScalar

instance ToUniform Bool where
  toUniform = UVScalar . toScalar

instance ToUniform Half where
  toUniform = UVScalar . toScalar

instance ToScalar a => ToUniform (V2 a) where
  toUniform (V2 a b) = UVVector 2 [toScalar a, toScalar b]

instance ToScalar a => ToUniform (V3 a) where
  toUniform (V3 a b c) = UVVector 3 [toScalar a, toScalar b, toScalar c]

instance ToScalar a => ToUniform (V4 a) where
  toUniform (V4 a b c d) = UVVector 4 [toScalar a, toScalar b, toScalar c, toScalar d]

instance ToScalar a => ToUniform (M2 a) where
  toUniform (M2 c0 c1) =
    case (toUniform c0, toUniform c1) of
      (UVVector _ v0, UVVector _ v1) -> UVMatrix 2 2 (v0 <> v1)
      _ -> error "ToUniform M2: invalid column vectors"

instance ToScalar a => ToUniform (M3 a) where
  toUniform (M3 c0 c1 c2) =
    case (toUniform c0, toUniform c1, toUniform c2) of
      (UVVector _ v0, UVVector _ v1, UVVector _ v2) -> UVMatrix 3 3 (v0 <> v1 <> v2)
      _ -> error "ToUniform M3: invalid column vectors"

instance ToScalar a => ToUniform (M4 a) where
  toUniform (M4 c0 c1 c2 c3) =
    case (toUniform c0, toUniform c1, toUniform c2, toUniform c3) of
      (UVVector _ v0, UVVector _ v1, UVVector _ v2, UVVector _ v3) -> UVMatrix 4 4 (v0 <> v1 <> v2 <> v3)
      _ -> error "ToUniform M4: invalid column vectors"

instance ToUniform a => ToUniform [a] where
  toUniform xs = UVArray (map toUniform xs)

class GUniform f where
  gUniform :: f p -> [(String, UniformValue)]

instance (GUniform a, GUniform b) => GUniform (a :*: b) where
  gUniform (a :*: b) = gUniform a <> gUniform b

instance {-# OVERLAPPABLE #-} (GUniform a) => GUniform (M1 i c a) where
  gUniform (M1 x) = gUniform x

instance {-# OVERLAPPING #-} (Selector s, ToUniform a) => GUniform (M1 S s (K1 i a)) where
  gUniform m1 =
    let name = selName m1
    in if null name
        then error "ToUniform: expected record fields (missing selector name)"
        else [(name, toUniform (unK1 (unM1 m1)))]

-- | Minimal SDL-like handles (replace with real SDL3.4 handles in your app).
newtype SDLSamplerHandle = SDLSamplerHandle Word64
  deriving (Eq, Show)

newtype SDLTextureHandle = SDLTextureHandle Word64
  deriving (Eq, Show)

newtype SDLBufferHandle = SDLBufferHandle Word64
  deriving (Eq, Show)

data SDLUniform = SDLUniform
  { sdlUniformName :: !String
  , sdlUniformGroup :: !Word32
  , sdlUniformBinding :: !Word32
  , sdlUniformBytes :: !ByteString
  } deriving (Eq, Show)

data SDLSampler = SDLSampler
  { sdlSamplerName :: !String
  , sdlSamplerGroup :: !Word32
  , sdlSamplerBinding :: !Word32
  , sdlSamplerHandle :: !SDLSamplerHandle
  } deriving (Eq, Show)

data SDLTexture = SDLTexture
  { sdlTextureName :: !String
  , sdlTextureGroup :: !Word32
  , sdlTextureBinding :: !Word32
  , sdlTextureHandle :: !SDLTextureHandle
  } deriving (Eq, Show)

data SDLStorageBuffer = SDLStorageBuffer
  { sdlStorageBufferName :: !String
  , sdlStorageBufferGroup :: !Word32
  , sdlStorageBufferBinding :: !Word32
  , sdlStorageBufferHandle :: !SDLBufferHandle
  } deriving (Eq, Show)

data SDLStorageTexture = SDLStorageTexture
  { sdlStorageTextureName :: !String
  , sdlStorageTextureGroup :: !Word32
  , sdlStorageTextureBinding :: !Word32
  , sdlStorageTextureHandle :: !SDLTextureHandle
  } deriving (Eq, Show)

data ShaderInputs (iface :: [Binding]) = ShaderInputs
  { siShader :: !(CompiledShader iface)
  , siUniforms :: ![SDLUniform]
  , siSamplers :: ![SDLSampler]
  , siTextures :: ![SDLTexture]
  , siStorageBuffers :: ![SDLStorageBuffer]
  , siStorageTextures :: ![SDLStorageTexture]
  }

emptyInputs :: CompiledShader iface -> ShaderInputs iface
emptyInputs shader =
  ShaderInputs
    { siShader = shader
    , siUniforms = []
    , siSamplers = []
    , siTextures = []
    , siStorageBuffers = []
    , siStorageTextures = []
    }

lookupBindingInfo :: String -> ShaderInterface -> Either String BindingInfo
lookupBindingInfo name iface =
  case find (\b -> biName b == name) (siBindings iface) of
    Just info -> Right info
    Nothing -> Left ("binding not found in interface: " <> name)

type ByteMap = IntMap.IntMap Word8

packUniform :: TypeLayout -> UniformValue -> Either String ByteString
packUniform layout value = do
  bytes <- writeUniform 0 layout value
  let size = fromIntegral (layoutSize layout)
  let out = [IntMap.findWithDefault 0 i bytes | i <- [0 .. size - 1]]
  pure (BS.pack out)

writeUniform :: Int -> TypeLayout -> UniformValue -> Either String ByteMap
writeUniform off layout value =
  case (layout, value) of
    (TLScalar s _ _, UVScalar v) -> do
      bs <- scalarValueBytes s v
      pure (bytesAt off bs)
    (TLVector n s _ _, UVVector n' vals)
      | n == n' -> foldM (writeScalarAt s off) IntMap.empty (zip [0 ..] vals)
      | otherwise -> Left ("vector length mismatch: expected " <> show n <> ", got " <> show n')
    (TLMatrix cols rows s _ _ stride, UVMatrix c r vals)
      | cols == c && rows == r -> writeMatrix off cols rows s (fromIntegral stride) vals
      | otherwise ->
          Left ("matrix size mismatch: expected " <> show cols <> "x" <> show rows <> ", got " <> show c <> "x" <> show r)
    (TLArray mlen stride elemLayout _ _, UVArray elems) -> do
      case mlen of
        Just n | n /= length elems ->
          Left ("array length mismatch: expected " <> show n <> ", got " <> show (length elems))
        _ -> pure ()
      foldM
        (\acc (ix, el) -> do
            bytes <- writeUniform (off + ix * fromIntegral stride) elemLayout el
            pure (IntMap.union bytes acc)
        )
        IntMap.empty
        (zip [0 ..] elems)
    (TLStruct _ fields _ _, UVStruct vals) -> do
      let valMap = Map.fromList vals
      foldM
        (\acc fld -> do
            val <- case Map.lookup (flName fld) valMap of
              Just v -> Right v
              Nothing -> Left ("missing struct field: " <> flName fld)
            bytes <- writeUniform (off + fromIntegral (flOffset fld)) (flType fld) val
            pure (IntMap.union bytes acc)
        )
        IntMap.empty
        fields
    _ -> Left ("uniform value does not match layout: " <> show layout)

writeScalarAt :: Scalar -> Int -> ByteMap -> (Int, ScalarValue) -> Either String ByteMap
writeScalarAt s off acc (ix, val) = do
  bs <- scalarValueBytes s val
  let elemSize = scalarByteSize s
  let bytes = bytesAt (off + ix * elemSize) bs
  pure (IntMap.union bytes acc)

writeMatrix :: Int -> Int -> Int -> Scalar -> Int -> [ScalarValue] -> Either String ByteMap
writeMatrix off cols rows scalar stride vals = do
  let expected = cols * rows
  when (length vals /= expected) $
    Left ("matrix value count mismatch: expected " <> show expected <> ", got " <> show (length vals))
  foldM
    (\acc (ix, val) -> do
        bs <- scalarValueBytes scalar val
        let col = ix `div` rows
        let row = ix `mod` rows
        let base = off + col * stride + row * scalarByteSize scalar
        pure (IntMap.union (bytesAt base bs) acc)
    )
    IntMap.empty
    (zip [0 ..] vals)

scalarValueBytes :: Scalar -> ScalarValue -> Either String [Word8]
scalarValueBytes scalar value =
  case (scalar, value) of
    (I32, SVI32 v) -> Right (word32LE (fromIntegral v))
    (U32, SVU32 v) -> Right (word32LE v)
    (F32, SVF32 v) -> Right (word32LE (castFloatToWord32 v))
    (F16, SVF16 v) -> Right (word16LE v)
    (Bool, SVBool v) -> Right (word32LE (if v then 1 else 0))
    _ -> Left ("scalar type mismatch: expected " <> show scalar <> ", got " <> show value)

scalarByteSize :: Scalar -> Int
scalarByteSize s = case s of
  F16 -> 2
  _ -> 4

word16LE :: Word16 -> [Word8]
word16LE w =
  [ fromIntegral (w .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  ]

word32LE :: Word32 -> [Word8]
word32LE w =
  [ fromIntegral (w .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 24) .&. 0xFF)
  ]

bytesAt :: Int -> [Word8] -> ByteMap
bytesAt off bytes =
  IntMap.fromList (zip [off ..] bytes)

class ApplyCategory (c :: BindingCategory) where
  applyCategory :: BindingInfo -> InputForCategory c -> ShaderInputs iface -> Either String (ShaderInputs iface)

instance ApplyCategory 'CatUniform where
  applyCategory info val inputs = do
    bytes <- packUniform (biType info) val
    let entry =
          SDLUniform
            { sdlUniformName = biName info
            , sdlUniformGroup = biGroup info
            , sdlUniformBinding = biBinding info
            , sdlUniformBytes = bytes
            }
    pure inputs { siUniforms = entry : siUniforms inputs }

instance ApplyCategory 'CatSampler where
  applyCategory info handle inputs =
    let entry =
          SDLSampler
            { sdlSamplerName = biName info
            , sdlSamplerGroup = biGroup info
            , sdlSamplerBinding = biBinding info
            , sdlSamplerHandle = handle
            }
    in Right inputs { siSamplers = entry : siSamplers inputs }

instance ApplyCategory 'CatTexture where
  applyCategory info handle inputs =
    let entry =
          SDLTexture
            { sdlTextureName = biName info
            , sdlTextureGroup = biGroup info
            , sdlTextureBinding = biBinding info
            , sdlTextureHandle = handle
            }
    in Right inputs { siTextures = entry : siTextures inputs }

instance ApplyCategory 'CatStorageBuffer where
  applyCategory info handle inputs =
    let entry =
          SDLStorageBuffer
            { sdlStorageBufferName = biName info
            , sdlStorageBufferGroup = biGroup info
            , sdlStorageBufferBinding = biBinding info
            , sdlStorageBufferHandle = handle
            }
    in Right inputs { siStorageBuffers = entry : siStorageBuffers inputs }

instance ApplyCategory 'CatStorageTexture where
  applyCategory info handle inputs =
    let entry =
          SDLStorageTexture
            { sdlStorageTextureName = biName info
            , sdlStorageTextureGroup = biGroup info
            , sdlStorageTextureBinding = biBinding info
            , sdlStorageTextureHandle = handle
            }
    in Right inputs { siStorageTextures = entry : siStorageTextures inputs }

class ApplyKind (k :: BindingKind) where
  applyKind :: BindingInfo -> InputForKind k -> ShaderInputs iface -> Either String (ShaderInputs iface)

instance ApplyCategory (CategoryOf k) => ApplyKind k where
  applyKind = applyCategory @(CategoryOf k)

class BuildInputsWith (iface :: [Binding]) (bs :: [Binding]) where
  buildInputsWith :: CompiledShader iface -> HList (InputsOf bs) -> Either String (ShaderInputs iface)

instance BuildInputsWith iface '[] where
  buildInputsWith shader HNil = Right (emptyInputs shader)

instance
  forall iface name kind set binding ty bs.
  ( KnownSymbol name
  , KnownBindingKind kind
  , KnownNat set
  , KnownNat binding
  , ApplyKind kind
  , BuildInputsWith iface bs
  ) =>
  BuildInputsWith iface ('Binding name kind set binding ty ': bs)
  where
  buildInputsWith shader (x :& xs) = do
    base <- buildInputsWith @iface @bs shader xs
    let name = symbolVal (Proxy @name)
    info <- lookupBindingInfo name (shaderInterface shader)
    applyKind @kind info x base

inputsForEither :: forall iface. BuildInputsWith iface iface => CompiledShader iface -> HList (InputsOf iface) -> Either String (ShaderInputs iface)
inputsForEither shader xs = buildInputsWith @iface @iface shader xs

inputsFor :: forall iface. BuildInputsWith iface iface => CompiledShader iface -> HList (InputsOf iface) -> ShaderInputs iface
inputsFor shader xs =
  case inputsForEither @iface shader xs of
    Left err -> error ("inputsFor: " <> err)
    Right ok -> ok

samplerBindings :: forall iface. ReflectBindings iface => Proxy iface -> [BindingDesc]
samplerBindings _ =
  filter
    (\b ->
      descKind b `elem`
        [ BSampler
        , BSamplerComparison
        , BTexture1D
        , BTexture1DArray
        , BTexture2D
        , BTexture2DArray
        , BTexture3D
        , BTextureCube
        , BTextureCubeArray
        , BTextureMultisampled2D
        , BTextureDepth2D
        , BTextureDepth2DArray
        , BTextureDepthCube
        , BTextureDepthCubeArray
        , BTextureDepthMultisampled2D
        ])
    (reflectBindings (Proxy @iface))

uniformBindings :: forall iface. ReflectBindings iface => Proxy iface -> [BindingDesc]
uniformBindings _ =
  filter (\b -> descKind b == BUniform) (reflectBindings (Proxy @iface))

storageBufferBindings :: forall iface. ReflectBindings iface => Proxy iface -> [BindingDesc]
storageBufferBindings _ =
  filter (\b -> descKind b == BStorageRead || descKind b == BStorageReadWrite) (reflectBindings (Proxy @iface))

storageTextureBindings :: forall iface. ReflectBindings iface => Proxy iface -> [BindingDesc]
storageTextureBindings _ =
  filter (\b -> descKind b `elem` [BStorageTexture1D, BStorageTexture2D, BStorageTexture2DArray, BStorageTexture3D]) (reflectBindings (Proxy @iface))

samplerBindingsFor :: forall iface. ReflectBindings iface => CompiledShader iface -> [BindingDesc]
samplerBindingsFor _ = samplerBindings (Proxy @iface)

uniformBindingsFor :: forall iface. ReflectBindings iface => CompiledShader iface -> [BindingDesc]
uniformBindingsFor _ = uniformBindings (Proxy @iface)

storageBufferBindingsFor :: forall iface. ReflectBindings iface => CompiledShader iface -> [BindingDesc]
storageBufferBindingsFor _ = storageBufferBindings (Proxy @iface)

storageTextureBindingsFor :: forall iface. ReflectBindings iface => CompiledShader iface -> [BindingDesc]
storageTextureBindingsFor _ = storageTextureBindings (Proxy @iface)

-- Runtime interface representation

data Scalar = I32 | U32 | F16 | F32 | Bool
  deriving (Eq, Show, Read)

data Type
  = TyScalar Scalar
  | TyVector Int Scalar
  | TyMatrix Int Int Scalar
  | TyArray Type (Maybe Int)
  | TyStructRef Text
  | TySampler
  | TySamplerComparison
  | TyTexture1D Scalar
  | TyTexture1DArray Scalar
  | TyTexture2D Scalar
  | TyTexture2DArray Scalar
  | TyTexture3D Scalar
  | TyTextureCube Scalar
  | TyTextureCubeArray Scalar
  | TyTextureMultisampled2D Scalar
  | TyTextureDepth2D
  | TyTextureDepth2DArray
  | TyTextureDepthCube
  | TyTextureDepthCubeArray
  | TyTextureDepthMultisampled2D
  | TyStorageTexture1D StorageFormat StorageAccess
  | TyStorageTexture2D StorageFormat StorageAccess
  | TyStorageTexture2DArray StorageFormat StorageAccess
  | TyStorageTexture3D StorageFormat StorageAccess
  | TyAtomic Scalar
  | TyPtr Text (Maybe StorageAccess) Type
  deriving (Eq, Show)

data FieldDecl = FieldDecl
  { fdName :: !Text
  , fdType :: !Type
  , fdAttrs :: ![Attr]
  } deriving (Eq, Show)

data StructDecl = StructDecl
  { sdName :: !Text
  , sdFields :: ![FieldDecl]
  } deriving (Eq, Show)

data BindingInfo = BindingInfo
  { biName :: !String
  , biKind :: !BindingKind
  , biGroup :: !Word32
  , biBinding :: !Word32
  , biType :: !TypeLayout
  } deriving (Eq, Show, Read)

data FieldLayout = FieldLayout
  { flName :: !String
  , flOffset :: !Word32
  , flType :: !TypeLayout
  , flAlign :: !Word32
  , flSize :: !Word32
  } deriving (Eq, Show, Read)

data TypeLayout
  = TLScalar !Scalar !Word32 !Word32
  | TLVector !Int !Scalar !Word32 !Word32
  | TLMatrix !Int !Int !Scalar !Word32 !Word32 !Word32
  | TLArray !(Maybe Int) !Word32 !TypeLayout !Word32 !Word32
  | TLStruct !String ![FieldLayout] !Word32 !Word32
  | TLSampler
  | TLSamplerComparison
  | TLTexture1D !Scalar
  | TLTexture1DArray !Scalar
  | TLTexture2D !Scalar
  | TLTexture2DArray !Scalar
  | TLTexture3D !Scalar
  | TLTextureCube !Scalar
  | TLTextureCubeArray !Scalar
  | TLTextureMultisampled2D !Scalar
  | TLTextureDepth2D
  | TLTextureDepth2DArray
  | TLTextureDepthCube
  | TLTextureDepthCubeArray
  | TLTextureDepthMultisampled2D
  | TLStorageTexture1D !StorageFormat !StorageAccess
  | TLStorageTexture2D !StorageFormat !StorageAccess
  | TLStorageTexture2DArray !StorageFormat !StorageAccess
  | TLStorageTexture3D !StorageFormat !StorageAccess
  | TLAtomic !Scalar
  | TLPointer !Word32 !(Maybe StorageAccess) !TypeLayout
  deriving (Eq, Show, Read)

layoutAlign :: TypeLayout -> Word32
layoutAlign tl = case tl of
  TLScalar _ a _ -> a
  TLVector _ _ a _ -> a
  TLMatrix _ _ _ a _ _ -> a
  TLArray _ _ _ a _ -> a
  TLStruct _ _ a _ -> a
  TLPointer _ _ _ -> 0
  TLSampler -> 0
  TLSamplerComparison -> 0
  TLTexture1D _ -> 0
  TLTexture1DArray _ -> 0
  TLTexture2D _ -> 0
  TLTexture2DArray _ -> 0
  TLTexture3D _ -> 0
  TLTextureCube _ -> 0
  TLTextureCubeArray _ -> 0
  TLTextureMultisampled2D _ -> 0
  TLTextureDepth2D -> 0
  TLTextureDepth2DArray -> 0
  TLTextureDepthCube -> 0
  TLTextureDepthCubeArray -> 0
  TLTextureDepthMultisampled2D -> 0
  TLStorageTexture1D _ _ -> 0
  TLStorageTexture2D _ _ -> 0
  TLStorageTexture2DArray _ _ -> 0
  TLStorageTexture3D _ _ -> 0
  TLAtomic _ -> 4

layoutSize :: TypeLayout -> Word32
layoutSize tl = case tl of
  TLScalar _ _ s -> s
  TLVector _ _ _ s -> s
  TLMatrix _ _ _ _ s _ -> s
  TLArray _ _ _ _ s -> s
  TLStruct _ _ _ s -> s
  TLPointer _ _ _ -> 0
  TLSampler -> 0
  TLSamplerComparison -> 0
  TLTexture1D _ -> 0
  TLTexture1DArray _ -> 0
  TLTexture2D _ -> 0
  TLTexture2DArray _ -> 0
  TLTexture3D _ -> 0
  TLTextureCube _ -> 0
  TLTextureCubeArray _ -> 0
  TLTextureMultisampled2D _ -> 0
  TLTextureDepth2D -> 0
  TLTextureDepth2DArray -> 0
  TLTextureDepthCube -> 0
  TLTextureDepthCubeArray -> 0
  TLTextureDepthMultisampled2D -> 0
  TLStorageTexture1D _ _ -> 0
  TLStorageTexture2D _ _ -> 0
  TLStorageTexture2DArray _ _ -> 0
  TLStorageTexture3D _ _ -> 0
  TLAtomic _ -> 4

scalarLayout :: Scalar -> (Word32, Word32)
scalarLayout s =
  case s of
    F16 -> (2, 2)
    _ -> (4, 4)

vectorLayout :: Scalar -> Int -> (Word32, Word32)
vectorLayout scalar n =
  let elemAlign = case scalar of
        F16 -> 2
        _ -> 4
      elemSize = elemAlign
  in case n of
      2 -> (elemAlign * 2, elemSize * 2)
      3 -> (elemAlign * 4, elemSize * 4)
      4 -> (elemAlign * 4, elemSize * 4)
      _ -> (elemAlign * 4, elemSize * 4)

matrixLayout :: Int -> Int -> Scalar -> TypeLayout
matrixLayout cols rows scalar =
  let (a, sz) = vectorLayout scalar rows
      stride = roundUp sz a
      total = stride * fromIntegral cols
  in TLMatrix cols rows scalar a total stride

data OverrideInfo = OverrideInfo
  { oiName :: !String
  , oiId :: !(Maybe Word32)
  , oiSpecId :: !(Maybe Word32)
  , oiType :: !TypeLayout
  } deriving (Eq, Show, Read)

data ShaderInterface = ShaderInterface
  { siBindings :: ![BindingInfo]
  , siOverrides :: ![OverrideInfo]
  } deriving (Eq, Show, Read)

specializableOverrides :: ShaderInterface -> [OverrideInfo]
specializableOverrides iface =
  filter (isJust . oiSpecId) (siOverrides iface)

-- | Compiled shader with a type-level interface description.
data CompiledShader (iface :: [Binding]) = CompiledShader
  { shaderSpirv :: ByteString
  , shaderInterface :: ShaderInterface
  }

-- | Existential wrapper for runtime compilation.
data SomeCompiledShader = forall iface. SomeCompiledShader (CompiledShader iface)

-- Errors and options

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
defaultCompileOptions = CompileOptions 0x00010600 [] [] SpecStrict

overrideValuesText :: [(String, OverrideValue)] -> [(Text, OverrideValue)]
overrideValuesText = map (\(k, v) -> (T.pack k, v))

-- Public API

compileWeslToSpirv :: String -> Either CompileError SomeCompiledShader
compileWeslToSpirv = compileWeslToSpirvWith defaultCompileOptions

compileWeslToSpirvWith :: CompileOptions -> String -> Either CompileError SomeCompiledShader
compileWeslToSpirvWith opts src = do
  moduleAst0 <- parseModuleWith (enabledFeatures opts) src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst <- lowerOverridesWith [] (overrideValuesText (overrideValues opts)) moduleAst1
  when (not (null (modImports moduleAst))) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface (overrideSpecMode opts) moduleAst
  spirv <- emitSpirv opts moduleAst iface
  let shader = CompiledShader spirv iface
  pure (SomeCompiledShader shader)

compileWeslToSpirvWithDiagnostics :: CompileOptions -> String -> Either CompileError (SomeCompiledShader, [Diagnostic])
compileWeslToSpirvWithDiagnostics opts src = do
  moduleAst0 <- parseModuleWith (enabledFeatures opts) src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst <- lowerOverridesWith [] (overrideValuesText (overrideValues opts)) moduleAst1
  when (not (null (modImports moduleAst))) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface (overrideSpecMode opts) moduleAst
  spirv <- emitSpirv opts moduleAst iface
  diags <- collectDiagnosticsMerged opts [] moduleAst
  let shader = CompiledShader spirv iface
  pure (SomeCompiledShader shader, diags)

compileWeslToSpirvFile :: FilePath -> IO (Either CompileError SomeCompiledShader)
compileWeslToSpirvFile = compileWeslToSpirvFileWith defaultCompileOptions

compileWeslToSpirvFileWith :: CompileOptions -> FilePath -> IO (Either CompileError SomeCompiledShader)
compileWeslToSpirvFileWith opts path = do
  inputPath <- resolveInputPath path
  case inputPath of
    Left err -> pure (Left err)
    Right filePath -> do
      src <- readFile filePath
      case parseModuleWith (enabledFeatures opts) src of
        Left err -> pure (Left err)
        Right moduleAst0 -> do
          resolved <- resolveImports opts (dropExtension filePath) moduleAst0
          case resolved of
            Left err -> pure (Left err)
            Right linked -> do
              case resolveTypeAliases linked of
                Left err -> pure (Left err)
                Right linked' -> do
                  let rootDir = takeDirectory filePath
                      rootPath = modulePathFromFile rootDir filePath
                  case lowerOverridesWith rootPath (overrideValuesText (overrideValues opts)) linked' of
                    Left err -> pure (Left err)
                    Right lowered -> do
                      case validateConstAssertsMerged opts rootPath lowered of
                        Left err -> pure (Left err)
                        Right () -> do
                          let iface = buildInterface (overrideSpecMode opts) lowered
                          case iface of
                            Left err -> pure (Left err)
                            Right iface' -> do
                              case emitSpirv opts lowered iface' of
                                Left err -> pure (Left err)
                                Right spirv -> pure (Right (SomeCompiledShader (CompiledShader spirv iface')))

compileWeslToSpirvFileWithDiagnostics :: CompileOptions -> FilePath -> IO (Either CompileError (SomeCompiledShader, [Diagnostic]))
compileWeslToSpirvFileWithDiagnostics opts path = do
  inputPath <- resolveInputPath path
  case inputPath of
    Left err -> pure (Left err)
    Right filePath -> do
      src <- readFile filePath
      case parseModuleWith (enabledFeatures opts) src of
        Left err -> pure (Left err)
        Right moduleAst0 -> do
          resolved <- resolveImports opts (dropExtension filePath) moduleAst0
          case resolved of
            Left err -> pure (Left err)
            Right linked -> do
              case resolveTypeAliases linked of
                Left err -> pure (Left err)
                Right linked' -> do
                  let rootDir = takeDirectory filePath
                      rootPath = modulePathFromFile rootDir filePath
                  case lowerOverridesWith rootPath (overrideValuesText (overrideValues opts)) linked' of
                    Left err -> pure (Left err)
                    Right lowered -> do
                      case collectDiagnosticsMerged opts rootPath lowered of
                        Left err -> pure (Left err)
                        Right diags -> do
                          let iface = buildInterface (overrideSpecMode opts) lowered
                          case iface of
                            Left err -> pure (Left err)
                            Right iface' -> do
                              case emitSpirv opts lowered iface' of
                                Left err -> pure (Left err)
                                Right spirv ->
                                  pure (Right (SomeCompiledShader (CompiledShader spirv iface'), diags))

data PackageInfo = PackageInfo
  { pkgName :: String
  , pkgVersion :: Maybe String
  , pkgRoot :: FilePath
  , pkgSourceRoot :: FilePath
  , pkgDependencies :: [PackageDependency]
  } deriving (Eq, Show)

data PackageDependency = PackageDependency
  { depName :: String
  , depVersion :: Maybe String
  , depPath :: Maybe FilePath
  } deriving (Eq, Show)

data TomlSection
  = TomlSectionNone
  | TomlSectionPackage
  | TomlSectionDependencies
  | TomlSectionDependency String
  deriving (Eq, Show)

discoverPackageInfo :: FilePath -> IO (Maybe PackageInfo)
discoverPackageInfo filePath = do
  let start = takeDirectory filePath
  findWeslToml start
  where
    findWeslToml dir = do
      let candidate = dir </> "wesl.toml"
      exists <- doesFileExist candidate
      if exists
        then parseWeslToml candidate
        else
          let parent = takeDirectory dir
          in if parent == dir
              then pure Nothing
              else findWeslToml parent

parseWeslToml :: FilePath -> IO (Maybe PackageInfo)
parseWeslToml path = do
  contents <- readFile path
  let root = takeDirectory path
      (_, mName, mVersion, mSource, deps) = foldl' (parseLine root) (TomlSectionNone, Nothing, Nothing, Nothing, Map.empty) (lines contents)
      name = fromMaybe "wesl-package" mName
      sourceRoot = fromMaybe "." mSource
  pure (Just (PackageInfo name mVersion root (root </> sourceRoot) (Map.elems deps)))
  where
    parseLine root (section, mName, mVersion, mSource, deps) line =
      let trimmed = trim (takeWhile (/= '#') line)
      in case trimmed of
          [] -> (section, mName, mVersion, mSource, deps)
          ('[':rest) ->
            case dropWhileEnd (== ']') rest of
              "package" -> (TomlSectionPackage, mName, mVersion, mSource, deps)
              "dependencies" -> (TomlSectionDependencies, mName, mVersion, mSource, deps)
              name
                | "dependencies." `isPrefixOf` name ->
                    let depName = drop (length ("dependencies." :: String)) name
                    in (TomlSectionDependency depName, mName, mVersion, mSource, deps)
              _ -> (TomlSectionNone, mName, mVersion, mSource, deps)
          _ ->
            case break (== '=') trimmed of
              (key, '=':val) ->
                let key' = trim key
                    val' = trim val
                in case section of
                    TomlSectionPackage ->
                      case key' of
                        "name" -> (section, Just (stripQuotes val'), mVersion, mSource, deps)
                        "version" -> (section, mName, Just (stripQuotes val'), mSource, deps)
                        "source_root" -> (section, mName, mVersion, Just (stripQuotes val'), deps)
                        _ -> (section, mName, mVersion, mSource, deps)
                    TomlSectionDependencies ->
                      let (dep, deps') = parseDependencyLine root key' val' deps
                      in (section, mName, mVersion, mSource, Map.insert (depName dep) dep deps')
                    TomlSectionDependency depName ->
                      let deps' = updateDependency root depName key' val' deps
                      in (section, mName, mVersion, mSource, deps')
                    _ -> (section, mName, mVersion, mSource, deps)
              _ -> (section, mName, mVersion, mSource, deps)

    parseDependencyLine root name val deps =
      if "{" `isPrefixOf` val
        then
          let fields = parseInlineTable val
              dep = applyDepFields root (emptyDep name) fields
          in (dep, deps)
        else
          let dep = (emptyDep name) { depVersion = Just (stripQuotes val) }
          in (dep, deps)

    updateDependency root name key val deps =
      let dep0 = Map.findWithDefault (emptyDep name) name deps
          dep1 =
            case key of
              "version" -> dep0 { depVersion = Just (stripQuotes val) }
              "path" -> dep0 { depPath = Just (resolvePath root (stripQuotes val)) }
              _ -> dep0
      in Map.insert name dep1 deps

    emptyDep name = PackageDependency name Nothing Nothing

    applyDepFields root dep fields =
      foldl'
        (\d (k, v) ->
           case k of
             "version" -> d { depVersion = Just v }
             "path" -> d { depPath = Just (resolvePath root v) }
             _ -> d)
        dep
        fields

    parseInlineTable raw =
      let inner = trim (dropWhile (== '{') (dropWhileEnd (== '}') raw))
      in mapMaybe parsePair (splitComma inner)

    parsePair item =
      case break (== '=') item of
        (k, '=':v) ->
          let key = trim k
              val = stripQuotes (trim v)
          in if null key then Nothing else Just (key, val)
        _ -> Nothing

    splitComma str = go str [] []
      where
        go [] acc cur = reverse (reverse cur : acc)
        go (c:cs) acc cur
          | c == ',' = go cs (reverse cur : acc) []
          | otherwise = go cs acc (c : cur)

    resolvePath root val =
      if isRelative val
        then normalise (root </> val)
        else val

    trim = dropWhileEnd isSpace . dropWhile isSpace
    dropWhileEnd f = reverse . dropWhile f . reverse
    stripQuotes s =
      case s of
        ('"':rest) -> reverse (drop 1 (reverse rest))
        _ -> s

resolveInputPath :: FilePath -> IO (Either CompileError FilePath)
resolveInputPath path = do
  exists <- doesFileExist path
  if exists
    then pure (Right path)
    else do
      let weslPath = path <.> "wesl"
      let wgslPath = path <.> "wgsl"
      weslExists <- doesFileExist weslPath
      if weslExists
        then pure (Right weslPath)
        else do
          wgslExists <- doesFileExist wgslPath
          if wgslExists
            then pure (Right wgslPath)
            else pure (Left (CompileError ("file not found: " <> path) Nothing Nothing))

compileWeslToSpirvBytes :: String -> Either CompileError ByteString
compileWeslToSpirvBytes src = compileWeslToSpirvBytesWith defaultCompileOptions src

compileWeslToSpirvBytesWith :: CompileOptions -> String -> Either CompileError ByteString
compileWeslToSpirvBytesWith opts src = do
  moduleAst0 <- parseModuleWith (enabledFeatures opts) src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst <- lowerOverridesWith [] (overrideValuesText (overrideValues opts)) moduleAst1
  when (not (null (modImports moduleAst))) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface (overrideSpecMode opts) moduleAst
  emitSpirv opts moduleAst iface

compileWeslToSpirvBytesWithDiagnostics :: CompileOptions -> String -> Either CompileError (ByteString, [Diagnostic])
compileWeslToSpirvBytesWithDiagnostics opts src = do
  moduleAst0 <- parseModuleWith (enabledFeatures opts) src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst <- lowerOverridesWith [] (overrideValuesText (overrideValues opts)) moduleAst1
  when (not (null (modImports moduleAst))) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface (overrideSpecMode opts) moduleAst
  bytes <- emitSpirv opts moduleAst iface
  diags <- collectDiagnosticsMerged opts [] moduleAst
  pure (bytes, diags)

weslCacheVersion :: String
weslCacheVersion = "wesl-cache-v1"

weslCacheDir :: FilePath
weslCacheDir = "dist-newstyle" </> ".wesl-cache"

weslCacheKey :: CompileOptions -> String -> String
weslCacheKey opts src =
  let keySrc =
        intercalate
          "\n"
          [ weslCacheVersion
          , "v=" <> show (spirvVersion opts)
          , "features=" <> show (enabledFeatures opts)
          , "overrides=" <> show (overrideValues opts)
          , "spec=" <> show (overrideSpecMode opts)
          , src
          ]
      bytes = BS.pack (map (fromIntegral . ord) keySrc)
      hash = fnv1a64 bytes
      hex = showHex hash ""
  in replicate (16 - length hex) '0' <> hex

fnv1a64 :: ByteString -> Word64
fnv1a64 = BS.foldl' step offset
  where
    offset = 14695981039346656037
    prime = 1099511628211
    step acc byte = (acc `xor` fromIntegral byte) * prime

weslCachePaths :: CompileOptions -> String -> (FilePath, FilePath)
weslCachePaths opts src =
  let key = weslCacheKey opts src
      base = weslCacheDir </> key
  in (base <.> "spv", base <.> "iface")

loadWeslCache :: CompileOptions -> String -> IO (Maybe (ByteString, ShaderInterface))
loadWeslCache opts src = do
  let (spvPath, ifacePath) = weslCachePaths opts src
  okSpv <- doesFileExist spvPath
  okIface <- doesFileExist ifacePath
  if not (okSpv && okIface)
    then pure Nothing
    else
      (do
        bytes <- BS.readFile spvPath
        ifaceText <- readFile ifacePath
        case readMaybe ifaceText of
          Just iface -> pure (Just (bytes, iface))
          Nothing -> pure Nothing
      ) `catch` \(_ :: SomeException) -> pure Nothing

writeWeslCache :: CompileOptions -> String -> ByteString -> ShaderInterface -> IO ()
writeWeslCache opts src bytes iface =
  let (spvPath, ifacePath) = weslCachePaths opts src
  in (do
        createDirectoryIfMissing True (takeDirectory spvPath)
        BS.writeFile spvPath bytes
        writeFile ifacePath (show iface)
     ) `catch` \(_ :: SomeException) -> pure ()

-- Quasiquoter

wesl :: QuasiQuoter
wesl =
  QuasiQuoter
    { quoteExp = weslExp
    , quotePat = const (fail "wesl: pattern context not supported")
    , quoteType = const (fail "wesl: type context not supported")
    , quoteDec = const (fail "wesl: declaration context not supported")
    }

weslExp :: String -> Q Exp
weslExp src = do
  let opts = defaultCompileOptions
  cached <- TH.runIO (loadWeslCache opts src)
  case cached of
    Just (bytes, iface) -> emitWeslExp bytes iface
    Nothing ->
      case compileWeslToSpirvWith opts src of
        Left err -> fail (renderError err)
        Right (SomeCompiledShader (CompiledShader bytes iface)) -> do
          TH.runIO (writeWeslCache opts src bytes iface)
          emitWeslExp bytes iface
  where
    emitWeslExp bytes iface = do
      bytesExp <- bytesToExp bytes
      ifaceExp <- interfaceToExp iface
      let ifaceTy = interfaceToType iface
      let shaderExp = TH.AppE (TH.AppE (TH.ConE 'CompiledShader) bytesExp) ifaceExp
      pure (TH.SigE shaderExp (TH.AppT (TH.ConT ''CompiledShader) ifaceTy))


-- Parsing

data SrcPos = SrcPos
  { spLine :: !Int
  , spCol :: !Int
  } deriving (Eq, Show)

data Token = Token
  { tkKind :: !TokKind
  , tkPos :: !SrcPos
  } deriving (Eq, Show)

data TokKind
  = TkIdent !Text
  | TkInt !Integer
  | TkFloat !Float
  | TkString !Text
  | TkSymbol !Text
  deriving (Eq, Show)

data ImportRelative
  = ImportPackage
  | ImportSuper Int
  deriving (Eq, Show)

data ImportItem = ImportItem
  { iiPath :: ![Text]
  , iiAlias :: !(Maybe Text)
  } deriving (Eq, Show)

data ImportDecl = ImportDecl
  { idRelative :: !(Maybe ImportRelative)
  , idItems :: ![ImportItem]
  } deriving (Eq, Show)

data DiagnosticSeverity = DiagError | DiagWarning | DiagInfo | DiagOff
  deriving (Eq, Show)

data Directive
  = DirEnable !Text
  | DirDiagnostic !DiagnosticSeverity !Text
  deriving (Eq, Show)

data AliasDecl = AliasDecl
  { adName :: !Text
  , adType :: !Type
  } deriving (Eq, Show)

data OverrideDecl = OverrideDecl
  { odName :: !Text
  , odId :: !(Maybe Word32)
  , odType :: !Type
  , odExpr :: !(Maybe Expr)
  } deriving (Eq, Show)

data ConstAssert = ConstAssert
  { caPos :: !SrcPos
  , caExpr :: !Expr
  } deriving (Eq, Show)

data ModuleAst = ModuleAst
  { modDirectives :: ![Directive]
  , modImports :: ![ImportDecl]
  , modAliases :: ![AliasDecl]
  , modStructs :: ![StructDecl]
  , modBindings :: ![BindingDecl]
  , modGlobals :: ![GlobalVarDecl]
  , modConsts :: ![ConstDecl]
  , modOverrides :: ![OverrideDecl]
  , modConstAsserts :: ![ConstAssert]
  , modFunctions :: ![FunctionDecl]
  , modEntry :: !(Maybe EntryPoint)
  } deriving (Eq, Show)

data BindingDecl = BindingDecl
  { bdName :: !Text
  , bdKind :: !BindingKind
  , bdGroup :: !Word32
  , bdBinding :: !Word32
  , bdType :: !Type
  } deriving (Eq, Show)

data GlobalVarDecl = GlobalVarDecl
  { gvName :: !Text
  , gvSpace :: !Text
  , gvType :: !Type
  , gvInit :: !(Maybe Expr)
  } deriving (Eq, Show)

data ConstDecl = ConstDecl
  { cdName :: !Text
  , cdExpr :: !Expr
  } deriving (Eq, Show)

data Stage = StageCompute | StageFragment | StageVertex
  deriving (Eq, Show)

data Param = Param
  { paramName :: !Text
  , paramType :: !Type
  , paramAttrs :: ![Attr]
  } deriving (Eq, Show)

data EntryPoint = EntryPoint
  { epName :: !Text
  , epStage :: !Stage
  , epWorkgroupSize :: !(Maybe (Word32, Word32, Word32))
  , epParams :: ![Param]
  , epReturnType :: !(Maybe Type)
  , epReturnLocation :: !(Maybe Word32)
  , epReturnBuiltin :: !(Maybe Text)
  , epBody :: ![Stmt]
  } deriving (Eq, Show)

data FunctionDecl = FunctionDecl
  { fnName :: !Text
  , fnParams :: ![Param]
  , fnReturnType :: !(Maybe Type)
  , fnBody :: ![Stmt]
  } deriving (Eq, Show)

data Stmt
  = SLet !Text !Expr
  | SVar !Text !Expr
  | SAssign !LValue !Expr
  | SAssignOp !LValue !BinOp !Expr
  | SInc !LValue
  | SDec !LValue
  | SExpr !Expr
  | SIf !Expr ![Stmt] !(Maybe [Stmt])
  | SWhile !Expr ![Stmt]
  | SLoop ![Stmt] !(Maybe [Stmt])
  | SFor !(Maybe Stmt) !(Maybe Expr) !(Maybe Stmt) ![Stmt]
  | SSwitch !Expr ![SwitchCase] !(Maybe [Stmt])
  | SBreak
  | SBreakIf !Expr
  | SContinue
  | SDiscard
  | SFallthrough
  | SReturn !(Maybe Expr)
  deriving (Eq, Show)

data SwitchCase = SwitchCase
  { scSelectors :: ![Expr]
  , scBody :: ![Stmt]
  } deriving (Eq, Show)

data LValue
  = LVVar !Text
  | LVField !LValue !Text
  | LVIndex !LValue !Expr
  | LVDeref !Expr
  deriving (Eq, Show)

data Expr
  = EVar !Text
  | EInt !Integer
  | EFloat !Float
  | EBool !Bool
  | EBinary !BinOp !Expr !Expr
  | EUnary !UnaryOp !Expr
  | ECall !Text ![Expr]
  | EBitcast !Type !Expr
  | EField !Expr !Text
  | EIndex !Expr !Expr
  deriving (Eq, Show)

data BinOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpEq
  | OpNe
  | OpLt
  | OpLe
  | OpGt
  | OpGe
  | OpAnd
  | OpOr
  | OpBitAnd
  | OpBitOr
  | OpBitXor
  | OpShl
  | OpShr
  deriving (Eq, Show)

data UnaryOp = OpNeg | OpNot | OpAddr | OpDeref
  deriving (Eq, Show)

parseModuleWith :: [String] -> String -> Either CompileError ModuleAst
parseModuleWith features src = do
  toks <- lexWesl (T.pack src)
  parseModuleTokensWith (Set.fromList (map T.pack features)) toks

type FeatureSet = Set.Set Text

lexWesl :: Text -> Either CompileError [Token]
lexWesl = go (SrcPos 1 1)
  where
    go _ src
      | T.null src = pure []
    go pos src =
      case T.uncons src of
        Nothing -> pure []
        Just (c, cs)
          | c == ' ' || c == '\t' -> go (advance pos c) cs
          | c == '\n' -> go (advance pos c) cs
          | c == '\r' -> go (advance pos c) cs
          | c == '/' && prefix "//" src ->
              let (_, rest, pos') = consumeLine (advance (advance pos '/') '/') (T.drop 2 src)
              in go pos' rest
          | c == '/' && prefix "/*" src ->
              case consumeBlockComment (advance (advance pos '/') '*') (T.drop 2 src) of
                Left err -> Left err
                Right (rest, pos') -> go pos' rest
          | isAlpha c || c == '_' ->
              let (ident, rest, pos') = consumeIdent pos src
              in (Token (TkIdent ident) pos :) <$> go pos' rest
          | isDigit c ->
              let (tok, rest, pos') = consumeNumber pos src
              in (Token tok pos :) <$> go pos' rest
          | c == '"' ->
              case consumeString pos cs of
                Left err -> Left err
                Right (str, rest, pos') -> (Token (TkString str) pos :) <$> go pos' rest
          | prefix "::" src -> token2 "::" pos c src
          | prefix "==" src -> token2 "==" pos c src
          | prefix "!=" src -> token2 "!=" pos c src
          | prefix "<=" src -> token2 "<=" pos c src
          | prefix ">=" src -> token2 ">=" pos c src
          | prefix "&&" src -> token2 "&&" pos c src
          | prefix "||" src -> token2 "||" pos c src
          | prefix "<<=" src -> token3 "<<=" pos c src
          | prefix ">>=" src -> token3 ">>=" pos c src
          | prefix "<<" src -> token2 "<<" pos c src
          | prefix ">>" src -> token2 ">>" pos c src
          | prefix "++" src -> token2 "++" pos c src
          | prefix "--" src -> token2 "--" pos c src
          | prefix "+=" src -> token2 "+=" pos c src
          | prefix "-=" src -> token2 "-=" pos c src
          | prefix "*=" src -> token2 "*=" pos c src
          | prefix "/=" src -> token2 "/=" pos c src
          | prefix "%=" src -> token2 "%=" pos c src
          | prefix "&=" src -> token2 "&=" pos c src
          | prefix "|=" src -> token2 "|=" pos c src
          | prefix "^=" src -> token2 "^=" pos c src
          | c `elem` symbolChars ->
              let sym = T.singleton c
              in (Token (TkSymbol sym) pos :) <$> go (advance pos c) cs
          | otherwise ->
              Left (CompileError ("unexpected character: " <> [c]) (Just (spLine pos)) (Just (spCol pos)))

    symbolChars :: String
    symbolChars = "@:{}();,<>=[]+-*/.%!&|^"

    prefix s xs = s `T.isPrefixOf` xs
    advance2 p a b = advance (advance p a) b
    advance3 p a b c = advance (advance2 p a b) c

    token2 sym p ch s =
      let c2 = T.index s 1
          rest = T.drop 2 s
      in (Token (TkSymbol sym) p :) <$> go (advance2 p ch c2) rest

    token3 sym p ch s =
      let c2 = T.index s 1
          c3 = T.index s 2
          rest = T.drop 3 s
      in (Token (TkSymbol sym) p :) <$> go (advance3 p ch c2 c3) rest

    consumeLine p rest =
      let (line, rest') = T.break (== '\n') rest
          pos' = T.foldl' advance p line
      in if T.null rest'
           then (line, T.empty, pos')
           else (line, T.drop 1 rest', advance pos' '\n')

    consumeBlockComment p cs =
      let goBlock pos' src' =
            case T.uncons src' of
              Nothing -> Left (CompileError "unterminated block comment" (Just (spLine pos')) (Just (spCol pos')))
              Just (x, rest0) ->
                case T.uncons rest0 of
                  Just (y, rest)
                    | x == '*' && y == '/' -> Right (rest, advance (advance pos' x) y)
                    | otherwise -> goBlock (advance pos' x) rest0
                  Nothing -> goBlock (advance pos' x) T.empty
      in goBlock p cs

    consumeIdent p cs =
      let (ident, rest) = T.span (\x -> isAlphaNum x || x == '_') cs
          pos' = T.foldl' advance p ident
      in (ident, rest, pos')

    consumeNumber p cs =
      let (digits, rest) = T.span isDigit cs
      in case T.uncons rest of
          Just ('.', r) ->
            case T.uncons r of
              Just (d, _rest1) | isDigit d ->
                let (frac, rest') = T.span isDigit r
                    numTxt = digits <> "." <> frac
                    pos' = T.foldl' advance p numTxt
                in (TkFloat (read (T.unpack numTxt)), rest', pos')
              _ ->
                let pos' = T.foldl' advance p digits
                in (TkInt (read (T.unpack digits)), rest, pos')
          _ ->
            let pos' = T.foldl' advance p digits
            in (TkInt (read (T.unpack digits)), rest, pos')

    consumeString p cs =
      let goStr acc pos' src' =
            case T.uncons src' of
              Nothing -> Left (CompileError "unterminated string" (Just (spLine pos')) (Just (spCol pos')))
              Just (x, xs)
                | x == '"' -> Right (T.pack (reverse acc), xs, advance pos' x)
                | x == '\\' ->
                    case T.uncons xs of
                      Nothing -> Left (CompileError "unterminated escape" (Just (spLine pos')) (Just (spCol pos')))
                      Just (e, rest) -> goStr (e : acc) (advance (advance pos' x) e) rest
                | otherwise -> goStr (x : acc) (advance pos' x) xs
      in goStr [] (advance p '"') cs

    advance (SrcPos l c) ch
      | ch == '\n' = SrcPos (l + 1) 1
      | otherwise = SrcPos l (c + 1)

parseModuleTokensWith :: FeatureSet -> [Token] -> Either CompileError ModuleAst
parseModuleTokensWith feats toks =
  let loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntry seenNonDirective seenNonImport rest =
        case rest of
          [] ->
            Right (ModuleAst (reverse accDirectives) (reverse accImports) (reverse accAliases) (reverse accStructs) (reverse accBindings) (reverse accGlobals) (reverse accConsts) (reverse accOverrides) (reverse accAsserts) (reverse accFns) accEntry)
          _ -> do
            (attrs, rest1) <- parseAttributes rest
            (keep, attrs') <- applyIf attrs feats rest1
            case rest1 of
              (Token (TkIdent "enable") _ : _) -> do
                when (seenNonDirective && keep) $
                  Left (errorAt rest1 "directives must appear before any declarations")
                when (not (null attrs')) $
                  Left (errorAt rest1 "enable does not accept attributes other than @if")
                (dir, rest2) <- parseEnableDirective rest1
                let accDirectives' = if keep then dir : accDirectives else accDirectives
                loop accDirectives' accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntry seenNonDirective seenNonImport rest2
              (Token (TkIdent "diagnostic") _ : _) -> do
                when (seenNonDirective && keep) $
                  Left (errorAt rest1 "directives must appear before any declarations")
                when (not (null attrs')) $
                  Left (errorAt rest1 "diagnostic does not accept attributes other than @if")
                (dir, rest2) <- parseDiagnosticDirective rest1
                let accDirectives' = if keep then dir : accDirectives else accDirectives
                loop accDirectives' accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntry seenNonDirective seenNonImport rest2
              (Token (TkIdent "import") _ : _) -> do
                when (seenNonImport && keep) $
                  Left (errorAt rest1 "imports must appear before other declarations")
                when (not (null attrs')) $
                  Left (errorAt rest1 "import does not accept attributes other than @if")
                (importDecl, rest2) <- parseImportStatement rest1
                let accImports' = if keep then importDecl : accImports else accImports
                loop accDirectives accImports' accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntry True seenNonImport rest2
              (Token (TkIdent "alias") _ : _) -> do
                when (not (null attrs')) $
                  Left (errorAt rest1 "alias does not accept attributes other than @if")
                (aliasDecl, rest2) <- parseAliasDecl rest1
                let accAliases' = if keep then aliasDecl : accAliases else accAliases
                loop accDirectives accImports accAliases' accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntry True True rest2
              (Token (TkIdent "const_assert") _ : _) -> do
                when (not (null attrs')) $
                  Left (errorAt rest1 "const_assert does not accept attributes other than @if")
                (assertDecl, rest2) <- parseConstAssert rest1
                let accAsserts' = if keep then assertDecl : accAsserts else accAsserts
                loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts' accFns accEntry True True rest2
              (Token (TkIdent "struct") _ : _) -> do
                (mStruct, rest2) <- parseStruct feats rest1
                let accStructs' = case (keep, mStruct) of
                      (True, Just structDecl) -> structDecl : accStructs
                      _ -> accStructs
                loop accDirectives accImports accAliases accStructs' accBindings accGlobals accConsts accOverrides accAsserts accFns accEntry True True rest2
              (Token (TkIdent "var") _ : _) -> do
                let hasBindingAttrs = isJust (attrInt "group" attrs') || isJust (attrInt "binding" attrs')
                (addrSpace, _access, _rest) <- parseAddressSpaceMaybe (drop 1 rest1)
                let isBindingSpace = maybe False (`elem` ["uniform", "storage", "sampler", "texture"]) addrSpace
                if hasBindingAttrs || isBindingSpace
                  then do
                    (bindingDecl, rest2) <- parseBinding attrs' rest1
                    let accBindings' = if keep then bindingDecl : accBindings else accBindings
                    loop accDirectives accImports accAliases accStructs accBindings' accGlobals accConsts accOverrides accAsserts accFns accEntry True True rest2
                  else do
                    (globalDecl, rest2) <- parseGlobalVar attrs' rest1
                    let accGlobals' = if keep then globalDecl : accGlobals else accGlobals
                    loop accDirectives accImports accAliases accStructs accBindings accGlobals' accConsts accOverrides accAsserts accFns accEntry True True rest2
              (Token (TkIdent "let") _ : _) -> do
                (constDecl, rest2) <- parseConstDecl attrs' rest1
                let accConsts' = if keep then constDecl : accConsts else accConsts
                loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts' accOverrides accAsserts accFns accEntry True True rest2
              (Token (TkIdent "override") _ : _) -> do
                let idAttr =
                      case attrInt "id" attrs' of
                        Nothing -> Nothing
                        Just v ->
                          if v < 0 || v > fromIntegral (maxBound :: Word32)
                            then Nothing
                            else Just (fromIntegral v)
                when (maybe False (\v -> v < 0 || v > fromIntegral (maxBound :: Word32)) (attrInt "id" attrs')) $
                  Left (errorAt rest1 "@id must be a non-negative 32-bit integer")
                let otherAttrs = [a | a <- attrs', not (isIdAttr a)]
                when (not (null otherAttrs)) $
                  Left (errorAt rest1 "override only accepts @id and @if attributes")
                (overrideDecl, rest2) <- parseOverrideDecl idAttr rest1
                let accOverrides' = if keep then overrideDecl : accOverrides else accOverrides
                loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides' accAsserts accFns accEntry True True rest2
              (Token (TkIdent "fn") _ : _) -> do
                (name, params, retType, retLoc, retBuiltin, body, rest2) <- parseFunctionGeneric feats rest1
                let hasStageAttr = any isStageAttr attrs'
                case entryAttributesMaybe attrs' of
                  Nothing ->
                    if hasStageAttr
                      then Left (errorAt rest1 "invalid entry point attributes")
                      else do
                        case retLoc of
                          Nothing -> pure ()
                          Just _ -> Left (errorAt rest1 "return location is only allowed on entry points")
                        case retBuiltin of
                          Nothing -> pure ()
                          Just _ -> Left (errorAt rest1 "return builtin is only allowed on entry points")
                        let fnDecl = FunctionDecl name params retType body
                        let accFns' = if keep then fnDecl : accFns else accFns
                        loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns' accEntry True True rest2
                  Just (stage, workgroup) -> do
                    let entry = EntryPoint name stage workgroup params retType retLoc retBuiltin body
                    case accEntry of
                      Nothing ->
                        if keep
                          then loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns (Just entry) True True rest2
                          else loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntry True True rest2
                      Just _ -> Left (errorAt rest1 "multiple entry points are not supported")
              _ -> Left (errorAt rest1 "expected directive, import, alias, struct, var, let, override, const_assert, or fn")
  in loop [] [] [] [] [] [] [] [] [] [] Nothing False False toks
  where
    isStageAttr attr =
      case attr of
        Attr n _ -> n == "compute" || n == "fragment" || n == "vertex"
        AttrIf _ -> False
    isIdAttr attr =
      case attr of
        Attr n [AttrInt _] -> n == "id"
        _ -> False

parseAttributes :: [Token] -> Either CompileError ([Attr], [Token])
parseAttributes toks =
  let go acc rest =
        case rest of
          (Token (TkSymbol "@") _ : Token (TkIdent name) _ : more) ->
            if name == "if"
              then do
                (expr, rest') <- parseIfAttr more
                go (AttrIf expr : acc) rest'
              else do
                (args, rest') <- parseAttrArgs more
                go (Attr name args : acc) rest'
          _ -> Right (reverse acc, rest)
  in go [] toks

parseIfAttr :: [Token] -> Either CompileError (TTExpr, [Token])
parseIfAttr toks =
  case toks of
    (Token (TkSymbol "(") _ : rest) -> do
      (expr, rest1) <- parseTTExpr rest
      rest2 <- expectSymbol ")" rest1
      Right (expr, rest2)
    _ -> Left (errorAt toks "expected @if(<expr>)")

parseAttrArgs :: [Token] -> Either CompileError ([AttrArg], [Token])
parseAttrArgs toks =
  case toks of
    (Token (TkSymbol "(") _ : rest) -> parseAttrArgList [] rest
    _ -> Right ([], toks)
  where
    parseAttrArgList acc rest =
      case rest of
        (Token (TkSymbol ")") _ : more) -> Right (reverse acc, more)
        _ -> do
          (arg, rest1) <- parseAttrArg rest
          case rest1 of
            (Token (TkSymbol ",") _ : more) -> parseAttrArgList (arg:acc) more
            (Token (TkSymbol ")") _ : more) -> Right (reverse (arg:acc), more)
            _ -> Left (errorAt rest1 "expected ',' or ')' in attribute arguments")

    parseAttrArg rest =
      case rest of
        (Token (TkInt n) _ : more) -> Right (AttrInt n, more)
        (Token (TkIdent name) _ : more) -> Right (AttrIdent name, more)
        _ -> Left (errorAt rest "expected attribute argument")

parseTTExpr :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTExpr = parseTTOr

parseTTOr :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTOr toks = do
  (lhs, rest) <- parseTTAnd toks
  parseTTOrTail lhs rest
  where
    parseTTOrTail lhs rest =
      case rest of
        (Token (TkSymbol "||") _ : more) -> do
          (rhs, rest1) <- parseTTAnd more
          parseTTOrTail (TTOr lhs rhs) rest1
        _ -> Right (lhs, rest)

parseTTAnd :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTAnd toks = do
  (lhs, rest) <- parseTTNot toks
  parseTTAndTail lhs rest
  where
    parseTTAndTail lhs rest =
      case rest of
        (Token (TkSymbol "&&") _ : more) -> do
          (rhs, rest1) <- parseTTNot more
          parseTTAndTail (TTAnd lhs rhs) rest1
        _ -> Right (lhs, rest)

parseTTNot :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTNot toks =
  case toks of
    (Token (TkSymbol "!") _ : more) -> do
      (expr, rest) <- parseTTNot more
      Right (TTNot expr, rest)
    _ -> parseTTPrimary toks

parseTTPrimary :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTPrimary toks =
  case toks of
    (Token (TkIdent "true") _ : rest) -> Right (TTBool True, rest)
    (Token (TkIdent "false") _ : rest) -> Right (TTBool False, rest)
    (Token (TkIdent name) _ : rest) -> Right (TTVar name, rest)
    (Token (TkSymbol "(") _ : rest) -> do
      (expr, rest1) <- parseTTExpr rest
      rest2 <- expectSymbol ")" rest1
      Right (expr, rest2)
    _ -> Left (errorAt toks "expected translate-time expression")

applyIf :: [Attr] -> FeatureSet -> [Token] -> Either CompileError (Bool, [Attr])
applyIf attrs feats _toks = do
  (ifExprs, attrs') <- stripIfAttr attrs
  case ifExprs of
    [] -> Right (True, attrs')
    _ ->
      let combined = foldl1 TTAnd ifExprs
      in Right (evalTTExpr feats combined, attrs')

stripIfAttr :: [Attr] -> Either CompileError ([TTExpr], [Attr])
stripIfAttr attrs =
  case partition isIf attrs of
    ([], others) -> Right ([], others)
    (ifs, others) -> Right ([expr | AttrIf expr <- ifs], others)
  where
    isIf attr =
      case attr of
        AttrIf _ -> True
        _ -> False

evalTTExpr :: FeatureSet -> TTExpr -> Bool
evalTTExpr feats expr =
  case expr of
    TTVar name -> Set.member name feats
    TTBool b -> b
    TTNot a -> not (evalTTExpr feats a)
    TTAnd a b -> evalTTExpr feats a && evalTTExpr feats b
    TTOr a b -> evalTTExpr feats a || evalTTExpr feats b

parseStruct :: FeatureSet -> [Token] -> Either CompileError (Maybe StructDecl, [Token])
parseStruct feats toks =
  case toks of
    (Token (TkIdent "struct") _ : Token (TkIdent name) _ : Token (TkSymbol "{") _ : rest) -> do
      (fields, rest1) <- parseStructFields feats [] rest
      case rest1 of
        (Token (TkSymbol ";") _ : more) -> Right (Just (StructDecl name fields), more)
        _ -> Right (Just (StructDecl name fields), rest1)
    _ -> Left (errorAt toks "malformed struct declaration")

parseStructFields :: FeatureSet -> [FieldDecl] -> [Token] -> Either CompileError ([FieldDecl], [Token])
parseStructFields feats acc rest =
  case rest of
    (Token (TkSymbol "}") _ : more) -> Right (reverse acc, more)
    _ -> do
      (mField, rest1) <- parseStructField feats rest
      let rest2 = case rest1 of
            (Token (TkSymbol ",") _ : more) -> more
            (Token (TkSymbol ";") _ : more) -> more
            _ -> rest1
      let acc' = case mField of
            Just field -> field : acc
            Nothing -> acc
      parseStructFields feats acc' rest2

parseStructField :: FeatureSet -> [Token] -> Either CompileError (Maybe FieldDecl, [Token])
parseStructField feats toks = do
  (attrs, rest1) <- parseAttributes toks
  (keep, attrs') <- applyIf attrs feats rest1
  case rest1 of
    (Token (TkIdent name) _ : Token (TkSymbol ":") _ : rest) -> do
      (ty, rest2) <- parseType rest
      let field = FieldDecl name ty attrs'
      Right (if keep then Just field else Nothing, rest2)
    _ -> Left (errorAt rest1 "expected struct field")

parseImportStatement :: [Token] -> Either CompileError (ImportDecl, [Token])
parseImportStatement toks =
  case toks of
    (Token (TkIdent "import") _ : rest) -> do
      (rel, rest1) <- parseImportRelative rest
      (items, rest2) <- parseImportClause rest1
      rest3 <- expectSymbol ";" rest2
      Right (ImportDecl rel items, rest3)
    _ -> Left (errorAt toks "expected import declaration")

parseImportRelative :: [Token] -> Either CompileError (Maybe ImportRelative, [Token])
parseImportRelative toks =
  case toks of
    (Token (TkIdent "package") _ : Token (TkSymbol "::") _ : rest) ->
      Right (Just ImportPackage, rest)
    (Token (TkIdent "super") _ : Token (TkSymbol "::") _ : rest) -> do
      (count, rest1) <- parseSuperChain 1 rest
      Right (Just (ImportSuper count), rest1)
    _ -> Right (Nothing, toks)
  where
    parseSuperChain count rest =
      case rest of
        (Token (TkIdent "super") _ : Token (TkSymbol "::") _ : more) ->
          parseSuperChain (count + 1) more
        _ -> Right (count, rest)

parseImportClause :: [Token] -> Either CompileError ([ImportItem], [Token])
parseImportClause toks =
  case toks of
    (Token (TkSymbol "{") _ : _) -> parseImportCollection toks
    _ -> parseImportPathOrItem toks

parseImportCollection :: [Token] -> Either CompileError ([ImportItem], [Token])
parseImportCollection toks =
  case toks of
    (Token (TkSymbol "{") _ : rest) -> parseImportItems [] rest
    _ -> Left (errorAt toks "expected import collection")
  where
    parseImportItems acc rest =
      case rest of
        (Token (TkSymbol "}") _ : more) -> Right (acc, more)
        _ -> do
          (items, rest1) <- parseImportPathOrItem rest
          let acc' = acc <> items
          case rest1 of
            (Token (TkSymbol ",") _ : more) -> parseImportItems acc' more
            (Token (TkSymbol "}") _ : more) -> Right (acc', more)
            _ -> Left (errorAt rest1 "expected ',' or '}' in import collection")

parseImportPathOrItem :: [Token] -> Either CompileError ([ImportItem], [Token])
parseImportPathOrItem toks = do
  (name, rest) <- parseIdent toks
  case rest of
    (Token (TkSymbol "::") _ : more) ->
      case more of
        (Token (TkSymbol "{") _ : _) -> do
          (items, rest1) <- parseImportCollection more
          Right (prefix name items, rest1)
        _ -> do
          (items, rest1) <- parseImportPathOrItem more
          Right (prefix name items, rest1)
    _ -> do
      (alias, rest1) <- parseImportAlias rest
      Right ([ImportItem [name] alias], rest1)
  where
    prefix name items = [item { iiPath = name : iiPath item } | item <- items]

parseImportAlias :: [Token] -> Either CompileError (Maybe Text, [Token])
parseImportAlias toks =
  case toks of
    (Token (TkIdent "as") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      Right (Just name, rest1)
    _ -> Right (Nothing, toks)

parseBinding :: [Attr] -> [Token] -> Either CompileError (BindingDecl, [Token])
parseBinding attrs toks =
  case toks of
    (Token (TkIdent "var") _ : rest) -> do
      (addrSpace, access, rest1) <- parseAddressSpaceMaybe rest
      (name, rest2) <- parseIdent rest1
      rest3 <- expectSymbol ":" rest2
      (ty, rest4) <- parseType rest3
      rest5 <- expectSymbol ";" rest4
      (group, binding) <- bindingNumbers attrs
      kind <- case addrSpace of
        Just space -> toBindingKind space access ty
        Nothing -> bindingKindFromType ty
      Right (BindingDecl name kind group binding ty, rest5)
    _ -> Left (errorAt toks "expected var declaration")

parseGlobalVar :: [Attr] -> [Token] -> Either CompileError (GlobalVarDecl, [Token])
parseGlobalVar attrs toks = do
  when (not (null attrs)) $
    Left (errorAt toks "global variables do not accept attributes other than @if")
  case toks of
    (Token (TkIdent "var") _ : rest) -> do
      (addrSpace, access, rest1) <- parseAddressSpaceMaybe rest
      space <- case addrSpace of
        Just s -> Right s
        Nothing -> Left (errorAt rest "global variables require an explicit address space (e.g. var<private>)")
      when (access /= Nothing) $
        Left (errorAt rest "global variables do not support access qualifiers")
      (name, rest2) <- parseIdent rest1
      rest3 <- expectSymbol ":" rest2
      (ty, rest4) <- parseType rest3
      (initExpr, rest5) <- parseGlobalVarInit rest4
      rest6 <- expectSymbol ";" rest5
      Right (GlobalVarDecl name space ty initExpr, rest6)
    _ -> Left (errorAt toks "expected var declaration")
  where
    parseGlobalVarInit rest =
      case rest of
        (Token (TkSymbol "=") _ : more) -> do
          (expr, rest1) <- parseExpr more
          Right (Just expr, rest1)
        _ -> Right (Nothing, rest)

parseConstDecl :: [Attr] -> [Token] -> Either CompileError (ConstDecl, [Token])
parseConstDecl attrs toks = do
  when (not (null attrs)) $
    Left (errorAt toks "const declarations do not accept attributes other than @if")
  case toks of
    (Token (TkIdent "let") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      rest2 <- expectSymbol "=" rest1
      (expr, rest3) <- parseExpr rest2
      rest4 <- expectSymbol ";" rest3
      Right (ConstDecl name expr, rest4)
    _ -> Left (errorAt toks "expected let declaration")

parseAliasDecl :: [Token] -> Either CompileError (AliasDecl, [Token])
parseAliasDecl toks =
  case toks of
    (Token (TkIdent "alias") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      rest2 <- expectSymbol "=" rest1
      (ty, rest3) <- parseType rest2
      rest4 <- expectSymbol ";" rest3
      Right (AliasDecl name ty, rest4)
    _ -> Left (errorAt toks "expected alias declaration")

parseOverrideDecl :: Maybe Word32 -> [Token] -> Either CompileError (OverrideDecl, [Token])
parseOverrideDecl overrideId toks =
  case toks of
    (Token (TkIdent "override") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      (mType, rest2) <- parseOverrideTypeMaybe rest1
      (mExpr, rest3) <- parseOverrideInitMaybe rest2
      rest4 <- expectSymbol ";" rest3
      ty <- case mType of
        Nothing -> Left (errorAt rest1 "override declarations require an explicit type for now")
        Just t -> Right t
      Right (OverrideDecl name overrideId ty mExpr, rest4)
    _ -> Left (errorAt toks "expected override declaration")
  where
    parseOverrideTypeMaybe rest =
      case rest of
        (Token (TkSymbol ":") _ : more) -> do
          (ty, rest1) <- parseType more
          Right (Just ty, rest1)
        _ -> Right (Nothing, rest)

    parseOverrideInitMaybe rest =
      case rest of
        (Token (TkSymbol "=") _ : more) -> do
          (expr, rest1) <- parseExpr more
          Right (Just expr, rest1)
        _ -> Right (Nothing, rest)

parseConstAssert :: [Token] -> Either CompileError (ConstAssert, [Token])
parseConstAssert toks =
  case toks of
    (Token (TkIdent "const_assert") pos : rest) -> do
      rest1 <- expectSymbol "(" rest
      (expr, rest2) <- parseExpr rest1
      rest3 <- expectSymbol ")" rest2
      rest4 <- expectSymbol ";" rest3
      Right (ConstAssert pos expr, rest4)
    _ -> Left (errorAt toks "expected const_assert")

parseEnableDirective :: [Token] -> Either CompileError (Directive, [Token])
parseEnableDirective toks =
  case toks of
    (Token (TkIdent "enable") _ : rest) -> do
      (name, rest1) <- parseFullIdent rest
      rest2 <- expectSymbol ";" rest1
      Right (DirEnable name, rest2)
    _ -> Left (errorAt toks "expected enable directive")

parseDiagnosticDirective :: [Token] -> Either CompileError (Directive, [Token])
parseDiagnosticDirective toks =
  case toks of
    (Token (TkIdent "diagnostic") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (severityName, rest2) <- parseIdent rest1
      rest3 <- expectSymbol "," rest2
      (ruleName, rest4) <- parseFullIdent rest3
      rest5 <- expectSymbol ")" rest4
      rest6 <- expectSymbol ";" rest5
      severity <- parseDiagnosticSeverity severityName rest1
      Right (DirDiagnostic severity ruleName, rest6)
    _ -> Left (errorAt toks "expected diagnostic directive")
  where
    parseDiagnosticSeverity name toks' =
      case name of
        "error" -> Right DiagError
        "warning" -> Right DiagWarning
        "info" -> Right DiagInfo
        "off" -> Right DiagOff
        _ -> Left (errorAt toks' "unknown diagnostic severity")

parseAddressSpaceMaybe :: [Token] -> Either CompileError (Maybe Text, Maybe Text, [Token])
parseAddressSpaceMaybe toks =
  case toks of
    (Token (TkSymbol "<") _ : _) -> do
      (addr, access, rest) <- parseAddressSpace toks
      Right (Just addr, access, rest)
    _ -> Right (Nothing, Nothing, toks)

parseAddressSpace :: [Token] -> Either CompileError (Text, Maybe Text, [Token])
parseAddressSpace toks =
  case toks of
    (Token (TkSymbol "<") _ : Token (TkIdent addr) _ : rest) ->
      case rest of
        (Token (TkSymbol ",") _ : Token (TkIdent access) _ : Token (TkSymbol ">") _ : more) ->
          Right (addr, Just access, more)
        (Token (TkSymbol ">") _ : more) -> Right (addr, Nothing, more)
        _ -> Left (errorAt rest "expected address space qualifier")
    _ -> Left (errorAt toks "expected address space qualifiers")

parseFunctionGeneric :: FeatureSet -> [Token] -> Either CompileError (Text, [Param], Maybe Type, Maybe Word32, Maybe Text, [Stmt], [Token])
parseFunctionGeneric feats toks =
  case toks of
    (Token (TkIdent "fn") _ : Token (TkIdent name) _ : Token (TkSymbol "(") _ : rest) -> do
      (params, rest1) <- parseParams feats rest
      rest2 <- expectSymbol ")" rest1
      (rest3, retType, retLoc, retBuiltin) <- parseReturnType rest2
      (body, rest4) <- parseBody feats rest3
      Right (name, params, retType, retLoc, retBuiltin, body, rest4)
    _ -> Left (errorAt toks "expected fn declaration")

parseParams :: FeatureSet -> [Token] -> Either CompileError ([Param], [Token])
parseParams feats toks =
  case toks of
    (Token (TkSymbol ")") _ : _) -> Right ([], toks)
    _ -> parseParamList [] toks
  where
    parseParamList acc rest = do
      (mParam, rest1) <- parseParam rest
      case rest1 of
        (Token (TkSymbol ",") _ : more) ->
          case mParam of
            Just param -> parseParamList (param:acc) more
            Nothing -> parseParamList acc more
        _ ->
          case mParam of
            Just param -> Right (reverse (param:acc), rest1)
            Nothing -> Right (reverse acc, rest1)

    parseParam rest = do
      (attrs, rest1) <- parseAttributes rest
      (keep, attrs') <- applyIf attrs feats rest1
      (name, rest2) <- parseIdent rest1
      rest3 <- expectSymbol ":" rest2
      (ty, rest4) <- parseType rest3
      let param = Param name ty attrs'
      pure (if keep then Just param else Nothing, rest4)

parseReturnType :: [Token] -> Either CompileError ([Token], Maybe Type, Maybe Word32, Maybe Text)
parseReturnType toks =
  case toks of
    (Token (TkSymbol "-") _ : Token (TkSymbol ">") _ : rest) -> do
      (retAttrs, rest1) <- parseAttributes rest
      case rest1 of
        (Token (TkIdent "void") _ : more) -> Right (more, Nothing, attrIntMaybe "location" retAttrs, attrBuiltin retAttrs)
        _ -> do
          (ty, rest2) <- parseType rest1
          Right (rest2, Just ty, attrIntMaybe "location" retAttrs, attrBuiltin retAttrs)
    _ -> Right (toks, Nothing, Nothing, Nothing)

parseBody :: FeatureSet -> [Token] -> Either CompileError ([Stmt], [Token])
parseBody feats toks =
  case toks of
    (Token (TkSymbol "{") _ : rest) -> parseStatements feats [] rest
    _ -> Left (errorAt toks "expected function body")

parseStatements :: FeatureSet -> [Stmt] -> [Token] -> Either CompileError ([Stmt], [Token])
parseStatements feats acc toks =
  case toks of
    (Token (TkSymbol "}") _ : more) -> Right (reverse acc, more)
    _ -> do
      (attrs, rest1) <- parseAttributes toks
      (keep, attrs') <- applyIf attrs feats rest1
      when (not (null attrs')) $
        Left (errorAt rest1 "statement attributes are not supported (except @if)")
      (stmt, rest) <- parseStmt feats rest1
      let acc' = if keep then stmt : acc else acc
      parseStatements feats acc' rest

parseStmt :: FeatureSet -> [Token] -> Either CompileError (Stmt, [Token])
parseStmt feats toks =
  case toks of
    (Token (TkSymbol "++") _ : rest) -> do
      (lv, rest1) <- parseLValue rest
      rest2 <- expectSymbol ";" rest1
      Right (SInc lv, rest2)
    (Token (TkSymbol "--") _ : rest) -> do
      (lv, rest1) <- parseLValue rest
      rest2 <- expectSymbol ";" rest1
      Right (SDec lv, rest2)
    (Token (TkIdent "if") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (cond, rest2) <- parseExpr rest1
      rest3 <- expectSymbol ")" rest2
      (thenBody, rest4) <- parseBody feats rest3
      case rest4 of
        (Token (TkIdent "else") _ : more) -> do
          (elseBody, rest5) <- parseBody feats more
          Right (SIf cond thenBody (Just elseBody), rest5)
        _ -> Right (SIf cond thenBody Nothing, rest4)
    (Token (TkIdent "switch") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (expr, rest2) <- parseExpr rest1
      rest3 <- expectSymbol ")" rest2
      (cases, defBody, rest4) <- parseSwitchBody feats rest3
      Right (SSwitch expr cases defBody, rest4)
    (Token (TkIdent "loop") _ : rest) -> do
      (body, continuing, rest1) <- parseLoopBody feats rest
      Right (SLoop body continuing, rest1)
    (Token (TkIdent "while") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (cond, rest2) <- parseExpr rest1
      rest3 <- expectSymbol ")" rest2
      (body, rest4) <- parseBody feats rest3
      Right (SWhile cond body, rest4)
    (Token (TkIdent "for") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (initStmt, rest2) <- parseForClause rest1
      rest3 <- expectSymbol ";" rest2
      (condExpr, rest4) <- parseForCond rest3
      rest5 <- expectSymbol ";" rest4
      (contStmt, rest6) <- parseForClause rest5
      rest7 <- expectSymbol ")" rest6
      (body, rest8) <- parseBody feats rest7
      Right (SFor initStmt condExpr contStmt body, rest8)
    (Token (TkIdent "break") _ : rest) ->
      case rest of
        (Token (TkIdent "if") _ : more) -> do
          rest1 <- expectSymbol "(" more
          (cond, rest2) <- parseExpr rest1
          rest3 <- expectSymbol ")" rest2
          rest4 <- expectSymbol ";" rest3
          Right (SBreakIf cond, rest4)
        _ -> do
          rest1 <- expectSymbol ";" rest
          Right (SBreak, rest1)
    (Token (TkIdent "continue") _ : rest) -> do
      rest1 <- expectSymbol ";" rest
      Right (SContinue, rest1)
    (Token (TkIdent "discard") _ : rest) -> do
      rest1 <- expectSymbol ";" rest
      Right (SDiscard, rest1)
    (Token (TkIdent "fallthrough") _ : rest) -> do
      rest1 <- expectSymbol ";" rest
      Right (SFallthrough, rest1)
    (Token (TkIdent "let") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      rest2 <- expectSymbol "=" rest1
      (expr, rest3) <- parseExpr rest2
      rest4 <- expectSymbol ";" rest3
      Right (SLet name expr, rest4)
    (Token (TkIdent "var") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      rest2 <- expectSymbol "=" rest1
      (expr, rest3) <- parseExpr rest2
      rest4 <- expectSymbol ";" rest3
      Right (SVar name expr, rest4)
    (Token (TkIdent "return") _ : rest) ->
      case rest of
        (Token (TkSymbol ";") _ : more) -> Right (SReturn Nothing, more)
        _ -> do
          (expr, rest1) <- parseExpr rest
          rest2 <- expectSymbol ";" rest1
          Right (SReturn (Just expr), rest2)
    _ -> do
      (lv, rest1) <- parseLValue toks
      case rest1 of
        (Token (TkSymbol "++") _ : restInc) -> do
          rest2 <- expectSymbol ";" restInc
          Right (SInc lv, rest2)
        (Token (TkSymbol "--") _ : restDec) -> do
          rest2 <- expectSymbol ";" restDec
          Right (SDec lv, rest2)
        (Token (TkSymbol "=") _ : restEq) -> do
          (expr, rest2) <- parseExpr restEq
          rest3 <- expectSymbol ";" rest2
          Right (SAssign lv expr, rest3)
        (Token (TkSymbol sym) _ : restEq)
          | Just op <- assignOpFromSymbol sym -> do
              (expr, rest2) <- parseExpr restEq
              rest3 <- expectSymbol ";" rest2
              Right (SAssignOp lv op expr, rest3)
        _ -> do
          (expr, rest2) <- parseExpr toks
          rest3 <- expectSymbol ";" rest2
          Right (SExpr expr, rest3)

parseSwitchBody :: FeatureSet -> [Token] -> Either CompileError ([SwitchCase], Maybe [Stmt], [Token])
parseSwitchBody feats toks =
  case toks of
    (Token (TkSymbol "{") _ : rest) -> parseCases [] Nothing rest
    _ -> Left (errorAt toks "expected '{' after switch")
  where
    parseCases acc def rest =
      case rest of
        (Token (TkSymbol "}") _ : more) -> Right (reverse acc, def, more)
        _ -> do
          (attrs, rest1) <- parseAttributes rest
          (keep, attrs') <- applyIf attrs feats rest1
          when (not (null attrs')) $
            Left (errorAt rest1 "switch case attributes are not supported (except @if)")
          case rest1 of
            (Token (TkIdent "case") _ : more) -> do
              (selectors, rest2) <- parseCaseSelectors more
              rest3 <- expectSymbol ":" rest2
              (body, rest4) <- parseBody feats rest3
              let acc' = if keep then SwitchCase selectors body : acc else acc
              parseCases acc' def rest4
            (Token (TkIdent "default") _ : more) -> do
              rest2 <- expectSymbol ":" more
              (body, rest3) <- parseBody feats rest2
              case def of
                Just _ -> Left (errorAt rest1 "multiple default cases in switch")
                Nothing -> parseCases acc (if keep then Just body else Nothing) rest3
            _ -> Left (errorAt rest1 "expected case or default in switch")

    parseCaseSelectors rest = do
      (expr, rest1) <- parseExpr rest
      parseMore [expr] rest1

    parseMore acc rest =
      case rest of
        (Token (TkSymbol ",") _ : more) -> do
          (expr, rest1) <- parseExpr more
          parseMore (acc <> [expr]) rest1
        _ -> Right (acc, rest)

parseLoopBody :: FeatureSet -> [Token] -> Either CompileError ([Stmt], Maybe [Stmt], [Token])
parseLoopBody feats toks =
  case toks of
    (Token (TkSymbol "{") _ : rest) -> parseLoopStmts [] rest
    _ -> Left (errorAt toks "expected '{' after loop")
  where
    parseLoopStmts acc rest =
      case rest of
        (Token (TkSymbol "}") _ : more) -> Right (reverse acc, Nothing, more)
        _ -> do
          (attrs, rest1) <- parseAttributes rest
          (keep, attrs') <- applyIf attrs feats rest1
          case rest1 of
            (Token (TkIdent "continuing") _ : more) -> do
              when (not (null attrs')) $
                Left (errorAt rest1 "continuing attributes are not supported (except @if)")
              (contBody, rest2) <- parseBody feats more
              case rest2 of
                (Token (TkSymbol "}") _ : more2) ->
                  Right (reverse acc, if keep then Just contBody else Nothing, more2)
                _ -> Left (errorAt rest2 "expected '}' after continuing block")
            _ -> do
              when (not (null attrs')) $
                Left (errorAt rest1 "statement attributes are not supported (except @if)")
              (stmt, rest2) <- parseStmt feats rest1
              let acc' = if keep then stmt : acc else acc
              parseLoopStmts acc' rest2

parseForClause :: [Token] -> Either CompileError (Maybe Stmt, [Token])
parseForClause toks =
  case toks of
    (Token (TkSymbol ";") _ : _) -> Right (Nothing, toks)
    (Token (TkSymbol ")") _ : _) -> Right (Nothing, toks)
    (Token (TkSymbol "++") _ : rest) -> do
      (lv, rest1) <- parseLValue rest
      Right (Just (SInc lv), rest1)
    (Token (TkSymbol "--") _ : rest) -> do
      (lv, rest1) <- parseLValue rest
      Right (Just (SDec lv), rest1)
    (Token (TkIdent "let") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      rest2 <- expectSymbol "=" rest1
      (expr, rest3) <- parseExpr rest2
      Right (Just (SLet name expr), rest3)
    (Token (TkIdent "var") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      rest2 <- expectSymbol "=" rest1
      (expr, rest3) <- parseExpr rest2
      Right (Just (SVar name expr), rest3)
    _ -> do
      (lv, rest1) <- parseLValue toks
      case rest1 of
        (Token (TkSymbol "++") _ : restInc) -> Right (Just (SInc lv), restInc)
        (Token (TkSymbol "--") _ : restDec) -> Right (Just (SDec lv), restDec)
        (Token (TkSymbol "=") _ : restEq) -> do
          (expr, rest3) <- parseExpr restEq
          Right (Just (SAssign lv expr), rest3)
        (Token (TkSymbol sym) _ : restEq)
          | Just op <- assignOpFromSymbol sym -> do
              (expr, rest3) <- parseExpr restEq
              Right (Just (SAssignOp lv op expr), rest3)
        _ -> Left (errorAt rest1 "expected assignment in for clause")

parseForCond :: [Token] -> Either CompileError (Maybe Expr, [Token])
parseForCond toks =
  case toks of
    (Token (TkSymbol ";") _ : _) -> Right (Nothing, toks)
    _ -> do
      (expr, rest) <- parseExpr toks
      Right (Just expr, rest)

assignOpFromSymbol :: Text -> Maybe BinOp
assignOpFromSymbol sym =
  case sym of
    "+=" -> Just OpAdd
    "-=" -> Just OpSub
    "*=" -> Just OpMul
    "/=" -> Just OpDiv
    "%=" -> Just OpMod
    "&=" -> Just OpBitAnd
    "|=" -> Just OpBitOr
    "^=" -> Just OpBitXor
    "<<=" -> Just OpShl
    ">>=" -> Just OpShr
    _ -> Nothing

parseLValue :: [Token] -> Either CompileError (LValue, [Token])
parseLValue toks =
  case toks of
    (Token (TkSymbol "*") _ : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (LVDeref expr, rest1)
    _ -> do
      (name, rest) <- parseFullIdent toks
      parseLValueTail (LVVar name) rest

parseLValueTail :: LValue -> [Token] -> Either CompileError (LValue, [Token])
parseLValueTail lv toks =
  case toks of
    (Token (TkSymbol ".") _ : Token (TkIdent field) _ : rest) ->
      parseLValueTail (LVField lv field) rest
    (Token (TkSymbol "[") _ : rest) -> do
      (idx, rest1) <- parseExpr rest
      rest2 <- expectSymbol "]" rest1
      parseLValueTail (LVIndex lv idx) rest2
    _ -> Right (lv, toks)

parseExpr :: [Token] -> Either CompileError (Expr, [Token])
parseExpr = parseLogicalOr

parseLogicalOr :: [Token] -> Either CompileError (Expr, [Token])
parseLogicalOr toks = do
  (lhs, rest) <- parseLogicalAnd toks
  parseLogicalOrTail lhs rest
  where
    parseLogicalOrTail lhs toks' =
      case toks' of
        (Token (TkSymbol "||") _ : rest) -> do
          (rhs, rest1) <- parseLogicalAnd rest
          parseLogicalOrTail (EBinary OpOr lhs rhs) rest1
        _ -> Right (lhs, toks')

parseLogicalAnd :: [Token] -> Either CompileError (Expr, [Token])
parseLogicalAnd toks = do
  (lhs, rest) <- parseBitOr toks
  parseLogicalAndTail lhs rest
  where
    parseLogicalAndTail lhs toks' =
      case toks' of
        (Token (TkSymbol "&&") _ : rest) -> do
          (rhs, rest1) <- parseBitOr rest
          parseLogicalAndTail (EBinary OpAnd lhs rhs) rest1
        _ -> Right (lhs, toks')

parseBitOr :: [Token] -> Either CompileError (Expr, [Token])
parseBitOr toks = do
  (lhs, rest) <- parseBitXor toks
  parseBitOrTail lhs rest
  where
    parseBitOrTail lhs toks' =
      case toks' of
        (Token (TkSymbol "|") _ : rest) -> do
          (rhs, rest1) <- parseBitXor rest
          parseBitOrTail (EBinary OpBitOr lhs rhs) rest1
        _ -> Right (lhs, toks')

parseBitXor :: [Token] -> Either CompileError (Expr, [Token])
parseBitXor toks = do
  (lhs, rest) <- parseBitAnd toks
  parseBitXorTail lhs rest
  where
    parseBitXorTail lhs toks' =
      case toks' of
        (Token (TkSymbol "^") _ : rest) -> do
          (rhs, rest1) <- parseBitAnd rest
          parseBitXorTail (EBinary OpBitXor lhs rhs) rest1
        _ -> Right (lhs, toks')

parseBitAnd :: [Token] -> Either CompileError (Expr, [Token])
parseBitAnd toks = do
  (lhs, rest) <- parseEquality toks
  parseBitAndTail lhs rest
  where
    parseBitAndTail lhs toks' =
      case toks' of
        (Token (TkSymbol "&") _ : rest) -> do
          (rhs, rest1) <- parseEquality rest
          parseBitAndTail (EBinary OpBitAnd lhs rhs) rest1
        _ -> Right (lhs, toks')

parseEquality :: [Token] -> Either CompileError (Expr, [Token])
parseEquality toks = do
  (lhs, rest) <- parseRelational toks
  parseEqualityTail lhs rest
  where
    parseEqualityTail lhs toks' =
      case toks' of
        (Token (TkSymbol "==") _ : rest) -> do
          (rhs, rest1) <- parseRelational rest
          parseEqualityTail (EBinary OpEq lhs rhs) rest1
        (Token (TkSymbol "!=") _ : rest) -> do
          (rhs, rest1) <- parseRelational rest
          parseEqualityTail (EBinary OpNe lhs rhs) rest1
        _ -> Right (lhs, toks')

parseRelational :: [Token] -> Either CompileError (Expr, [Token])
parseRelational toks = do
  (lhs, rest) <- parseShift toks
  parseRelationalTail lhs rest
  where
    parseRelationalTail lhs toks' =
      case toks' of
        (Token (TkSymbol "<") _ : rest) -> do
          (rhs, rest1) <- parseShift rest
          parseRelationalTail (EBinary OpLt lhs rhs) rest1
        (Token (TkSymbol "<=") _ : rest) -> do
          (rhs, rest1) <- parseShift rest
          parseRelationalTail (EBinary OpLe lhs rhs) rest1
        (Token (TkSymbol ">") _ : rest) -> do
          (rhs, rest1) <- parseShift rest
          parseRelationalTail (EBinary OpGt lhs rhs) rest1
        (Token (TkSymbol ">=") _ : rest) -> do
          (rhs, rest1) <- parseShift rest
          parseRelationalTail (EBinary OpGe lhs rhs) rest1
        _ -> Right (lhs, toks')

parseShift :: [Token] -> Either CompileError (Expr, [Token])
parseShift toks = do
  (lhs, rest) <- parseAddSub toks
  parseShiftTail lhs rest
  where
    parseShiftTail lhs toks' =
      case toks' of
        (Token (TkSymbol "<<") _ : rest) -> do
          (rhs, rest1) <- parseAddSub rest
          parseShiftTail (EBinary OpShl lhs rhs) rest1
        (Token (TkSymbol ">>") _ : rest) -> do
          (rhs, rest1) <- parseAddSub rest
          parseShiftTail (EBinary OpShr lhs rhs) rest1
        _ -> Right (lhs, toks')

parseAddSub :: [Token] -> Either CompileError (Expr, [Token])
parseAddSub toks = do
  (lhs, rest) <- parseMulDiv toks
  parseAddSubTail lhs rest
  where
    parseAddSubTail lhs toks' =
      case toks' of
        (Token (TkSymbol "+") _ : rest) -> do
          (rhs, rest1) <- parseMulDiv rest
          parseAddSubTail (EBinary OpAdd lhs rhs) rest1
        (Token (TkSymbol "-") _ : rest) -> do
          (rhs, rest1) <- parseMulDiv rest
          parseAddSubTail (EBinary OpSub lhs rhs) rest1
        _ -> Right (lhs, toks')

parseMulDiv :: [Token] -> Either CompileError (Expr, [Token])
parseMulDiv toks = do
  (lhs, rest) <- parseUnaryExpr toks
  parseMulDivTail lhs rest
  where
    parseMulDivTail lhs toks' =
      case toks' of
        (Token (TkSymbol "*") _ : rest) -> do
          (rhs, rest1) <- parseUnaryExpr rest
          parseMulDivTail (EBinary OpMul lhs rhs) rest1
        (Token (TkSymbol "/") _ : rest) -> do
          (rhs, rest1) <- parseUnaryExpr rest
          parseMulDivTail (EBinary OpDiv lhs rhs) rest1
        (Token (TkSymbol "%") _ : rest) -> do
          (rhs, rest1) <- parseUnaryExpr rest
          parseMulDivTail (EBinary OpMod lhs rhs) rest1
        _ -> Right (lhs, toks')

parseUnaryExpr :: [Token] -> Either CompileError (Expr, [Token])
parseUnaryExpr toks =
  case toks of
    (Token (TkSymbol "-") _ : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (EUnary OpNeg expr, rest1)
    (Token (TkSymbol "!") _ : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (EUnary OpNot expr, rest1)
    (Token (TkSymbol "&") _ : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (EUnary OpAddr expr, rest1)
    (Token (TkSymbol "*") _ : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (EUnary OpDeref expr, rest1)
    _ -> parsePostfixExpr toks

parsePostfixExpr :: [Token] -> Either CompileError (Expr, [Token])
parsePostfixExpr toks = do
  (base, rest) <- parsePrimaryExpr toks
  parsePostfixTail base rest
  where
    parsePostfixTail expr toks' =
      case toks' of
        (Token (TkSymbol ".") _ : Token (TkIdent field) _ : rest) ->
          parsePostfixTail (EField expr field) rest
        (Token (TkSymbol "[") _ : rest) -> do
          (idx, rest1) <- parseExpr rest
          rest2 <- expectSymbol "]" rest1
          parsePostfixTail (EIndex expr idx) rest2
        _ -> Right (expr, toks')

parsePrimaryExpr :: [Token] -> Either CompileError (Expr, [Token])
parsePrimaryExpr toks =
  case toks of
    (Token (TkInt n) _ : rest) -> Right (EInt n, rest)
    (Token (TkFloat f) _ : rest) -> Right (EFloat f, rest)
    (Token (TkIdent "true") _ : rest) -> Right (EBool True, rest)
    (Token (TkIdent "false") _ : rest) -> Right (EBool False, rest)
    (Token (TkIdent _) _ : _) -> do
      (name, rest) <- parseFullIdent toks
      case rest of
        (Token (TkSymbol "<") _ : _)
          | name == "bitcast" -> do
              rest1 <- expectSymbol "<" rest
              (targetTy, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              rest4 <- expectSymbol "(" rest3
              (args, rest5) <- parseCallArgs [] rest4
              case args of
                [arg] -> Right (EBitcast targetTy arg, rest5)
                _ -> Left (errorAt rest4 "bitcast expects one argument")
        (Token (TkSymbol "(") _ : more) -> do
          (args, rest1) <- parseCallArgs [] more
          Right (ECall name args, rest1)
        _ -> Right (EVar name, rest)
    (Token (TkSymbol "(") _ : rest) -> do
      (expr, rest1) <- parseExpr rest
      rest2 <- expectSymbol ")" rest1
      Right (expr, rest2)
    _ -> Left (errorAt toks "expected expression")

parseCallArgs :: [Expr] -> [Token] -> Either CompileError ([Expr], [Token])
parseCallArgs acc toks =
  case toks of
    (Token (TkSymbol ")") _ : rest) -> Right (reverse acc, rest)
    _ -> do
      (expr, rest1) <- parseExpr toks
      case rest1 of
        (Token (TkSymbol ",") _ : rest2) -> parseCallArgs (expr:acc) rest2
        (Token (TkSymbol ")") _ : rest2) -> Right (reverse (expr:acc), rest2)
        _ -> Left (errorAt rest1 "expected ',' or ')' in call arguments")

validateEntry :: EntryPoint -> Either CompileError ()
validateEntry entry =
  case epStage entry of
    StageCompute ->
      case epWorkgroupSize entry of
        Nothing -> Left (CompileError "@workgroup_size is required for @compute" Nothing Nothing)
        Just _ -> pure ()
    StageVertex ->
      case epWorkgroupSize entry of
        Nothing -> pure ()
        Just _ -> Left (CompileError "@workgroup_size is not allowed for @vertex" Nothing Nothing)
    StageFragment ->
      case epWorkgroupSize entry of
        Nothing -> pure ()
        Just _ -> Left (CompileError "@workgroup_size is not allowed for @fragment" Nothing Nothing)

-- Import resolution (subset: module imports + qualified names)

data ModuleNode = ModuleNode
  { mnFile :: !FilePath
  , mnPath :: ![Text]
  , mnAst :: !ModuleAst
  , mnImports :: ![ImportResolved]
  } deriving (Eq, Show)

data ImportResolved = ImportResolved
  { irModulePath :: ![Text]
  , irModuleFile :: !FilePath
  , irItem :: !(Maybe Text)
  , irAlias :: !(Maybe Text)
  } deriving (Eq, Show)

emptyModuleAst :: ModuleAst
emptyModuleAst = ModuleAst [] [] [] [] [] [] [] [] [] [] Nothing

resolveImports :: CompileOptions -> FilePath -> ModuleAst -> IO (Either CompileError ModuleAst)
resolveImports opts rootFile rootAst = do
  let rootDir = takeDirectory rootFile
  graph <- loadModuleGraph opts rootDir rootFile rootAst
  pure (graph >>= linkModules opts rootFile rootDir)

loadModuleGraph :: CompileOptions -> FilePath -> FilePath -> ModuleAst -> IO (Either CompileError [ModuleNode])
loadModuleGraph opts rootDir rootFile rootAst = go Map.empty [(rootFile, rootAst)]
  where
    go acc [] = pure (Right (Map.elems acc))
    go acc ((filePath, ast):queue) = do
      let pathSegs = modulePathFromFile rootDir filePath
      resolvedImports <- resolveImportItems opts rootDir filePath ast
      case resolvedImports of
        Left err -> pure (Left err)
        Right imports -> do
          case validateImportAliases imports of
            Left err -> pure (Left err)
            Right () -> do
              let node = ModuleNode filePath pathSegs ast imports
              let acc' = Map.insert filePath node acc
              loadTargets <- loadImportTargets opts acc' imports
              case loadTargets of
                Left err -> pure (Left err)
                Right targets -> go acc' (queue <> targets)

    loadImportTargets opts' acc' imports = do
      let moduleFiles = Map.keys (Map.fromList [(irModuleFile imp, ()) | imp <- imports])
      results <- mapM (loadOne opts' acc') moduleFiles
      pure (fmap concat (sequence results))

    loadOne opts' acc' moduleFile =
      if Map.member moduleFile acc'
        then pure (Right [])
        else do
          moduleAst <- loadModuleFromFile opts' moduleFile
          pure (fmap (\ast' -> [(moduleFile, ast')]) moduleAst)

validateImportAliases :: [ImportResolved] -> Either CompileError ()
validateImportAliases imports =
  let aliases = mapMaybe aliasFor imports
      (dups, _) = foldl' collect ([], Set.empty) aliases
      targets = map importTarget imports
      (dupTargets, _) = foldl' collect ([], Set.empty) targets
  in if not (null dups)
      then Left (CompileError ("duplicate import aliases: " <> T.unpack (T.intercalate ", " dups)) Nothing Nothing)
      else
        if null dupTargets
          then Right ()
          else Left (CompileError ("duplicate imports: " <> T.unpack (T.intercalate ", " dupTargets)) Nothing Nothing)
  where
    aliasFor imp =
      case irItem imp of
        Nothing ->
          let alias = fromMaybe (last (irModulePath imp)) (irAlias imp)
          in if T.null alias then Nothing else Just alias
        Just item ->
          let alias = fromMaybe item (irAlias imp)
          in if T.null alias then Nothing else Just alias

    importTarget imp =
      let modName = T.intercalate "::" (irModulePath imp)
      in case irItem imp of
          Nothing -> modName
          Just item -> modName <> "::" <> item

    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)

resolveImportItems :: CompileOptions -> FilePath -> FilePath -> ModuleAst -> IO (Either CompileError [ImportResolved])
resolveImportItems opts rootDir moduleFile ast = do
  let items = [(decl, item) | decl <- modImports ast, item <- idItems decl]
  results <- mapM (resolveOne opts rootDir moduleFile) items
  pure (sequence results)
  where
    resolveOne opts' rootDir' moduleFile' (decl, item) = resolveImportItem opts' rootDir' moduleFile' decl item

loadModuleFromFile :: CompileOptions -> FilePath -> IO (Either CompileError ModuleAst)
loadModuleFromFile opts path = do
  let candidateWesl = path <.> "wesl"
  let candidateWgsl = path <.> "wgsl"
  weslExists <- doesFileExist candidateWesl
  if weslExists
    then parseModule candidateWesl
    else do
      wgslExists <- doesFileExist candidateWgsl
      if wgslExists
        then parseModule candidateWgsl
        else pure (Right emptyModuleAst)
  where
    parseModule file = do
      src <- readFile file
      pure (parseModuleWith (enabledFeatures opts) src)

resolveImportItem :: CompileOptions -> FilePath -> FilePath -> ImportDecl -> ImportItem -> IO (Either CompileError ImportResolved)
resolveImportItem opts rootDir moduleFile decl item = do
  let baseDir = importBaseDir rootDir moduleFile (idRelative decl)
  let segs = iiPath item
  let fullBase = foldl (</>) baseDir (map T.unpack segs)
  fullMod <- findModuleFile fullBase
  case fullMod of
    Just moduleBase -> do
      ambiguous <- case segs of
        [] -> pure False
        [_] -> pure False
        _ -> do
          let modSegs = init segs
          let itemName = last segs
          let moduleBasePath = foldl (</>) baseDir (map T.unpack modSegs)
          moduleBaseItem <- findModuleFile moduleBasePath
          case moduleBaseItem of
            Nothing -> pure False
            Just mb -> do
              itemExists <- moduleHasItem opts mb itemName
              pure itemExists
      if ambiguous
        then pure (Left (CompileError ("ambiguous import: " <> T.unpack (T.intercalate "::" segs) <> " refers to both a module and an item") Nothing Nothing))
        else pure (Right (ImportResolved (modulePathFromFile rootDir moduleBase) moduleBase Nothing (iiAlias item)))
    Nothing ->
      case segs of
        [] -> pure (Left (CompileError "import path is empty" Nothing Nothing))
        [_] -> pure (Left (CompileError ("import module not found: " <> T.unpack (T.intercalate "::" segs)) Nothing Nothing))
        _ -> do
          let modSegs = init segs
          let itemName = last segs
          let moduleBasePath = foldl (</>) baseDir (map T.unpack modSegs)
          moduleBase <- findModuleFile moduleBasePath
          case moduleBase of
            Just mb ->
              pure (Right (ImportResolved (modulePathFromFile rootDir mb) mb (Just itemName) (iiAlias item)))
            Nothing ->
              pure (Left (CompileError ("import module not found: " <> T.unpack (T.intercalate "::" modSegs)) Nothing Nothing))

moduleHasItem :: CompileOptions -> FilePath -> Text -> IO Bool
moduleHasItem opts moduleBase itemName = do
  astResult <- loadModuleFromFile opts moduleBase
  case astResult of
    Left _ -> pure False
    Right ast ->
      pure $
        itemName `elem` (map sdName (modStructs ast))
          || itemName `elem` map bdName (modBindings ast)
          || itemName `elem` map gvName (modGlobals ast)
          || itemName `elem` map cdName (modConsts ast)
          || itemName `elem` map odName (modOverrides ast)
          || itemName `elem` map adName (modAliases ast)
          || itemName `elem` map fnName (modFunctions ast)
          || maybe False ((== itemName) . epName) (modEntry ast)

findModuleFile :: FilePath -> IO (Maybe FilePath)
findModuleFile base = do
  let weslPath = base <.> "wesl"
  let wgslPath = base <.> "wgsl"
  weslExists <- doesFileExist weslPath
  if weslExists
    then pure (Just base)
    else do
      wgslExists <- doesFileExist wgslPath
      if wgslExists
        then pure (Just base)
        else pure Nothing

modulePathFromFile :: FilePath -> FilePath -> [Text]
modulePathFromFile rootDir filePath =
  let rel = dropExtension (makeRelative rootDir filePath)
  in map T.pack (filter (not . null) (splitDirectories rel))

importBaseDir :: FilePath -> FilePath -> Maybe ImportRelative -> FilePath
importBaseDir rootDir moduleFile rel =
  case rel of
    Nothing -> rootDir
    Just ImportPackage -> rootDir
    Just (ImportSuper n) -> superModuleFile moduleFile n

superModuleFile :: FilePath -> Int -> FilePath
superModuleFile filePath n =
  iterate takeDirectory filePath !! n

linkModules :: CompileOptions -> FilePath -> FilePath -> [ModuleNode] -> Either CompileError ModuleAst
linkModules opts rootFile rootDir nodes = do
  rootNode <- case [n | n <- nodes, mnFile n == rootFile] of
    (n:_) -> Right n
    [] -> Left (CompileError "root module not found during import resolution" Nothing Nothing)
  let rootPath = mnPath rootNode
  let otherEntries = [n | n <- nodes, mnFile n /= rootFile, modEntry (mnAst n) /= Nothing]
  when (not (null otherEntries)) $
    Left (CompileError "entry points are only supported in the root module" Nothing Nothing)
  let constIndex = buildConstIndex nodes
  let fnIndex = buildFunctionIndex nodes
  let structIndex = buildStructIndex nodes
  let overrideIndex = buildOverrideIndex nodes
  validateModuleScopes opts True rootPath rootDir constIndex fnIndex structIndex overrideIndex nodes
  let contexts = Map.fromList [(mnFile n, buildModuleContext rootPath rootDir n) | n <- nodes]
  let qualified = map (\n -> qualifyModule rootPath contexts n) nodes
  let merged = foldl' mergeModule emptyModuleAst qualified
  let rootEntry =
        case [q | q <- qualified, mnFile q == rootFile] of
          (q:_) -> modEntry (mnAst q)
          [] -> Nothing
  Right merged { modEntry = rootEntry }

mergeModule :: ModuleAst -> ModuleNode -> ModuleAst
mergeModule acc node =
  let ast = mnAst node
  in acc
      { modDirectives = modDirectives acc <> modDirectives ast
      , modAliases = modAliases acc <> modAliases ast
      , modStructs = modStructs acc <> modStructs ast
      , modBindings = modBindings acc <> modBindings ast
      , modGlobals = modGlobals acc <> modGlobals ast
      , modConsts = modConsts acc <> modConsts ast
      , modOverrides = modOverrides acc <> modOverrides ast
      , modConstAsserts = modConstAsserts acc <> modConstAsserts ast
      , modFunctions = modFunctions acc <> modFunctions ast
      }

data ModuleContext = ModuleContext
  { mcPath :: [Text]
  , mcModuleAliases :: Map.Map Text [Text]
  , mcItemAliases :: Map.Map Text [Text]
  , mcLocals :: Set.Set Text
  , mcConstNames :: Set.Set Text
  , mcFunctionNames :: Set.Set Text
  , mcRootPath :: [Text]
  }

data NameTable = NameTable
  { ntNext :: !Int
  , ntMap :: !(Map.Map Text Int)
  } deriving (Eq, Show)

emptyNameTable :: NameTable
emptyNameTable = NameTable 0 Map.empty

internName :: Text -> NameTable -> (Int, NameTable)
internName name table =
  case Map.lookup name (ntMap table) of
    Just i -> (i, table)
    Nothing ->
      let i = ntNext table
          table' = table { ntNext = i + 1, ntMap = Map.insert name i (ntMap table) }
      in (i, table')

internNames :: NameTable -> [Text] -> (NameTable, [Int])
internNames table names =
  let (table', revIds) = foldl' step (table, []) names
  in (table', reverse revIds)
  where
    step (t, acc) name =
      let (i, t') = internName name t
      in (t', i : acc)

internNameSet :: NameTable -> [Text] -> (NameTable, IntSet.IntSet)
internNameSet table names =
  let (table', ids) = internNames table names
  in (table', IntSet.fromList ids)

internNamePairs :: NameTable -> [Text] -> (NameTable, [(Text, Int)])
internNamePairs table names =
  let (table', ids) = internNames table names
  in (table', zip names ids)

uniqueById :: [(Text, Int)] -> [(Text, Int)]
uniqueById = go IntSet.empty
  where
    go _ [] = []
    go seen ((name, ident) : rest)
      | IntSet.member ident seen = go seen rest
      | otherwise = (name, ident) : go (IntSet.insert ident seen) rest

pathKey :: [Text] -> Text
pathKey = T.intercalate "::"

buildModuleContext :: [Text] -> FilePath -> ModuleNode -> ModuleContext
buildModuleContext rootPath _rootDir node =
  let ast = mnAst node
      localNames =
        Set.fromList
          ( map sdName (modStructs ast)
            <> map bdName (modBindings ast)
            <> map gvName (modGlobals ast)
            <> map cdName (modConsts ast)
            <> map odName (modOverrides ast)
            <> map fnName (modFunctions ast)
            <> maybe [] (\e -> [epName e]) (modEntry ast)
          )
      constNames = Set.fromList (map cdName (modConsts ast) <> map odName (modOverrides ast))
      functionNames = Set.fromList (map fnName (modFunctions ast))
      (moduleAliases, itemAliases) = buildAliasMaps (mnImports node)
  in ModuleContext (mnPath node) moduleAliases itemAliases localNames constNames functionNames rootPath

data ConstIndex = ConstIndex
  { ciPathTable :: !NameTable
  , ciNameTable :: !NameTable
  , ciEntries :: !(IntMap.IntMap (IntMap.IntMap Expr))
  }

buildConstIndex :: [ModuleNode] -> ConstIndex
buildConstIndex nodes =
  let (pt, nt, entries) = foldl' addNode (emptyNameTable, emptyNameTable, IntMap.empty) nodes
  in ConstIndex pt nt entries
  where
    addNode (pt, nt, acc) node =
      let (pid, pt1) = internName (pathKey (mnPath node)) pt
          (nt1, entryMap) = foldl' addEntry (nt, IntMap.empty) (constPairs node)
          merged = IntMap.union entryMap (IntMap.findWithDefault IntMap.empty pid acc)
      in (pt1, nt1, IntMap.insert pid merged acc)
    addEntry (nt0, m) (name, expr) =
      let (nid, nt1) = internName name nt0
      in (nt1, IntMap.insert nid expr m)
    constPairs n =
      let ast = mnAst n
          consts = [(cdName c, cdExpr c) | c <- modConsts ast]
          overrides = [(odName o, expr) | o <- modOverrides ast, Just expr <- [odExpr o]]
      in consts <> overrides

lookupConstIndex :: ConstIndex -> [Text] -> Text -> Maybe Expr
lookupConstIndex idx path name = do
  pid <- Map.lookup (pathKey path) (ntMap (ciPathTable idx))
  nid <- Map.lookup name (ntMap (ciNameTable idx))
  IntMap.lookup pid (ciEntries idx) >>= IntMap.lookup nid

type OverrideIndex = Map.Map [Text] (Set.Set Text)

buildOverrideIndex :: [ModuleNode] -> OverrideIndex
buildOverrideIndex nodes =
  Map.fromList
    [ (mnPath n, Set.fromList (map odName (modOverrides (mnAst n))))
    | n <- nodes
    ]

data StructIndex = StructIndex
  { siPathTable :: !NameTable
  , siNameTable :: !NameTable
  , siEntries :: !(IntMap.IntMap (IntMap.IntMap StructDecl))
  }

buildStructIndex :: [ModuleNode] -> StructIndex
buildStructIndex nodes =
  let (pt, nt, entries) = foldl' addNode (emptyNameTable, emptyNameTable, IntMap.empty) nodes
  in StructIndex pt nt entries
  where
    addNode (pt, nt, acc) node =
      let (pid, pt1) = internName (pathKey (mnPath node)) pt
          (nt1, entryMap) = foldl' addEntry (nt, IntMap.empty) [(sdName s, s) | s <- modStructs (mnAst node)]
          merged = IntMap.union entryMap (IntMap.findWithDefault IntMap.empty pid acc)
      in (pt1, nt1, IntMap.insert pid merged acc)
    addEntry (nt0, m) (name, decl) =
      let (nid, nt1) = internName name nt0
      in (nt1, IntMap.insert nid decl m)

lookupStructIndex :: StructIndex -> [Text] -> Text -> Maybe StructDecl
lookupStructIndex idx path name = do
  pid <- Map.lookup (pathKey path) (ntMap (siPathTable idx))
  nid <- Map.lookup name (ntMap (siNameTable idx))
  IntMap.lookup pid (siEntries idx) >>= IntMap.lookup nid

data FunctionIndex = FunctionIndex
  { fiPathTable :: !NameTable
  , fiNameTable :: !NameTable
  , fiEntries :: !(IntMap.IntMap (IntMap.IntMap [FunctionDecl]))
  }

buildFunctionIndex :: [ModuleNode] -> FunctionIndex
buildFunctionIndex nodes =
  let (pt, nt, entries) = foldl' addNode (emptyNameTable, emptyNameTable, IntMap.empty) nodes
  in FunctionIndex pt nt entries
  where
    addNode (pt, nt, acc) node =
      let (pid, pt1) = internName (pathKey (mnPath node)) pt
          (nt1, entryMap) = foldl' addEntry (nt, IntMap.empty) [(fnName f, [f]) | f <- modFunctions (mnAst node)]
          merged = IntMap.unionWith (<>) entryMap (IntMap.findWithDefault IntMap.empty pid acc)
      in (pt1, nt1, IntMap.insert pid merged acc)
    addEntry (nt0, m) (name, decls) =
      let (nid, nt1) = internName name nt0
      in (nt1, IntMap.insertWith (<>) nid decls m)

lookupFunctionIndex :: FunctionIndex -> [Text] -> Text -> Maybe [FunctionDecl]
lookupFunctionIndex idx path name = do
  pid <- Map.lookup (pathKey path) (ntMap (fiPathTable idx))
  nid <- Map.lookup name (ntMap (fiNameTable idx))
  IntMap.lookup pid (fiEntries idx) >>= IntMap.lookup nid

lowerOverridesWith :: [Text] -> [(Text, OverrideValue)] -> ModuleAst -> Either CompileError ModuleAst
lowerOverridesWith rootPath overridesMap ast =
  case modOverrides ast of
    [] -> Right ast
    overrides -> do
      let existing = Set.fromList (map cdName (modConsts ast))
      let (dups, _) = foldl' collect ([], Set.empty) (map odName overrides)
      when (not (null dups)) $
        Left (CompileError ("duplicate override declarations: " <> T.unpack (T.intercalate ", " dups)) Nothing Nothing)
      when (any (`Set.member` existing) (map odName overrides)) $
        Left (CompileError "override names must not conflict with const declarations" Nothing Nothing)
      let overrideLookup = buildOverrideValueMap rootPath overridesMap
      let structEnv = [(sdName s, s) | s <- modStructs ast]
      (consts, kept) <- foldM (partitionOverride structEnv overrideLookup) ([], []) overrides
      Right ast { modConsts = modConsts ast <> reverse consts, modOverrides = reverse kept }
  where
    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)
    partitionOverride structEnv overrideLookup (constAcc, keepAcc) o =
      case Map.lookup (odName o) overrideLookup of
        Just ov -> do
          expr <- overrideValueToExpr structEnv (odType o) ov
          Right (ConstDecl (odName o) expr : constAcc, keepAcc)
        Nothing -> Right (constAcc, o : keepAcc)

buildOverrideValueMap :: [Text] -> [(Text, OverrideValue)] -> Map.Map Text OverrideValue
buildOverrideValueMap rootPath overrides =
  Map.fromList
    [ (candidate, val)
    | (key, val) <- overrides
    , candidate <- overrideKeyCandidates rootPath key
    ]

overrideKeyCandidates :: [Text] -> Text -> [Text]
overrideKeyCandidates rootPath key
  | "__wesl__" `T.isPrefixOf` key = [key]
  | otherwise =
      case splitQName key of
        [] -> []
        [single] -> [single]
        segs ->
          let path = init segs
              name = last segs
          in if path == rootPath || null rootPath
                then [name]
                else ["__wesl__" <> T.intercalate "__" path <> "__" <> name]

overrideValueToExpr :: [(Text, StructDecl)] -> Type -> OverrideValue -> Either CompileError Expr
overrideValueToExpr structEnv ty ov =
  case ty of
    TyScalar Bool ->
      case ov of
        OVBool b -> Right (EBool b)
        _ -> Left (CompileError "override value must be a bool" Nothing Nothing)
    TyScalar I32 ->
      case ov of
        OVI32 v -> Right (EInt v)
        _ -> Left (CompileError "override value must be an i32" Nothing Nothing)
    TyScalar U32 ->
      case ov of
        OVU32 v -> Right (ECall "u32" [EInt v])
        _ -> Left (CompileError "override value must be a u32" Nothing Nothing)
    TyScalar F32 ->
      case ov of
        OVF32 v -> Right (EFloat v)
        OVF16 v -> Right (ECall "f32" [EFloat v])
        _ -> Left (CompileError "override value must be an f32" Nothing Nothing)
    TyScalar F16 ->
      case ov of
        OVF16 v -> Right (ECall "f16" [EFloat v])
        OVF32 v -> Right (ECall "f16" [EFloat v])
        _ -> Left (CompileError "override value must be an f16" Nothing Nothing)
    TyVector n scalar ->
      case ov of
        OVComposite vals -> do
          when (length vals /= n) $
            Left (CompileError "override vector arity does not match" Nothing Nothing)
          args <- mapM (overrideValueToExpr structEnv (TyScalar scalar)) vals
          Right (ECall ("vec" <> T.pack (show n)) args)
        _ -> Left (CompileError "override value must be a vector" Nothing Nothing)
    TyMatrix cols rows scalar ->
      case ov of
        OVComposite colsVals -> do
          when (length colsVals /= cols) $
            Left (CompileError "override matrix column count does not match" Nothing Nothing)
          let colTy = TyVector rows scalar
          args <- mapM (overrideValueToExpr structEnv colTy) colsVals
          Right (ECall ("mat" <> T.pack (show cols) <> "x" <> T.pack (show rows)) args)
        _ -> Left (CompileError "override value must be a matrix" Nothing Nothing)
    TyArray elemTy (Just n) ->
      case ov of
        OVComposite vals -> do
          when (length vals /= n) $
            Left (CompileError "override array length does not match" Nothing Nothing)
          args <- mapM (overrideValueToExpr structEnv elemTy) vals
          Right (ECall "array" args)
        _ -> Left (CompileError "override value must be an array" Nothing Nothing)
    TyArray _ Nothing ->
      Left (CompileError "override values cannot target runtime-sized arrays" Nothing Nothing)
    TyStructRef name ->
      case lookup name structEnv of
        Nothing -> Left (CompileError ("unknown struct: " <> T.unpack name) Nothing Nothing)
        Just decl ->
          case ov of
            OVComposite vals -> do
              let fields = sdFields decl
              when (length vals /= length fields) $
                Left (CompileError "override struct field count does not match" Nothing Nothing)
              args <- zipWithM (\f v -> overrideValueToExpr structEnv (fdType f) v) fields vals
              Right (ECall name args)
            _ -> Left (CompileError "override value must be a struct composite" Nothing Nothing)
    _ -> Left (CompileError "override values are only supported for scalar, vector, matrix, array, and struct types" Nothing Nothing)

resolveTypeAliases :: ModuleAst -> Either CompileError ModuleAst
resolveTypeAliases ast = do
  let (dupAliases, _) = foldl' collect ([], Set.empty) (map adName (modAliases ast))
  when (not (null dupAliases)) $
    Left (CompileError ("duplicate type aliases: " <> T.unpack (T.intercalate ", " dupAliases)) Nothing Nothing)
  let aliasMap = Map.fromList [(adName a, adType a) | a <- modAliases ast]
  let expand = expandType aliasMap
  aliases <- mapM (\a -> (\ty -> a { adType = ty }) <$> expand (adType a)) (modAliases ast)
  structs <- mapM (expandStruct expand) (modStructs ast)
  bindings <- mapM (expandBinding expand) (modBindings ast)
  globals <- mapM (expandGlobal expand) (modGlobals ast)
  consts <- mapM (expandConst expand) (modConsts ast)
  overrides <- mapM (expandOverride expand) (modOverrides ast)
  constAsserts <- mapM (expandConstAssert expand) (modConstAsserts ast)
  functions <- mapM (expandFunction expand) (modFunctions ast)
  entry <- mapM (expandEntry expand) (modEntry ast)
  Right ast
    { modAliases = aliases
    , modStructs = structs
    , modBindings = bindings
    , modGlobals = globals
    , modConsts = consts
    , modOverrides = overrides
    , modConstAsserts = constAsserts
    , modFunctions = functions
    , modEntry = entry
    }
  where
    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)
    expandType aliasMap = go Set.empty
      where
        go stack ty =
          case ty of
            TyStructRef name ->
              case Map.lookup name aliasMap of
                Nothing -> Right ty
                Just aliasTy ->
                  if Set.member name stack
                    then Left (CompileError ("type alias cycle involving: " <> T.unpack name) Nothing Nothing)
                    else go (Set.insert name stack) aliasTy
            TyArray elemTy n -> TyArray <$> go stack elemTy <*> pure n
            _ -> Right ty

    expandStruct expand decl = do
      fields <- mapM (\f -> (\ty -> f { fdType = ty }) <$> expand (fdType f)) (sdFields decl)
      Right decl { sdFields = fields }

    expandBinding expand decl = do
      ty <- expand (bdType decl)
      Right decl { bdType = ty }

    expandGlobal expand decl = do
      ty <- expand (gvType decl)
      init' <- mapM (expandExpr expand) (gvInit decl)
      Right decl { gvType = ty, gvInit = init' }

    expandConstAssert expand (ConstAssert pos expr) = do
      expr' <- expandExpr expand expr
      Right (ConstAssert pos expr')

    expandConst expand decl = do
      expr <- expandExpr expand (cdExpr decl)
      Right decl { cdExpr = expr }

    expandOverride expand decl = do
      ty <- expand (odType decl)
      expr <- mapM (expandExpr expand) (odExpr decl)
      Right decl { odType = ty, odExpr = expr }

    expandFunction expand decl = do
      params <- mapM (\p -> (\ty -> p { paramType = ty }) <$> expand (paramType p)) (fnParams decl)
      ret <- mapM expand (fnReturnType decl)
      body <- mapM (expandStmt expand) (fnBody decl)
      Right decl { fnParams = params, fnReturnType = ret, fnBody = body }

    expandEntry expand decl = do
      params <- mapM (\p -> (\ty -> p { paramType = ty }) <$> expand (paramType p)) (epParams decl)
      ret <- mapM expand (epReturnType decl)
      body <- mapM (expandStmt expand) (epBody decl)
      Right decl { epParams = params, epReturnType = ret, epBody = body }

    expandStmt expand stmt =
      case stmt of
        SLet name expr -> SLet name <$> expandExpr expand expr
        SVar name expr -> SVar name <$> expandExpr expand expr
        SAssign lv expr -> SAssign <$> expandLValue expand lv <*> expandExpr expand expr
        SAssignOp lv op expr -> SAssignOp <$> expandLValue expand lv <*> pure op <*> expandExpr expand expr
        SInc lv -> SInc <$> expandLValue expand lv
        SDec lv -> SDec <$> expandLValue expand lv
        SExpr expr -> SExpr <$> expandExpr expand expr
        SIf cond thenBody elseBody ->
          SIf <$> expandExpr expand cond <*> mapM (expandStmt expand) thenBody <*> mapM (mapM (expandStmt expand)) elseBody
        SWhile cond body -> SWhile <$> expandExpr expand cond <*> mapM (expandStmt expand) body
        SLoop body cont -> SLoop <$> mapM (expandStmt expand) body <*> mapM (mapM (expandStmt expand)) cont
        SFor initStmt cond cont body ->
          SFor <$> mapM (expandStmt expand) initStmt <*> mapM (expandExpr expand) cond <*> mapM (expandStmt expand) cont <*> mapM (expandStmt expand) body
        SSwitch expr cases defBody ->
          SSwitch <$> expandExpr expand expr <*> mapM (expandCase expand) cases <*> mapM (mapM (expandStmt expand)) defBody
        SBreak -> Right SBreak
        SBreakIf expr -> SBreakIf <$> expandExpr expand expr
        SContinue -> Right SContinue
        SDiscard -> Right SDiscard
        SFallthrough -> Right SFallthrough
        SReturn expr -> SReturn <$> mapM (expandExpr expand) expr

    expandCase expand (SwitchCase sels body) =
      SwitchCase <$> mapM (expandExpr expand) sels <*> mapM (expandStmt expand) body

    expandLValue expand lv =
      case lv of
        LVVar name -> Right (LVVar name)
        LVField inner field -> LVField <$> expandLValue expand inner <*> pure field
        LVIndex inner idx -> LVIndex <$> expandLValue expand inner <*> expandExpr expand idx
        LVDeref expr -> LVDeref <$> expandExpr expand expr

    expandExpr expand expr =
      case expr of
        EBinary op a b -> EBinary op <$> expandExpr expand a <*> expandExpr expand b
        EUnary op a -> EUnary op <$> expandExpr expand a
        ECall name args -> ECall name <$> mapM (expandExpr expand) args
        EBitcast ty arg -> EBitcast <$> expand ty <*> expandExpr expand arg
        EField base field -> EField <$> expandExpr expand base <*> pure field
        EIndex base idx -> EIndex <$> expandExpr expand base <*> expandExpr expand idx
        _ -> Right expr

buildAliasMaps :: [ImportResolved] -> (Map.Map Text [Text], Map.Map Text [Text])
buildAliasMaps imports = foldl' add (Map.empty, Map.empty) imports
  where
    add (modAcc, itemAcc) imp =
      case irItem imp of
        Nothing ->
          let alias = fromMaybe (last (irModulePath imp)) (irAlias imp)
          in if T.null alias
              then (modAcc, itemAcc)
              else (Map.insert alias (irModulePath imp) modAcc, itemAcc)
        Just item ->
          let alias = fromMaybe item (irAlias imp)
              target = irModulePath imp <> [item]
          in if T.null alias
              then (modAcc, itemAcc)
              else (modAcc, Map.insert alias target itemAcc)

data Scope = Scope
  { scNameTable :: !NameTable
  , scGlobals :: IntSet.IntSet
  , scScopes :: [IntSet.IntSet]
  , scModuleAliases :: IntSet.IntSet
  , scItemAliases :: IntSet.IntSet
  , scTypeAliases :: IntSet.IntSet
  , scAllowShadowing :: Bool
  , scAllowFallthrough :: Bool
  }

validateDirectives :: CompileOptions -> [Directive] -> Either CompileError DiagnosticConfig
validateDirectives opts directives = do
  mapM_ checkEnable directives
  pure (foldl' applyDiag Map.empty directives)
  where
    enabled = Set.fromList (map T.pack (enabledFeatures opts))
    checkEnable dir =
      case dir of
        DirEnable feat ->
          if Set.member feat enabled
            then Right ()
            else Left (CompileError ("feature not enabled: " <> T.unpack feat) Nothing Nothing)
        _ -> Right ()
    applyDiag acc dir =
      case dir of
        DirDiagnostic severity rule -> Map.insert (T.unpack rule) severity acc
        _ -> acc

diagnosticSeverity :: DiagnosticConfig -> String -> DiagnosticSeverity
diagnosticSeverity cfg rule = fromMaybe DiagError (Map.lookup rule cfg)

validateModuleScopes :: CompileOptions -> Bool -> [Text] -> FilePath -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideIndex -> [ModuleNode] -> Either CompileError ()
validateModuleScopes opts skipConstAsserts rootPath rootDir constIndex fnIndex structIndex overrideIndex nodes =
  mapM_ (validateModuleScope opts skipConstAsserts rootPath rootDir constIndex fnIndex structIndex overrideIndex) nodes

validateModuleScope :: CompileOptions -> Bool -> [Text] -> FilePath -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideIndex -> ModuleNode -> Either CompileError ()
validateModuleScope opts skipConstAsserts rootPath rootDir constIndex fnIndex structIndex overrideIndex node = do
  let ctx = buildModuleContext rootPath rootDir node
  diagConfig <- validateDirectives opts (modDirectives (mnAst node))
  let allowShadowing = diagnosticSeverity diagConfig "shadowing" /= DiagError
  let (nt1, globalsIds) = internNameSet emptyNameTable (Set.toList (mcLocals ctx))
  let (nt2, moduleAliasIds) = internNameSet nt1 (Map.keys (mcModuleAliases ctx))
  let (nt3, itemAliasIds) = internNameSet nt2 (Map.keys (mcItemAliases ctx))
  let (nt4, typeAliasIds) = internNameSet nt3 (map adName (modAliases (mnAst node)))
  let scope0 =
        Scope
          { scNameTable = nt4
          , scGlobals = globalsIds
          , scScopes = [IntSet.empty]
          , scModuleAliases = moduleAliasIds
          , scItemAliases = itemAliasIds
          , scTypeAliases = typeAliasIds
          , scAllowShadowing = allowShadowing
          , scAllowFallthrough = False
          }
  validateModuleAst ctx constIndex fnIndex structIndex overrideIndex scope0 diagConfig skipConstAsserts (mnAst node)

type DiagnosticConfig = Map.Map String DiagnosticSeverity

validateModuleAst :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideIndex -> Scope -> DiagnosticConfig -> Bool -> ModuleAst -> Either CompileError ()
validateModuleAst ctx constIndex fnIndex structIndex overrideIndex scope diagConfig skipConstAsserts ast = do
  mapM_ (validateAlias ctx scope) (modAliases ast)
  mapM_ (validateStruct ctx scope) (modStructs ast)
  mapM_ (validateBinding ctx scope) (modBindings ast)
  mapM_ (validateGlobalVar ctx scope) (modGlobals ast)
  mapM_ (validateConst ctx scope) (modConsts ast)
  mapM_ (validateOverride ctx scope overrideIndex) (modOverrides ast)
  when (not skipConstAsserts) $
    mapM_ (validateConstAssert ctx constIndex fnIndex structIndex diagConfig) (modConstAsserts ast)
  mapM_ (validateFunction ctx constIndex fnIndex structIndex skipConstAsserts scope) (modFunctions ast)
  case modEntry ast of
    Nothing -> Right ()
    Just entry -> validateEntryPoint ctx constIndex fnIndex structIndex skipConstAsserts scope entry

validateConstAssertsMerged :: CompileOptions -> [Text] -> ModuleAst -> Either CompileError ()
validateConstAssertsMerged opts rootPath ast = do
  let node = ModuleNode "<merged>" rootPath ast []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let ctx = buildModuleContext rootPath "" node
  diagConfig <- validateDirectives opts (modDirectives ast)
  mapM_ (validateConstAssert ctx constIndex fnIndex structIndex diagConfig) (modConstAsserts ast)
  Right ()

collectDiagnosticsMerged :: CompileOptions -> [Text] -> ModuleAst -> Either CompileError [Diagnostic]
collectDiagnosticsMerged opts rootPath ast = do
  let node = ModuleNode "<merged>" rootPath ast []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let ctx = buildModuleContext rootPath "" node
  diagConfig <- validateDirectives opts (modDirectives ast)
  constDiags <- fmap concat (mapM (collectConstAssertDiagnostic ctx constIndex fnIndex structIndex diagConfig) (modConstAsserts ast))
  let unreachableDiags = collectUnreachableDiagnostics diagConfig ast
  let unusedExprDiags = collectUnusedExpressionDiagnostics diagConfig ast
  let unusedVarDiags = collectUnusedVariableDiagnostics diagConfig ast
  let unusedParamDiags = collectUnusedParameterDiagnostics diagConfig ast
  let shadowingDiags = collectShadowingDiagnostics diagConfig ast
  let constantCondDiags = collectConstantConditionDiagnostics diagConfig ctx constIndex fnIndex structIndex ast
  let duplicateCaseDiags = collectDuplicateCaseDiagnostics diagConfig ctx constIndex fnIndex structIndex ast
  Right
    ( constDiags
        <> unreachableDiags
        <> unusedExprDiags
        <> unusedVarDiags
        <> unusedParamDiags
        <> shadowingDiags
        <> constantCondDiags
        <> duplicateCaseDiags
    )

collectConstAssertDiagnostic :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> DiagnosticConfig -> ConstAssert -> Either CompileError [Diagnostic]
collectConstAssertDiagnostic ctx constIndex fnIndex structIndex diagConfig (ConstAssert pos expr) =
  case diagnosticSeverity diagConfig "const_assert" of
    DiagOff -> Right []
    DiagInfo -> emitIfFalse DiagInfo
    DiagWarning -> emitIfFalse DiagWarning
    DiagError -> do
      ok <- withPos pos (evalConstBoolExpr ctx constIndex fnIndex structIndex expr)
      if ok
        then Right []
        else Left (errorAtPos pos "const_assert condition failed")
  where
    emitIfFalse sev = do
      ok <- withPos pos (evalConstBoolExpr ctx constIndex fnIndex structIndex expr)
      if ok
        then Right []
        else Right [Diagnostic sev "const_assert" "const_assert condition failed" (Just (spLine pos)) (Just (spCol pos))]

collectUnreachableDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnreachableDiagnostics diagConfig ast =
  case Map.lookup "unreachable_code" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let diag = Diagnostic sev "unreachable_code" "unreachable code" Nothing Nothing
          bodies = map fnBody (modFunctions ast) <> maybe [] (pure . epBody) (modEntry ast)
      in concatMap (unreachableInStmts diag) bodies
  where
    unreachableInStmts diag = go False
      where
        go _ [] = []
        go unreachable (stmt:rest) =
          let here = if unreachable then [diag] else []
              nested =
                case stmt of
                  SIf _ thenBody elseBody ->
                    unreachableInStmts diag thenBody <> maybe [] (unreachableInStmts diag) elseBody
                  SWhile _ body ->
                    unreachableInStmts diag body
                  SLoop body continuing ->
                    unreachableInStmts diag body <> maybe [] (unreachableInStmts diag) continuing
                  SFor _ _ _ body ->
                    unreachableInStmts diag body
                  SSwitch _ cases defBody ->
                    concatMap (unreachableInStmts diag . scBody) cases <> maybe [] (unreachableInStmts diag) defBody
                  _ -> []
              unreachable' = unreachable || isTerminator stmt
          in here <> nested <> go unreachable' rest

    isTerminator stmt =
      case stmt of
        SReturn _ -> True
        SBreak -> True
        SContinue -> True
        SDiscard -> True
        _ -> False

collectUnusedExpressionDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnusedExpressionDiagnostics diagConfig ast =
  case Map.lookup "unused_expression" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let diag = Diagnostic sev "unused_expression" "expression has no effect" Nothing Nothing
          bodies = map fnBody (modFunctions ast) <> maybe [] (pure . epBody) (modEntry ast)
      in concatMap (unusedExprs diag) bodies
  where
    unusedExprs diag = go
      where
        go [] = []
        go (stmt:rest) =
          let here =
                case stmt of
                  SExpr expr ->
                    case expr of
                      ECall _ _ -> []
                      _ -> [diag]
                  _ -> []
              nested =
                case stmt of
                  SIf _ thenBody elseBody ->
                    go thenBody <> maybe [] go elseBody
                  SWhile _ body ->
                    go body
                  SLoop body continuing ->
                    go body <> maybe [] go continuing
                  SFor _ _ _ body ->
                    go body
                  SSwitch _ cases defBody ->
                    concatMap (go . scBody) cases <> maybe [] go defBody
                  _ -> []
          in here <> nested <> go rest

collectUnusedVariableDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnusedVariableDiagnostics diagConfig ast =
  case Map.lookup "unused_variable" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let diag name = Diagnostic sev "unused_variable" ("unused variable: " <> T.unpack name) Nothing Nothing
          bodies = map fnBody (modFunctions ast) <> maybe [] (pure . epBody) (modEntry ast)
      in concatMap (unusedVarsInBody diag) bodies
  where
    unusedVarsInBody mkDiag stmts =
      let declaredNames = collectDecls stmts
          (nt1, declaredPairs) = internNamePairs emptyNameTable declaredNames
          (_, usedIds) = internNameSet nt1 (collectUsesInStmts stmts)
          unused =
            [ name
            | (name, ident) <- uniqueById declaredPairs
            , not (IntSet.member ident usedIds)
            , not (isIgnored name)
            ]
      in map mkDiag unused

    isIgnored name = T.isPrefixOf "_" name

    collectDecls = concatMap declsInStmt
    declsInStmt stmt =
      case stmt of
        SLet name _ -> [name]
        SVar name _ -> [name]
        SIf _ t e -> collectDecls t <> maybe [] collectDecls e
        SWhile _ body -> collectDecls body
        SLoop body cont -> collectDecls body <> maybe [] collectDecls cont
        SFor initStmt _ contStmt body ->
          maybe [] declsInStmt initStmt <> maybe [] declsInStmt contStmt <> collectDecls body
        SSwitch _ cases defBody ->
          concatMap (collectDecls . scBody) cases <> maybe [] collectDecls defBody
        _ -> []

collectUnusedParameterDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnusedParameterDiagnostics diagConfig ast =
  case Map.lookup "unused_parameter" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag name = Diagnostic sev "unused_parameter" ("unused parameter: " <> T.unpack name) Nothing Nothing
      in concatMap (unusedParamsInFunction mkDiag) (modFunctions ast)
          <> maybe [] (unusedParamsInEntry mkDiag) (modEntry ast)
  where
    unusedParamsInFunction mkDiag fn =
      let params = map paramName (fnParams fn)
          (nt1, paramPairs) = internNamePairs emptyNameTable params
          (_, usedIds) = internNameSet nt1 (collectUsesInStmts (fnBody fn))
          unused =
            [ name
            | (name, ident) <- uniqueById paramPairs
            , not (IntSet.member ident usedIds)
            , not (isIgnored name)
            ]
      in map mkDiag unused
    unusedParamsInEntry mkDiag entry =
      let params = map paramName (epParams entry)
          (nt1, paramPairs) = internNamePairs emptyNameTable params
          (_, usedIds) = internNameSet nt1 (collectUsesInStmts (epBody entry))
          unused =
            [ name
            | (name, ident) <- uniqueById paramPairs
            , not (IntSet.member ident usedIds)
            , not (isIgnored name)
            ]
      in map mkDiag unused
    isIgnored name = T.isPrefixOf "_" name

collectShadowingDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectShadowingDiagnostics diagConfig ast =
  case Map.lookup "shadowing" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag name = Diagnostic sev "shadowing" ("name shadows outer scope: " <> T.unpack name) Nothing Nothing
      in concatMap (shadowingInFunction mkDiag) (modFunctions ast)
          <> maybe [] (shadowingInEntry mkDiag) (modEntry ast)
  where
    shadowingInFunction mkDiag fn =
      let params = map paramName (fnParams fn)
          (nt1, paramIds) = internNames emptyNameTable params
      in shadowingInStmts mkDiag nt1 [IntSet.empty, IntSet.fromList paramIds] (fnBody fn)
    shadowingInEntry mkDiag entry =
      let params = map paramName (epParams entry)
          (nt1, paramIds) = internNames emptyNameTable params
      in shadowingInStmts mkDiag nt1 [IntSet.empty, IntSet.fromList paramIds] (epBody entry)

    shadowingInStmts mkDiag table scopes0 stmts = let (diags, _, _) = go table scopes0 stmts in diags
      where
        go table0 scopesAcc [] = ([], table0, scopesAcc)
        go table0 scopesAcc (stmt:rest) =
          let (diags1, table1, scopes1) = shadowingInStmt mkDiag table0 scopesAcc stmt
              (diags2, table2, scopes2) = go table1 scopes1 rest
          in (diags1 <> diags2, table2, scopes2)

    shadowingInStmt mkDiag table scopes stmt =
      case stmt of
        SLet name _ ->
          let (ident, table') = internName name table
          in (diagIfShadow mkDiag scopes ident name, table', addToCurrent ident scopes)
        SVar name _ ->
          let (ident, table') = internName name table
          in (diagIfShadow mkDiag scopes ident name, table', addToCurrent ident scopes)
        SIf _ thenBody elseBody ->
          let (diags1, table1) = shadowingInBlock mkDiag table scopes thenBody
              (diags2, table2) = maybe ([], table1) (shadowingInBlock mkDiag table1 scopes) elseBody
          in (diags1 <> diags2, table2, scopes)
        SWhile _ body ->
          let (diags1, table1) = shadowingInBlock mkDiag table scopes body
          in (diags1, table1, scopes)
        SLoop body continuing ->
          let (diags1, table1) = shadowingInBlock mkDiag table scopes body
              (diags2, table2) = maybe ([], table1) (shadowingInBlock mkDiag table1 scopes) continuing
          in (diags1 <> diags2, table2, scopes)
        SFor initStmt _ contStmt body ->
          let loopScopes = pushScope scopes
              (diagsInit, table1, loopScopes1) =
                case initStmt of
                  Nothing -> ([], table, loopScopes)
                  Just s -> shadowingInStmt mkDiag table loopScopes s
              (diagsCont, table2, loopScopes2) =
                case contStmt of
                  Nothing -> ([], table1, loopScopes1)
                  Just s -> shadowingInStmt mkDiag table1 loopScopes1 s
              (diagsBody, table3) = shadowingInBlock mkDiag table2 loopScopes2 body
          in (diagsInit <> diagsCont <> diagsBody, table3, scopes)
        SSwitch _ cases defBody ->
          let (diagsCases, table1) = shadowingInCases mkDiag table scopes cases
              (diagsDef, table2) = maybe ([], table1) (shadowingInBlock mkDiag table1 scopes) defBody
          in (diagsCases <> diagsDef, table2, scopes)
        _ -> ([], table, scopes)

    shadowingInBlock mkDiag table scopes body = shadowingInStmtsWithTable mkDiag table (pushScope scopes) body

    shadowingInStmtsWithTable mkDiag table scopes stmts = go table scopes stmts
      where
        go table0 _ [] = ([], table0)
        go table0 scopes0 (stmt:rest) =
          let (diags1, table1, scopes1) = shadowingInStmt mkDiag table0 scopes0 stmt
              (diags2, table2) = go table1 scopes1 rest
          in (diags1 <> diags2, table2)

    shadowingInCases mkDiag table scopes cases =
      foldl' step ([], table) cases
      where
        step (diagsAcc, tableAcc) sc =
          let (diags, table') = shadowingInBlock mkDiag tableAcc scopes (scBody sc)
          in (diagsAcc <> diags, table')

    diagIfShadow mkDiag scopes ident name
      | isIgnored name = []
      | otherwise =
          let outers = drop 1 scopes
          in if any (IntSet.member ident) outers then [mkDiag name] else []

    addToCurrent ident scopes =
      case scopes of
        [] -> [IntSet.singleton ident]
        current : rest -> IntSet.insert ident current : rest

    pushScope scopes = IntSet.empty : scopes

    isIgnored name = T.isPrefixOf "_" name

collectConstantConditionDiagnostics :: DiagnosticConfig -> ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ModuleAst -> [Diagnostic]
collectConstantConditionDiagnostics diagConfig ctx constIndex fnIndex structIndex ast =
  case Map.lookup "constant_condition" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag msg = Diagnostic sev "constant_condition" msg Nothing Nothing
      in concatMap (constantCondInStmts mkDiag) (map fnBody (modFunctions ast))
          <> maybe [] (constantCondInStmts mkDiag . epBody) (modEntry ast)
  where
    constantCondInStmts mkDiag stmts = concatMap (constantCondInStmt mkDiag) stmts

    constantCondInStmt mkDiag stmt =
      case stmt of
        SIf cond thenBody elseBody ->
          checkCond mkDiag "if" cond
            <> constantCondInStmts mkDiag thenBody
            <> maybe [] (constantCondInStmts mkDiag) elseBody
        SWhile cond body ->
          checkCond mkDiag "while" cond <> constantCondInStmts mkDiag body
        SLoop body continuing ->
          constantCondInStmts mkDiag body <> maybe [] (constantCondInStmts mkDiag) continuing
        SFor initStmt condExpr contStmt body ->
          maybe [] (constantCondInStmt mkDiag) initStmt
            <> maybe [] (constantCondInStmt mkDiag) contStmt
            <> maybe [] (checkCond mkDiag "for" ) condExpr
            <> constantCondInStmts mkDiag body
        SSwitch _ cases defBody ->
          concatMap (constantCondInStmts mkDiag . scBody) cases <> maybe [] (constantCondInStmts mkDiag) defBody
        SBreakIf cond -> checkCond mkDiag "break if" cond
        _ -> []

    checkCond mkDiag label cond =
      case evalConstBoolExpr ctx constIndex fnIndex structIndex cond of
        Right _ -> [mkDiag ("constant condition in " <> label)]
        Left _ -> []

collectDuplicateCaseDiagnostics :: DiagnosticConfig -> ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ModuleAst -> [Diagnostic]
collectDuplicateCaseDiagnostics diagConfig ctx constIndex fnIndex structIndex ast =
  case Map.lookup "duplicate_case" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag val = Diagnostic sev "duplicate_case" ("duplicate switch case selector: " <> val) Nothing Nothing
      in concatMap (duplicateInStmts mkDiag) (map fnBody (modFunctions ast))
          <> maybe [] (duplicateInStmts mkDiag . epBody) (modEntry ast)
  where
    duplicateInStmts mkDiag stmts = concatMap (duplicateInStmt mkDiag) stmts

    duplicateInStmt mkDiag stmt =
      case stmt of
        SIf _ thenBody elseBody ->
          duplicateInStmts mkDiag thenBody <> maybe [] (duplicateInStmts mkDiag) elseBody
        SWhile _ body -> duplicateInStmts mkDiag body
        SLoop body continuing ->
          duplicateInStmts mkDiag body <> maybe [] (duplicateInStmts mkDiag) continuing
        SFor initStmt _ contStmt body ->
          maybe [] (duplicateInStmt mkDiag) initStmt
            <> maybe [] (duplicateInStmt mkDiag) contStmt
            <> duplicateInStmts mkDiag body
        SSwitch _ cases defBody ->
          let selectors = concatMap scSelectors cases
              values = mapMaybe (constSelectorValue ctx constIndex fnIndex structIndex) selectors
              keys = map renderKey values
              dupes = duplicateValues keys
              diags = map mkDiag dupes
              nested = concatMap (duplicateInStmts mkDiag . scBody) cases <> maybe [] (duplicateInStmts mkDiag) defBody
          in diags <> nested
        _ -> []

    constSelectorValue ctx' constIndex' fnIndex' structIndex' expr =
      case evalConstIntExpr ctx' constIndex' fnIndex' structIndex' expr of
        Right (ConstInt scalar val) -> Just (scalar, val)
        Left _ -> Nothing

    duplicateValues vals =
      let (dups, _) =
            foldl'
              (\(acc, seen) v ->
                 if Set.member v seen
                   then (Set.insert v acc, seen)
                   else (acc, Set.insert v seen))
              (Set.empty, Set.empty)
              vals
      in Set.toList dups

    renderKey (scalar, val) =
      show val <> case scalar of
        U32 -> "u"
        I32 -> "i"
        _ -> ""

collectUsesInStmts :: [Stmt] -> [Text]
collectUsesInStmts = concatMap collectUsesInStmt

collectUsesInStmt :: Stmt -> [Text]
collectUsesInStmt stmt =
  case stmt of
    SLet _ expr -> collectUsesInExpr expr
    SVar _ expr -> collectUsesInExpr expr
    SAssign lv expr -> collectUsesInLValue lv <> collectUsesInExpr expr
    SAssignOp lv _ expr -> collectUsesInLValue lv <> collectUsesInExpr expr
    SInc lv -> collectUsesInLValue lv
    SDec lv -> collectUsesInLValue lv
    SExpr expr -> collectUsesInExpr expr
    SIf cond t e -> collectUsesInExpr cond <> collectUsesInStmts t <> maybe [] collectUsesInStmts e
    SWhile cond body -> collectUsesInExpr cond <> collectUsesInStmts body
    SLoop body cont -> collectUsesInStmts body <> maybe [] collectUsesInStmts cont
    SFor initStmt condExpr contStmt body ->
      maybe [] collectUsesInStmt initStmt
        <> maybe [] collectUsesInExpr condExpr
        <> maybe [] collectUsesInStmt contStmt
        <> collectUsesInStmts body
    SSwitch expr cases defBody ->
      collectUsesInExpr expr <> concatMap collectUsesInCase cases <> maybe [] collectUsesInStmts defBody
    SBreakIf expr -> collectUsesInExpr expr
    SReturn mexpr -> maybe [] collectUsesInExpr mexpr
    _ -> []

collectUsesInCase :: SwitchCase -> [Text]
collectUsesInCase sc =
  concatMap collectUsesInExpr (scSelectors sc) <> collectUsesInStmts (scBody sc)

collectUsesInExpr :: Expr -> [Text]
collectUsesInExpr expr =
  case expr of
    EVar name -> [name]
    EInt _ -> []
    EFloat _ -> []
    EBool _ -> []
    EBinary _ a b -> collectUsesInExpr a <> collectUsesInExpr b
    EUnary _ a -> collectUsesInExpr a
    ECall _ args -> concatMap collectUsesInExpr args
    EBitcast _ arg -> collectUsesInExpr arg
    EField base _ -> collectUsesInExpr base
    EIndex base idx -> collectUsesInExpr base <> collectUsesInExpr idx

collectUsesInLValue :: LValue -> [Text]
collectUsesInLValue lv =
  case lv of
    LVVar name -> [name]
    LVField base _ -> collectUsesInLValue base
    LVIndex base idx -> collectUsesInLValue base <> collectUsesInExpr idx
    LVDeref expr -> collectUsesInExpr expr

validateStruct :: ModuleContext -> Scope -> StructDecl -> Either CompileError ()
validateStruct ctx scope decl =
  mapM_ (validateType ctx scope . fdType) (sdFields decl)

validateBinding :: ModuleContext -> Scope -> BindingDecl -> Either CompileError ()
validateBinding ctx scope decl = validateType ctx scope (bdType decl)

validateGlobalVar :: ModuleContext -> Scope -> GlobalVarDecl -> Either CompileError ()
validateGlobalVar ctx scope decl = do
  validateType ctx scope (gvType decl)
  mapM_ (validateExpr ctx scope) (gvInit decl)
  case gvType decl of
    TyPtr {} -> Left (CompileError "global pointer types are not supported" Nothing Nothing)
    _ -> Right ()
  case gvSpace decl of
    "private" -> Right ()
    "workgroup" ->
      case gvInit decl of
        Nothing -> Right ()
        Just _ -> Left (CompileError "workgroup variables cannot have initializers" Nothing Nothing)
    _ -> Left (CompileError ("unsupported global address space: " <> textToString (gvSpace decl)) Nothing Nothing)

validateConst :: ModuleContext -> Scope -> ConstDecl -> Either CompileError ()
validateConst ctx scope decl = validateExpr ctx scope (cdExpr decl) >> Right ()

validateAlias :: ModuleContext -> Scope -> AliasDecl -> Either CompileError ()
validateAlias ctx scope decl = validateType ctx scope (adType decl)

validateOverride :: ModuleContext -> Scope -> OverrideIndex -> OverrideDecl -> Either CompileError ()
validateOverride ctx scope _overrideIndex decl = do
  validateType ctx scope (odType decl)
  case odExpr decl of
    Nothing -> Right ()
    Just expr -> do
      validateExpr ctx scope expr
      Right ()
  where
    _ = _overrideIndex

validateConstAssert :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> DiagnosticConfig -> ConstAssert -> Either CompileError ()
validateConstAssert ctx constIndex fnIndex structIndex diagConfig (ConstAssert pos expr) =
  case diagnosticSeverity diagConfig "const_assert" of
    DiagOff -> Right ()
    DiagInfo -> Right ()
    DiagWarning -> Right ()
    DiagError -> do
      ok <- withPos pos (evalConstBoolExpr ctx constIndex fnIndex structIndex expr)
      if ok
        then Right ()
        else Left (errorAtPos pos "const_assert condition failed")

validateFunction :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> FunctionDecl -> Either CompileError ()
validateFunction ctx constIndex fnIndex structIndex skipConstEval scope fn = do
  mapM_ (validateType ctx scope . paramType) (fnParams fn)
  mapM_ (validateType ctx scope) (maybeToList (fnReturnType fn))
  let paramNames = map paramName (fnParams fn)
  ensureNoDuplicates "function parameters" paramNames
  let scope1 = scopeWithParams scope paramNames
  validateStmtList ctx constIndex fnIndex structIndex skipConstEval scope1 (fnBody fn)

validateEntryPoint :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> EntryPoint -> Either CompileError ()
validateEntryPoint ctx constIndex fnIndex structIndex skipConstEval scope entry = do
  mapM_ (validateType ctx scope . paramType) (epParams entry)
  mapM_ (validateType ctx scope) (maybeToList (epReturnType entry))
  let paramNames = map paramName (epParams entry)
  ensureNoDuplicates "entry point parameters" paramNames
  let scope1 = scopeWithParams scope paramNames
  validateStmtList ctx constIndex fnIndex structIndex skipConstEval scope1 (epBody entry)

validateStmtList :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> [Stmt] -> Either CompileError ()
validateStmtList ctx constIndex fnIndex structIndex skipConstEval scope0 = go scope0
  where
    go _ [] = Right ()
    go sc (stmt:rest) = do
      sc' <- validateStmt ctx constIndex fnIndex structIndex skipConstEval sc stmt
      go sc' rest

validateStmt :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> Stmt -> Either CompileError Scope
validateStmt ctx constIndex fnIndex structIndex skipConstEval scope stmt =
  case stmt of
    SLet name expr -> do
      validateExpr ctx scope expr
      scopeAdd scope name
    SVar name expr -> do
      validateExpr ctx scope expr
      scopeAdd scope name
    SAssign lv expr -> validateLValue ctx scope lv >> validateExpr ctx scope expr >> Right scope
    SAssignOp lv _ expr -> validateLValue ctx scope lv >> validateExpr ctx scope expr >> Right scope
    SInc lv -> validateLValue ctx scope lv >> Right scope
    SDec lv -> validateLValue ctx scope lv >> Right scope
    SExpr expr -> validateExpr ctx scope expr >> Right scope
    SIf cond thenBody elseBody -> do
      validateExpr ctx scope cond
      validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope) thenBody
      mapM_ (validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope)) elseBody
      Right scope
    SWhile cond body -> do
      validateExpr ctx scope cond
      validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope) body
      Right scope
    SLoop body continuing -> do
      validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope) body
      mapM_ (validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope)) continuing
      Right scope
    SFor initStmt condExpr contStmt body -> do
      scope1 <- case initStmt of
        Nothing -> Right scope
        Just s -> validateStmt ctx constIndex fnIndex structIndex skipConstEval scope s
      mapM_ (validateExpr ctx scope1) condExpr
      mapM_ (validateStmt ctx constIndex fnIndex structIndex skipConstEval scope1) contStmt
      validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope1) body
      Right scope
    SSwitch expr cases defBody -> do
      validateExpr ctx scope expr
      mapM_ (validateSwitchCase ctx constIndex fnIndex structIndex skipConstEval scope) cases
      mapM_ (validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope)) defBody
      Right scope
    SBreak -> Right scope
    SBreakIf cond -> validateExpr ctx scope cond >> Right scope
    SContinue -> Right scope
    SDiscard -> Right scope
    SFallthrough ->
      if scAllowFallthrough scope
        then Right scope
        else Left (CompileError "fallthrough is only allowed in switch cases" Nothing Nothing)
    SReturn mexpr -> mapM_ (validateExpr ctx scope) mexpr >> Right scope

validateSwitchCase :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> SwitchCase -> Either CompileError ()
validateSwitchCase ctx constIndex fnIndex structIndex skipConstEval scope sc = do
  _ <- analyzeFallthroughPlacement (scBody sc)
  when (not skipConstEval) $
    mapM_ (evalConstIntExpr ctx constIndex fnIndex structIndex) (scSelectors sc)
  let scope' = scope { scAllowFallthrough = True }
  validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope') (scBody sc)

analyzeFallthroughPlacement :: [Stmt] -> Either CompileError (Bool, [Stmt])
analyzeFallthroughPlacement body =
  case body of
    [] -> Right (False, [])
    _ ->
      let initStmts = init body
          lastStmt = last body
          nestedFallthrough = any stmtHasFallthrough initStmts
      in case lastStmt of
          SFallthrough ->
            if nestedFallthrough
              then Left (CompileError "fallthrough must be the last statement in a switch case" Nothing Nothing)
              else Right (True, initStmts)
          _ ->
            if nestedFallthrough || stmtHasFallthrough lastStmt
              then Left (CompileError "fallthrough must be the last statement in a switch case" Nothing Nothing)
              else Right (False, body)

stmtHasFallthrough :: Stmt -> Bool
stmtHasFallthrough stmt =
  case stmt of
    SFallthrough -> True
    SIf _ thenBody elseBody ->
      any stmtHasFallthrough thenBody || maybe False (any stmtHasFallthrough) elseBody
    SWhile _ body -> any stmtHasFallthrough body
    SLoop body continuing ->
      any stmtHasFallthrough body || maybe False (any stmtHasFallthrough) continuing
    SFor initStmt _ condStmt body ->
      maybe False stmtHasFallthrough initStmt
        || maybe False stmtHasFallthrough condStmt
        || any stmtHasFallthrough body
    SSwitch _ _ _ -> False
    _ -> False

expandSwitchCases :: [SwitchCase] -> Maybe [Stmt] -> Either CompileError [SwitchCase]
expandSwitchCases cases defBody = do
  infos <- mapM extract cases
  go [] defBody (reverse infos)
  where
    extract (SwitchCase selectors body) = do
      (fallthrough, stripped) <- analyzeFallthroughPlacement body
      Right (selectors, stripped, fallthrough)

    go acc _ [] = Right acc
    go acc nextChain ((selectors, body, fallthrough):rest) =
      case fallthrough of
        True ->
          case nextChain of
            Nothing ->
              Left (CompileError "fallthrough requires a following case or default" Nothing Nothing)
            Just chain ->
              let combined = body <> chain
              in go (SwitchCase selectors combined : acc) (Just combined) rest
        False ->
          let combined = body
          in go (SwitchCase selectors combined : acc) (Just combined) rest

data ConstInt = ConstInt
  { ciScalar :: Scalar
  , ciValue :: Integer
  } deriving (Eq, Show)

data ConstFloat = ConstFloat
  { cfScalar :: Scalar
  , cfValue :: Double
  } deriving (Eq, Show)

data ConstValue
  = CVInt ConstInt
  | CVBool Bool
  | CVFloat ConstFloat
  | CVVector Int Scalar [ConstValue]
  | CVMatrix Int Int Scalar [ConstValue]
  | CVArray Type [ConstValue]
  | CVStruct Text [(Text, ConstValue)]
  | CVPointer Type LValue
  deriving (Eq, Show)

data ConstBinding = ConstBinding
  { cbValue :: ConstValue
  , cbMutable :: Bool
  } deriving (Eq, Show)

type ConstEnv = Map.Map Text ConstBinding

constValueType :: ConstValue -> Type
constValueType val =
  case val of
    CVInt (ConstInt scalar _) -> TyScalar scalar
    CVFloat (ConstFloat scalar _) -> TyScalar scalar
    CVBool _ -> TyScalar Bool
    CVVector n scalar _ -> TyVector n scalar
    CVMatrix cols rows scalar _ -> TyMatrix cols rows scalar
    CVArray elemTy vals -> TyArray elemTy (Just (length vals))
    CVStruct name _ -> TyStructRef name
    CVPointer ty _ -> ty

constScalarType :: ConstValue -> Either CompileError Scalar
constScalarType val =
  case val of
    CVInt (ConstInt scalar _) -> Right scalar
    CVFloat (ConstFloat scalar _) -> Right scalar
    CVBool _ -> Right Bool
    _ -> Left (CompileError "expected scalar constant" Nothing Nothing)

constValueToInt :: ConstValue -> Either CompileError ConstInt
constValueToInt val =
  case val of
    CVInt v -> Right v
    _ -> Left (CompileError "expected integer constant" Nothing Nothing)

constValueToFloat :: ConstValue -> Either CompileError ConstFloat
constValueToFloat val =
  case val of
    CVFloat v -> Right v
    CVInt (ConstInt _ v) -> Right (ConstFloat F32 (fromIntegral v))
    _ -> Left (CompileError "expected float constant" Nothing Nothing)

constValueToBool :: ConstValue -> Either CompileError Bool
constValueToBool val =
  case val of
    CVBool b -> Right b
    _ -> Left (CompileError "expected bool constant" Nothing Nothing)

coerceConstScalarValue :: Scalar -> ConstValue -> Either CompileError ConstValue
coerceConstScalarValue target val =
  case target of
    Bool ->
      case val of
        CVBool b -> Right (CVBool b)
        _ -> Left (CompileError "expected bool constant" Nothing Nothing)
    I32 ->
      case val of
        CVInt ci -> CVInt <$> coerceConstIntToScalar I32 ci
        CVFloat (ConstFloat _ v) -> do
          let n = truncate v :: Integer
          when (n < 0 || n > 0x7FFFFFFF) $
            Left (CompileError "constant i32 is out of range" Nothing Nothing)
          Right (CVInt (ConstInt I32 n))
        _ -> Left (CompileError "expected integer constant" Nothing Nothing)
    U32 ->
      case val of
        CVInt ci -> CVInt <$> coerceConstIntToScalar U32 ci
        CVFloat (ConstFloat _ v) -> do
          let n = truncate v :: Integer
          when (n < 0 || n > fromIntegral (maxBound :: Word32)) $
            Left (CompileError "constant u32 is out of range" Nothing Nothing)
          Right (CVInt (ConstInt U32 n))
        _ -> Left (CompileError "expected integer constant" Nothing Nothing)
    F32 ->
      case val of
        CVFloat cf -> Right (CVFloat (convertConstFloatTo F32 cf))
        CVInt (ConstInt _ v) -> Right (CVFloat (ConstFloat F32 (fromIntegral v)))
        _ -> Left (CompileError "expected float constant" Nothing Nothing)
    F16 ->
      case val of
        CVFloat cf -> Right (CVFloat (convertConstFloatTo F16 cf))
        CVInt (ConstInt _ v) -> Right (CVFloat (convertConstFloatTo F16 (ConstFloat F32 (fromIntegral v))))
        _ -> Left (CompileError "expected float constant" Nothing Nothing)

coerceConstValueToType :: ModuleContext -> StructIndex -> Type -> ConstValue -> Either CompileError ConstValue
coerceConstValueToType ctx structIndex target val =
  case target of
    TyScalar scalar -> coerceConstScalarValue scalar val
    TyVector n scalar ->
      case val of
        CVVector m _ comps | m == n -> do
          comps' <- mapM (coerceConstScalarValue scalar) comps
          Right (CVVector n scalar comps')
        _ -> Left (CompileError "vector constant does not match type" Nothing Nothing)
    TyMatrix cols rows scalar ->
      case val of
        CVMatrix c r _ colsVals | c == cols && r == rows -> do
          cols' <- mapM (coerceConstValueToType ctx structIndex (TyVector rows scalar)) colsVals
          Right (CVMatrix cols rows scalar cols')
        _ -> Left (CompileError "matrix constant does not match type" Nothing Nothing)
    TyArray elemTy (Just len) ->
      case val of
        CVArray _ elems | length elems == len -> do
          elems' <- mapM (coerceConstValueToType ctx structIndex elemTy) elems
          Right (CVArray elemTy elems')
        _ -> Left (CompileError "array constant does not match type" Nothing Nothing)
    TyArray _ Nothing ->
      Left (CompileError "runtime array constants are not supported" Nothing Nothing)
    TyStructRef name ->
      case val of
        CVStruct structName pairs | structName == name -> do
          decl <- resolveStructDecl ctx structIndex name
          fields' <- mapM (coerceField pairs) (sdFields decl)
          Right (CVStruct name fields')
        _ -> Left (CompileError "struct constant does not match type" Nothing Nothing)
    TyPtr space access elemTy ->
      case val of
        CVPointer ptrTy lv ->
          case ptrTy of
            TyPtr space' access' elemTy' ->
              if space == space' && elemTy == elemTy' && ptrAccessCompatible access access'
                then Right (CVPointer ptrTy lv)
                else Left (CompileError "pointer constant does not match type" Nothing Nothing)
            _ -> Left (CompileError "pointer constant does not match type" Nothing Nothing)
        _ -> Left (CompileError "pointer constant does not match type" Nothing Nothing)
    _ -> Left (CompileError "unsupported const type" Nothing Nothing)
  where
    coerceField pairs field = do
      val' <- case lookup (fdName field) pairs of
        Just v -> Right v
        Nothing -> Left (CompileError ("missing field: " <> textToString (fdName field)) Nothing Nothing)
      coerced <- coerceConstValueToType ctx structIndex (fdType field) val'
      Right (fdName field, coerced)

resolveStructDecl :: ModuleContext -> StructIndex -> Text -> Either CompileError StructDecl
resolveStructDecl ctx structIndex name =
  case splitQName name of
    [] -> Left (CompileError "invalid struct reference" Nothing Nothing)
    [single] ->
      case lookupStruct (mcPath ctx) single of
        Just decl -> Right decl
        Nothing ->
          case Map.lookup single (mcItemAliases ctx) of
            Just target ->
              case splitLast target of
                Nothing -> Left (CompileError "invalid struct reference" Nothing Nothing)
                Just (path, item) ->
                  case lookupStruct path item of
                    Just decl -> Right decl
                    Nothing -> Left (CompileError ("unknown struct: " <> textToString item) Nothing Nothing)
            Nothing -> Left (CompileError ("unknown struct: " <> textToString single) Nothing Nothing)
    seg0 : segRest ->
      case Map.lookup seg0 (mcModuleAliases ctx) of
        Nothing -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> Left (CompileError "invalid struct reference" Nothing Nothing)
            Just (path, item) ->
              case lookupStruct path item of
                Just decl -> Right decl
                Nothing -> Left (CompileError ("unknown struct: " <> textToString item) Nothing Nothing)
  where
    lookupStruct path item = lookupStructIndex structIndex path item

evalConstValueWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError ConstValue
evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr = go seenConsts seenFns expr
  where
    go seen fnSeen ex =
      case ex of
        EUnary OpAddr inner -> evalConstAddressOf seen fnSeen inner
        EUnary OpDeref inner -> do
          ptr <- go seen fnSeen inner
          derefConstPointer seen fnSeen ptr
        EVar name ->
          case Map.lookup name env of
            Just binding -> Right (cbValue binding)
            Nothing -> do
              (path, ident) <- resolveConstRef ctx name
              let key = T.intercalate "::" (path <> [ident])
              when (Set.member key seen) $
                Left (CompileError "cycle detected while evaluating constant expression" Nothing Nothing)
              let entry = lookupConstIndex constIndex path ident
              case entry of
                Nothing -> Left (CompileError ("unknown constant: " <> textToString ident) Nothing Nothing)
                Just expr' -> evalConstValueWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
        EField base field -> do
          baseVal <- go seen fnSeen base
          evalConstFieldAccess baseVal field
        EIndex base idxExpr -> do
          baseVal <- go seen fnSeen base
          idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen idxExpr
          evalConstIndexAccess baseVal idxVal
        ECall name args ->
          case name of
            "vec2" -> evalConstVectorCtor 2 seen fnSeen args
            "vec3" -> evalConstVectorCtor 3 seen fnSeen args
            "vec4" -> evalConstVectorCtor 4 seen fnSeen args
            "array" -> evalConstArrayCtor seen fnSeen args
            _ | Just (cols, rows) <- parseMatrixName name ->
                  evalConstMatrixCtor cols rows seen fnSeen args
            _ ->
              if isBuiltinName name
                then evalConstScalarValue seen fnSeen ex
                else
                  case resolveStructDecl ctx structIndex name of
                    Right decl -> evalConstStructCtor (sdName decl) (sdFields decl) seen fnSeen args
                    Left _ ->
                      evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
        _ -> evalConstScalarValue seen fnSeen ex

    evalConstScalarValue seen fnSeen ex =
      case evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex of
        Right v -> Right (CVInt v)
        Left errI ->
          case evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex of
            Right v -> Right (CVFloat v)
            Left errF ->
              case evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex of
                Right v -> Right (CVBool v)
                Left _ -> Left (firstError errI errF)

    firstError errI _ = errI

    evalConstVectorCtor n seen fnSeen args = do
      when (length args /= n) $
        Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      case vals of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        (firstVal : _) -> do
          scalar <- constScalarType firstVal
          coerced <- mapM (coerceConstScalarValue scalar) vals
          Right (CVVector n scalar coerced)

    evalConstMatrixCtor cols rows seen fnSeen args = do
      when (null args) $
        Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      let scalarCount = cols * rows
      if length vals == scalarCount && all isScalarConst vals
        then
          case vals of
            [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
            (v:_) -> do
              scalar <- constScalarType v
              scalars <- mapM (coerceConstScalarValue scalar) vals
              let colsVals = map (CVVector rows scalar) (chunk rows scalars)
              Right (CVMatrix cols rows scalar colsVals)
        else do
          when (length vals /= cols) $
            Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
          case vals of
            (firstCol : _) -> do
              scalar <- case firstCol of
                CVVector n scalar _ | n == rows -> Right scalar
                _ -> Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
              mapM_ (ensureColumn scalar) vals
              Right (CVMatrix cols rows scalar vals)
            [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
      where
        isScalarConst v =
          case v of
            CVInt _ -> True
            CVFloat _ -> True
            CVBool _ -> True
            _ -> False
        ensureColumn scalar val =
          case val of
            CVVector n s _ | n == rows && s == scalar -> Right ()
            CVVector _ _ _ -> Left (CompileError "matrix constructor expects column vectors" Nothing Nothing)
            _ -> Left (CompileError "matrix constructor expects column vectors" Nothing Nothing)
        chunk n xs =
          case splitAt n xs of
            (col, []) | null col -> []
            (col, rest) -> col : chunk n rest

    evalConstArrayCtor seen fnSeen args = do
      when (null args) $
        Left (CompileError "array constructor needs arguments" Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      case vals of
        [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
        (v:vs) ->
          case v of
            CVInt (ConstInt scalar _) -> do
              coerced <- mapM (coerceConstScalarValue scalar) (v:vs)
              Right (CVArray (TyScalar scalar) coerced)
            CVFloat (ConstFloat scalar _) -> do
              coerced <- mapM (coerceConstScalarValue scalar) (v:vs)
              Right (CVArray (TyScalar scalar) coerced)
            CVBool _ -> do
              coerced <- mapM (coerceConstScalarValue Bool) (v:vs)
              Right (CVArray (TyScalar Bool) coerced)
            _ -> do
              let elemTy = constValueType v
              mapM_ (ensureSameType elemTy) vs
              Right (CVArray elemTy (v:vs))
      where
        ensureSameType ty val =
          if constValueType val == ty
            then Right ()
            else Left (CompileError "array constructor argument type mismatch" Nothing Nothing)

    evalConstStructCtor name fields seen fnSeen args = do
      when (length args /= length fields) $
        Left (CompileError ("struct constructor arity mismatch for " <> textToString name) Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      let pairs = zip (map fdName fields) vals
      Right (CVStruct name pairs)

    evalConstAddressOf seen fnSeen inner =
      case exprToLValue inner of
        Nothing -> Left (CompileError "address-of requires an lvalue" Nothing Nothing)
        Just lv ->
          case lv of
            LVDeref ptrExpr -> do
              ptr <- go seen fnSeen ptrExpr
              case ptr of
                CVPointer _ _ -> Right ptr
                _ -> Left (CompileError "address-of expects a pointer operand" Nothing Nothing)
            _ -> do
              frozen <- freezeConstLValue seen fnSeen lv
              val <- evalConstLValueGet ctx constIndex fnIndex structIndex env seen fnSeen frozen
              let ptrTy = TyPtr "function" Nothing (constValueType val)
              Right (CVPointer ptrTy frozen)

    derefConstPointer seen fnSeen val =
      case val of
        CVPointer _ lv -> evalConstLValueGet ctx constIndex fnIndex structIndex env seen fnSeen lv
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)

    freezeConstLValue seen fnSeen lv =
      case lv of
        LVVar name -> Right (LVVar name)
        LVField base field -> LVField <$> freezeConstLValue seen fnSeen base <*> pure field
        LVIndex base idxExpr -> do
          base' <- freezeConstLValue seen fnSeen base
          ConstInt _ idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen idxExpr
          Right (LVIndex base' (EInt idxVal))
        LVDeref _ ->
          Left (CompileError "address-of requires an lvalue" Nothing Nothing)

evalConstFunctionValueWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> FunctionDecl -> Either CompileError ConstValue
evalConstFunctionValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns decl = do
  (_, ctrl, _) <- evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns constEvalMaxSteps (fnBody decl)
  val <- case ctrl of
    CCReturn v -> Right v
    CCNone -> Left (CompileError "const function must return a value" Nothing Nothing)
    CCBreak -> Left (CompileError "break used outside of a loop" Nothing Nothing)
    CCContinue -> Left (CompileError "continue used outside of a loop" Nothing Nothing)
  case fnReturnType decl of
    Just ty -> coerceConstValueToType ctx structIndex ty val
    Nothing -> Left (CompileError "const function must declare a return type" Nothing Nothing)

evalConstUserFunctionCall :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Text -> [Expr] -> Either CompileError ConstValue
evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seenConsts seenFns name args = do
  (path, ident) <- resolveFunctionRef ctx name
  let key = T.intercalate "::" (path <> [ident])
  when (Set.member key seenFns) $
    Left (CompileError "cycle detected while evaluating const function" Nothing Nothing)
  decls <- case lookupFunctionIndex fnIndex path ident of
    Nothing -> Left (CompileError ("unknown function: " <> textToString ident) Nothing Nothing)
    Just ds -> Right ds
  argVals <- mapM (evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns) args
  let seenFns' = Set.insert key seenFns
  let attempt decl = do
        let params = fnParams decl
        when (length params /= length argVals) $
          Left (CompileError "const function overload not found" Nothing Nothing)
        coercedArgs <- zipWithM (coerceConstValueToType ctx structIndex . paramType) params argVals
        let bindings = [ConstBinding val False | val <- coercedArgs]
        let env' = Map.fromList (zip (map paramName params) bindings)
        evalConstFunctionValueWithEnv ctx constIndex fnIndex structIndex env' seenConsts seenFns' decl
  let (errs, results) = partitionEithers (map attempt decls)
  case results of
    [v] -> Right v
    [] ->
      case errs of
        (err:_) -> Left err
        [] -> Left (CompileError "const function overload not found" Nothing Nothing)
    _ -> Left (CompileError "const function overload is ambiguous" Nothing Nothing)

data ConstControl
  = CCNone
  | CCReturn ConstValue
  | CCBreak
  | CCContinue
  deriving (Eq, Show)

constEvalMaxSteps :: Int
constEvalMaxSteps = 20000

evalConstStmtList :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Int -> [Stmt] -> Either CompileError (ConstEnv, ConstControl, Int)
evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel stmts = go env fuel stmts
  where
    go env' fuel' [] = Right (env', CCNone, fuel')
    go env' fuel' (stmt:rest) = do
      (envNext, ctrl, fuelNext) <- evalConstStmt ctx constIndex fnIndex structIndex env' seenConsts seenFns fuel' stmt
      case ctrl of
        CCNone -> go envNext fuelNext rest
        _ -> Right (envNext, ctrl, fuelNext)

evalConstStmt :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Int -> Stmt -> Either CompileError (ConstEnv, ConstControl, Int)
evalConstStmt ctx constIndex fnIndex structIndex env seenConsts seenFns fuel stmt = do
  fuel' <- consumeConstFuel fuel
  case stmt of
    SLet name expr -> do
      val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      Right (Map.insert name (ConstBinding val False) env, CCNone, fuel')
    SVar name expr -> do
      val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      Right (Map.insert name (ConstBinding val True) env, CCNone, fuel')
    SAssign lv expr -> do
      oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
      newVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      let targetTy = constValueType oldVal
      newVal' <- coerceConstValueToType ctx structIndex targetTy newVal
      env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal'
      Right (env', CCNone, fuel')
    SAssignOp lv op expr -> do
      oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
      rhsVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      let targetTy = constValueType oldVal
      rhsVal' <- coerceConstValueToType ctx structIndex targetTy rhsVal
      newVal <- evalConstAssignOp op oldVal rhsVal'
      env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal
      Right (env', CCNone, fuel')
    SInc lv -> do
      oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
      one <- constScalarOne oldVal
      newVal <- evalConstAssignOp OpAdd oldVal one
      env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal
      Right (env', CCNone, fuel')
    SDec lv -> do
      oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
      one <- constScalarOne oldVal
      newVal <- evalConstAssignOp OpSub oldVal one
      env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal
      Right (env', CCNone, fuel')
    SExpr expr -> do
      _ <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      Right (env, CCNone, fuel')
    SSwitch expr cases defBody -> do
      cases' <- expandSwitchCases cases defBody
      ConstInt _ selector <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      matchCase selector cases' defBody fuel'
    SIf cond thenBody elseBody -> do
      ok <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns cond
      if ok
        then do
          (env', ctrl, fuel'') <- evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel' thenBody
          Right (env', ctrl, fuel'')
        else
          case elseBody of
            Nothing -> Right (env, CCNone, fuel')
            Just body -> do
              (env', ctrl, fuel'') <- evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel' body
              Right (env', ctrl, fuel'')
    SWhile cond body -> evalConstWhile cond body fuel'
    SLoop body continuing -> evalConstLoop body continuing fuel'
    SFor initStmt condExpr contStmt body -> evalConstFor initStmt condExpr contStmt body fuel'
    SBreak -> Right (env, CCBreak, fuel')
    SBreakIf cond -> do
      ok <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns cond
      Right (env, if ok then CCBreak else CCNone, fuel')
    SContinue -> Right (env, CCContinue, fuel')
    SReturn (Just expr) -> do
      val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      Right (env, CCReturn val, fuel')
    SReturn Nothing ->
      Left (CompileError "const function return requires a value" Nothing Nothing)
    _ -> Left (CompileError "const function bodies may only contain let/var, if, switch, loops, assignments, expr, and return statements" Nothing Nothing)
  where
    matchCase _ [] defBody fuel'' =
      case defBody of
        Nothing -> Right (env, CCNone, fuel'')
        Just body -> evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel'' body
    matchCase selector (case0:rest) defBody fuel'' = do
      matched <- or <$> mapM (matchesSelector selector) (scSelectors case0)
      if matched
        then evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel'' (scBody case0)
        else matchCase selector rest defBody fuel''

    matchesSelector selector ex = do
      ConstInt _ val <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns ex
      Right (val == selector)

    consumeConstFuel n =
      if n <= 0
        then Left (CompileError "const evaluation exceeded step limit" Nothing Nothing)
        else Right (n - 1)

    evalConstWhile cond body fuel'' = loop fuel'' env
      where
        loop fuelLoop envLoop = do
          fuelNext <- consumeConstFuel fuelLoop
          ok <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex envLoop seenConsts seenFns cond
          if not ok
            then Right (envLoop, CCNone, fuelNext)
            else do
              (envBody, ctrl, fuelBody) <- evalConstStmtList ctx constIndex fnIndex structIndex envLoop seenConsts seenFns fuelNext body
              case ctrl of
                CCNone -> loop fuelBody envBody
                CCContinue -> loop fuelBody envBody
                CCBreak -> Right (envBody, CCNone, fuelBody)
                _ -> Right (envBody, ctrl, fuelBody)

    evalConstLoop body continuing fuel'' = loop fuel'' env
      where
        loop fuelLoop envLoop = do
          fuelNext <- consumeConstFuel fuelLoop
          (envBody, ctrl, fuelBody) <- evalConstStmtList ctx constIndex fnIndex structIndex envLoop seenConsts seenFns fuelNext body
          case ctrl of
            CCReturn _ -> Right (envBody, ctrl, fuelBody)
            CCBreak -> Right (envBody, CCNone, fuelBody)
            CCContinue -> runContinuing envBody fuelBody
            CCNone -> runContinuing envBody fuelBody

        runContinuing envLoop fuelLoop =
          case continuing of
            Nothing -> loop fuelLoop envLoop
            Just contBody -> do
              (envCont, ctrlCont, fuelCont) <- evalConstStmtList ctx constIndex fnIndex structIndex envLoop seenConsts seenFns fuelLoop contBody
              case ctrlCont of
                CCReturn _ -> Right (envCont, ctrlCont, fuelCont)
                CCBreak -> Right (envCont, CCNone, fuelCont)
                _ -> loop fuelCont envCont

    evalConstFor initStmt condExpr contStmt body fuel'' = do
      (envInit, ctrlInit, fuelInit) <- case initStmt of
        Nothing -> Right (env, CCNone, fuel'')
        Just s -> evalConstStmt ctx constIndex fnIndex structIndex env seenConsts seenFns fuel'' s
      case ctrlInit of
        CCNone -> loop fuelInit envInit
        _ -> Left (CompileError "invalid control flow in for initializer" Nothing Nothing)
      where
        loop fuelLoop envLoop = do
          fuelNext <- consumeConstFuel fuelLoop
          ok <- case condExpr of
            Nothing -> Right True
            Just cond -> evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex envLoop seenConsts seenFns cond
          if not ok
            then Right (envLoop, CCNone, fuelNext)
            else do
              (envBody, ctrlBody, fuelBody) <- evalConstStmtList ctx constIndex fnIndex structIndex envLoop seenConsts seenFns fuelNext body
              case ctrlBody of
                CCReturn _ -> Right (envBody, ctrlBody, fuelBody)
                CCBreak -> Right (envBody, CCNone, fuelBody)
                _ -> do
                  (envCont, ctrlCont, fuelCont) <- case contStmt of
                    Nothing -> Right (envBody, CCNone, fuelBody)
                    Just s -> evalConstStmt ctx constIndex fnIndex structIndex envBody seenConsts seenFns fuelBody s
                  case ctrlCont of
                    CCNone -> loop fuelCont envCont
                    CCContinue -> loop fuelCont envCont
                    _ -> Right (envCont, ctrlCont, fuelCont)

evalConstLValueGet :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> LValue -> Either CompileError ConstValue
evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv =
  case lv of
    LVVar name ->
      case Map.lookup name env of
        Just binding -> Right (cbValue binding)
        Nothing -> Left (CompileError ("unknown variable: " <> textToString name) Nothing Nothing)
    LVField base field -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      evalConstFieldAccess baseVal field
    LVIndex base idxExpr -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns idxExpr
      evalConstIndexAccess baseVal idxVal
    LVDeref expr -> do
      ptr <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      case ptr of
        CVPointer _ ptrLv -> evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns ptrLv
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)

evalConstLValueSet :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> LValue -> ConstValue -> Either CompileError ConstEnv
evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal =
  case lv of
    LVVar name ->
      case Map.lookup name env of
        Nothing -> Left (CompileError ("unknown variable: " <> textToString name) Nothing Nothing)
        Just binding ->
          if cbMutable binding
            then Right (Map.insert name binding { cbValue = newVal } env)
            else Left (CompileError "cannot assign to immutable let binding" Nothing Nothing)
    LVField base field -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      updated <- updateFieldValue baseVal field newVal
      evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns base updated
    LVIndex base idxExpr -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns idxExpr
      updated <- updateIndexValue baseVal idxVal newVal
      evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns base updated
    LVDeref expr -> do
      ptr <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      case ptr of
        CVPointer _ ptrLv -> evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns ptrLv newVal
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)

updateFieldValue :: ConstValue -> Text -> ConstValue -> Either CompileError ConstValue
updateFieldValue base field newVal =
  case base of
    CVStruct name pairs ->
      if any ((== field) . fst) pairs
        then Right (CVStruct name (map upd pairs))
        else Left (CompileError ("unknown field: " <> textToString field) Nothing Nothing)
      where
        upd (fname, fval)
          | fname == field = (fname, newVal)
          | otherwise = (fname, fval)
    CVVector n scalar comps -> do
      ix <- vectorFieldIndex field n
      newScalar <- coerceConstScalarValue scalar newVal
      let comps' = [if i == ix then newScalar else c | (i, c) <- zip [0..] comps]
      Right (CVVector n scalar comps')
    _ -> Left (CompileError "field access requires struct or vector type" Nothing Nothing)

updateIndexValue :: ConstValue -> ConstInt -> ConstValue -> Either CompileError ConstValue
updateIndexValue base (ConstInt _ raw) newVal = do
  when (raw < 0) $
    Left (CompileError "index must be non-negative" Nothing Nothing)
  when (raw > fromIntegral (maxBound :: Int)) $
    Left (CompileError "index is out of range" Nothing Nothing)
  let ix = fromIntegral raw :: Int
  case base of
    CVVector n scalar comps ->
      if ix < n
        then do
          newScalar <- coerceConstScalarValue scalar newVal
          let comps' = [if i == ix then newScalar else c | (i, c) <- zip [0..] comps]
          Right (CVVector n scalar comps')
        else Left (CompileError "vector index out of range" Nothing Nothing)
    CVMatrix cols rows scalar colsVals ->
      if ix < cols
        then do
          let targetTy = TyVector rows scalar
          when (constValueType newVal /= targetTy) $
            Left (CompileError "matrix index assignment type mismatch" Nothing Nothing)
          let cols' = [if i == ix then newVal else c | (i, c) <- zip [0..] colsVals]
          Right (CVMatrix cols rows scalar cols')
        else Left (CompileError "matrix index out of range" Nothing Nothing)
    CVArray elemTy vals ->
      if ix < length vals
        then do
          when (constValueType newVal /= elemTy) $
            Left (CompileError "array index assignment type mismatch" Nothing Nothing)
          let vals' = [if i == ix then newVal else c | (i, c) <- zip [0..] vals]
          Right (CVArray elemTy vals')
        else Left (CompileError "array index out of range" Nothing Nothing)
    _ -> Left (CompileError "indexing requires array, vector, or matrix type" Nothing Nothing)

constScalarOne :: ConstValue -> Either CompileError ConstValue
constScalarOne val =
  case val of
    CVInt (ConstInt scalar _) -> Right (CVInt (ConstInt scalar 1))
    CVFloat (ConstFloat scalar _) -> Right (CVFloat (ConstFloat scalar 1.0))
    _ -> Left (CompileError "increment/decrement requires a scalar integer or float" Nothing Nothing)

evalConstAssignOp :: BinOp -> ConstValue -> ConstValue -> Either CompileError ConstValue
evalConstAssignOp op lhs rhs =
  case (lhs, rhs) of
    (CVInt (ConstInt scalar a), CVInt (ConstInt _ b)) -> do
      let result = applyIntOp scalar a b
      CVInt <$> result
    (CVFloat (ConstFloat scalar a), CVFloat (ConstFloat _ b)) -> do
      v <- applyFloatOp a b
      Right (CVFloat (ConstFloat scalar (if scalar == F16 then quantizeF16 v else v)))
    (CVBool a, CVBool b) ->
      case op of
        OpAnd -> Right (CVBool (a && b))
        OpOr -> Right (CVBool (a || b))
        _ -> Left (CompileError "unsupported boolean assignment operation" Nothing Nothing)
    (CVVector n scalar xs, CVVector m _ ys)
      | n == m && length xs == length ys ->
          CVVector n scalar <$> zipWithM (evalConstAssignOp op) xs ys
    (CVMatrix c r s xs, CVMatrix c' r' _ ys)
      | c == c' && r == r' && length xs == length ys ->
          CVMatrix c r s <$> zipWithM (evalConstAssignOp op) xs ys
    _ -> Left (CompileError "unsupported assignment operand types" Nothing Nothing)
  where
    applyIntOp scalar a b =
      case op of
        OpAdd -> makeInt scalar (a + b)
        OpSub -> makeInt scalar (a - b)
        OpMul -> makeInt scalar (a * b)
        OpDiv ->
          if b == 0
            then Left (CompileError "division by zero in constant expression" Nothing Nothing)
            else makeInt scalar (a `quot` b)
        OpMod ->
          if b == 0
            then Left (CompileError "modulo by zero in constant expression" Nothing Nothing)
            else makeInt scalar (a `rem` b)
        OpBitAnd -> makeInt scalar (a .&. b)
        OpBitOr -> makeInt scalar (a .|. b)
        OpBitXor -> makeInt scalar (xor a b)
        OpShl ->
          if b < 0 then Left (CompileError "shift amount must be non-negative" Nothing Nothing) else makeInt scalar (shiftL a (fromIntegral b))
        OpShr ->
          if b < 0 then Left (CompileError "shift amount must be non-negative" Nothing Nothing) else makeInt scalar (shiftR a (fromIntegral b))
        _ -> Left (CompileError "unsupported integer assignment operation" Nothing Nothing)

    makeInt scalar n =
      case scalar of
        I32 -> checkI32 n >> Right (ConstInt I32 n)
        U32 -> checkU32 n >> Right (ConstInt U32 n)
        _ -> Left (CompileError "unsupported integer assignment operation" Nothing Nothing)

    applyFloatOp a b =
      case op of
        OpAdd -> Right (a + b)
        OpSub -> Right (a - b)
        OpMul -> Right (a * b)
        OpDiv ->
          if b == 0.0
            then Left (CompileError "division by zero in constant expression" Nothing Nothing)
            else Right (a / b)
        _ -> Left (CompileError "unsupported float assignment operation" Nothing Nothing)

    checkU32 n =
      when (n < 0 || n > fromIntegral (maxBound :: Word32)) $
        Left (CompileError "constant u32 is out of range" Nothing Nothing)

    checkI32 n =
      when (n < 0 || n > 0x7FFFFFFF) $
        Left (CompileError "constant i32 is out of range" Nothing Nothing)

evalConstFieldAccess :: ConstValue -> Text -> Either CompileError ConstValue
evalConstFieldAccess val field =
  case val of
    CVVector n scalar comps -> do
      idxs <- vectorFieldIndices field n
      case idxs of
        [ix] -> Right (comps !! fromIntegral ix)
        _ -> do
          let comps' = map (\ix -> comps !! fromIntegral ix) idxs
          Right (CVVector (length idxs) scalar comps')
    CVStruct _ fields ->
      case lookup field fields of
        Just v -> Right v
        Nothing -> Left (CompileError ("unknown field: " <> textToString field) Nothing Nothing)
    _ -> Left (CompileError "field access requires struct or vector type" Nothing Nothing)

evalConstIndexAccess :: ConstValue -> ConstInt -> Either CompileError ConstValue
evalConstIndexAccess val (ConstInt _ raw) = do
  when (raw < 0) $
    Left (CompileError "index must be non-negative" Nothing Nothing)
  when (raw > fromIntegral (maxBound :: Int)) $
    Left (CompileError "index is out of range" Nothing Nothing)
  let ix = fromIntegral raw :: Int
  case val of
    CVVector n _ comps ->
      if ix < n then Right (comps !! ix) else Left (CompileError "vector index out of range" Nothing Nothing)
    CVMatrix cols _ _ colsVals ->
      if ix < cols then Right (colsVals !! ix) else Left (CompileError "matrix index out of range" Nothing Nothing)
    CVArray _ vals ->
      if ix < length vals then Right (vals !! ix) else Left (CompileError "array index out of range" Nothing Nothing)
    _ -> Left (CompileError "indexing requires array, vector, or matrix type" Nothing Nothing)

evalConstIntExpr :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Expr -> Either CompileError ConstInt
evalConstIntExpr ctx constIndex fnIndex structIndex expr =
  evalConstIntExprWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty expr

evalConstIntExprWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError ConstInt
evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr = go seenConsts seenFns expr
  where
    go seen fnSeen ex =
      case ex of
        EInt n -> do
          (scalar, val) <- selectIntLiteralScalar n
          case scalar of
            I32 -> do
              checkI32 val
              Right (ConstInt I32 val)
            U32 -> do
              checkU32 val
              Right (ConstInt U32 val)
            _ -> Left (CompileError "integer literal must be i32 or u32" Nothing Nothing)
        EUnary OpNeg inner -> do
          ConstInt scalar val <- go seen fnSeen inner
          case scalar of
            I32 -> do
              let v = negate val
              checkI32 v
              Right (ConstInt I32 v)
            U32 -> Left (CompileError "unary minus is not supported for u32 in const expressions" Nothing Nothing)
            _ -> Left (CompileError "unary minus expects integer constants" Nothing Nothing)
        EUnary OpDeref inner -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen (EUnary OpDeref inner)
          constValueToInt val
        EFloat _ ->
          Left (CompileError "const int expression references a float literal" Nothing Nothing)
        ECall "u32" [arg] ->
          case go seen fnSeen arg of
            Right (ConstInt _ n) -> do
              checkU32 n
              Right (ConstInt U32 n)
            _ -> do
              ConstFloat _ v <- evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen arg
              let n = truncate v :: Integer
              checkU32 n
              Right (ConstInt U32 n)
        ECall "i32" [arg] ->
          case go seen fnSeen arg of
            Right (ConstInt _ n) -> do
              checkI32 n
              Right (ConstInt I32 n)
            _ -> do
              ConstFloat _ v <- evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen arg
              let n = truncate v :: Integer
              checkI32 n
              Right (ConstInt I32 n)
        EBitcast _ _ ->
          Left (CompileError "bitcast is not allowed in const integer expressions" Nothing Nothing)
        ECall name args
          | not (isBuiltinName name) -> do
              val <- evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
              constValueToInt val
        EBinary op a b -> do
          ConstInt sa va <- go seen fnSeen a
          ConstInt sb vb <- go seen fnSeen b
          (scalar, x, y) <- coercePair sa va sb vb
          case op of
            OpAdd -> applyIntOp scalar (+) x y
            OpSub -> applyIntOp scalar (-) x y
            OpMul -> applyIntOp scalar (*) x y
            OpDiv -> applyIntDiv scalar x y
            OpMod -> applyIntMod scalar x y
            OpBitAnd -> applyIntOp scalar (.&.) x y
            OpBitOr -> applyIntOp scalar (.|.) x y
            OpBitXor -> applyIntOp scalar xor x y
            OpShl -> applyShift scalar x y True
            OpShr -> applyShift scalar x y False
            _ -> Left (CompileError "unsupported const integer operation" Nothing Nothing)
        EVar name ->
          case Map.lookup name env of
            Just binding ->
              case cbValue binding of
                CVInt v -> Right v
                CVBool _ -> Left (CompileError "const int expression references a bool value" Nothing Nothing)
                CVFloat _ -> Left (CompileError "const int expression references a float value" Nothing Nothing)
                _ -> Left (CompileError "const int expression references a composite value" Nothing Nothing)
            Nothing -> do
              (path, ident) <- resolveConstRef ctx name
              let key = T.intercalate "::" (path <> [ident])
              when (Set.member key seen) $
                Left (CompileError "cycle detected while evaluating constant selector" Nothing Nothing)
              let entry = lookupConstIndex constIndex path ident
              case entry of
                Nothing -> Left (CompileError ("unknown constant: " <> textToString ident) Nothing Nothing)
                Just expr' -> evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
        EField _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToInt val
        EIndex _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToInt val
        _ -> Left (CompileError "switch case selector must be a constant integer expression" Nothing Nothing)

    checkU32 n =
      when (n < 0 || n > fromIntegral (maxBound :: Word32)) $
        Left (CompileError "switch case selector is out of range for u32" Nothing Nothing)

    checkI32 n =
      when (n < 0 || n > 0x7FFFFFFF) $
        Left (CompileError "switch case selector is out of range for i32" Nothing Nothing)

    coercePair sa va sb vb =
      case (sa, sb) of
        (I32, I32) -> Right (I32, va, vb)
        (U32, U32) -> Right (U32, va, vb)
        (I32, U32) ->
          if vb <= 0x7FFFFFFF
            then Right (I32, va, vb)
            else Left (CompileError "constant u32 is out of range for i32 operation" Nothing Nothing)
        (U32, I32) ->
          if va <= 0x7FFFFFFF
            then Right (I32, va, vb)
            else Left (CompileError "constant u32 is out of range for i32 operation" Nothing Nothing)
        _ -> Left (CompileError "unsupported const integer types" Nothing Nothing)

    applyIntOp scalar f x y =
      let v = f x y
      in case scalar of
          I32 -> checkI32 v >> Right (ConstInt I32 v)
          U32 -> checkU32 v >> Right (ConstInt U32 v)
          _ -> Left (CompileError "unsupported const integer operation" Nothing Nothing)

    applyIntDiv scalar x y =
      if y == 0
        then Left (CompileError "division by zero in constant expression" Nothing Nothing)
        else case scalar of
          I32 ->
            let v = (fromIntegral x :: Integer) `quot` (fromIntegral y :: Integer)
            in checkI32 v >> Right (ConstInt I32 v)
          U32 ->
            let v = (fromIntegral x :: Integer) `div` (fromIntegral y :: Integer)
            in checkU32 v >> Right (ConstInt U32 v)
          _ -> Left (CompileError "unsupported const integer division" Nothing Nothing)

    applyIntMod scalar x y =
      if y == 0
        then Left (CompileError "modulo by zero in constant expression" Nothing Nothing)
        else case scalar of
          I32 ->
            let v = (fromIntegral x :: Integer) `rem` (fromIntegral y :: Integer)
            in checkI32 v >> Right (ConstInt I32 v)
          U32 ->
            let v = (fromIntegral x :: Integer) `mod` (fromIntegral y :: Integer)
            in checkU32 v >> Right (ConstInt U32 v)
          _ -> Left (CompileError "unsupported const integer modulo" Nothing Nothing)

    applyShift scalar x y isLeft =
      if y < 0
        then Left (CompileError "shift amount must be non-negative" Nothing Nothing)
        else
          let v = if isLeft
                    then shiftL x (fromIntegral y)
                    else shiftR x (fromIntegral y)
          in case scalar of
              I32 -> checkI32 v >> Right (ConstInt I32 v)
              U32 -> checkU32 v >> Right (ConstInt U32 v)
              _ -> Left (CompileError "unsupported const integer shift" Nothing Nothing)

convertConstFloatTo :: Scalar -> ConstFloat -> ConstFloat
convertConstFloatTo target (ConstFloat _ v) =
  ConstFloat target (if target == F16 then quantizeF16 v else v)

quantizeF16 :: Double -> Double
quantizeF16 v =
  let f = realToFrac v :: Float
      bits = floatToHalfBits f
  in realToFrac (halfBitsToFloat bits) :: Double

evalConstFloatExprWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError ConstFloat
evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr = go seenConsts seenFns expr
  where
    go seen fnSeen ex =
      case ex of
        EFloat f -> Right (ConstFloat F32 (realToFrac f))
        EInt n -> do
          (_, val) <- selectIntLiteralScalar n
          Right (ConstFloat F32 (fromIntegral val))
        EUnary OpNeg inner -> do
          cf <- go seen fnSeen inner
          Right (applyFloatOp (cfScalar cf) negate (cfValue cf))
        EUnary OpDeref inner -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen (EUnary OpDeref inner)
          constValueToFloat val
        ECall "f32" [arg] -> do
          cf <- evalFloatArg seen fnSeen arg
          Right (convertConstFloatTo F32 cf)
        ECall "f16" [arg] -> do
          cf <- evalFloatArg seen fnSeen arg
          Right (convertConstFloatTo F16 cf)
        ECall "abs" [arg] -> do
          cf <- evalFloatArg seen fnSeen arg
          Right (applyFloatOp (cfScalar cf) abs (cfValue cf))
        ECall "min" [a, b] -> do
          cfA <- evalFloatArg seen fnSeen a
          cfB <- evalFloatArg seen fnSeen b
          let (scalar, x, y) = coerceFloatPair cfA cfB
          Right (applyFloatOp scalar (\v -> v) (min x y))
        ECall "max" [a, b] -> do
          cfA <- evalFloatArg seen fnSeen a
          cfB <- evalFloatArg seen fnSeen b
          let (scalar, x, y) = coerceFloatPair cfA cfB
          Right (applyFloatOp scalar (\v -> v) (max x y))
        ECall "clamp" [xExpr, loExpr, hiExpr] -> do
          cfX <- evalFloatArg seen fnSeen xExpr
          cfLo <- evalFloatArg seen fnSeen loExpr
          cfHi <- evalFloatArg seen fnSeen hiExpr
          let (scalar1, x, lo) = coerceFloatPair cfX cfLo
          let hi = cfValue (convertConstFloatTo scalar1 cfHi)
          Right (applyFloatOp scalar1 (\v -> v) (min (max x lo) hi))
        ECall "mix" [aExpr, bExpr, tExpr] -> do
          cfA <- evalFloatArg seen fnSeen aExpr
          cfB <- evalFloatArg seen fnSeen bExpr
          cfT <- evalFloatArg seen fnSeen tExpr
          let (scalar1, a, b) = coerceFloatPair cfA cfB
          let t = cfValue (convertConstFloatTo scalar1 cfT)
          Right (applyFloatOp scalar1 (\v -> v) (a * (1.0 - t) + b * t))
        ECall "select" [aExpr, bExpr, condExpr] -> do
          cond <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen condExpr
          cfA <- evalFloatArg seen fnSeen aExpr
          cfB <- evalFloatArg seen fnSeen bExpr
          Right (if cond then cfB else cfA)
        ECall name args
          | not (isBuiltinName name) -> do
              val <- evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
              constValueToFloat val
        EBinary op a b -> do
          cfA <- evalFloatArg seen fnSeen a
          cfB <- evalFloatArg seen fnSeen b
          let (scalar, x, y) = coerceFloatPair cfA cfB
          case op of
            OpAdd -> Right (applyFloatOp scalar (\v -> v) (x + y))
            OpSub -> Right (applyFloatOp scalar (\v -> v) (x - y))
            OpMul -> Right (applyFloatOp scalar (\v -> v) (x * y))
            OpDiv ->
              if y == 0.0
                then Left (CompileError "division by zero in constant expression" Nothing Nothing)
                else Right (applyFloatOp scalar (\v -> v) (x / y))
            _ -> Left (CompileError "unsupported const float operation" Nothing Nothing)
        EVar name ->
          case Map.lookup name env of
            Just binding ->
              case cbValue binding of
                CVFloat v -> Right v
                CVInt (ConstInt _ v) -> Right (ConstFloat F32 (fromIntegral v))
                CVBool _ -> Left (CompileError "const float expression references a bool value" Nothing Nothing)
                _ -> Left (CompileError "const float expression references a composite value" Nothing Nothing)
            Nothing -> do
              (path, ident) <- resolveConstRef ctx name
              let key = T.intercalate "::" (path <> [ident])
              when (Set.member key seen) $
                Left (CompileError "cycle detected while evaluating const float expression" Nothing Nothing)
              let entry = lookupConstIndex constIndex path ident
              case entry of
                Nothing -> Left (CompileError ("unknown constant: " <> textToString ident) Nothing Nothing)
                Just expr' -> evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
        EField _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToFloat val
        EIndex _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToFloat val
        _ -> Left (CompileError "const float expression requires a constant float expression" Nothing Nothing)

    evalFloatArg seen fnSeen ex = do
      cf <- go seen fnSeen ex
      Right cf

    coerceFloatPair a b =
      let scalar = if cfScalar a == F32 || cfScalar b == F32 then F32 else F16
          a' = convertConstFloatTo scalar a
          b' = convertConstFloatTo scalar b
      in (scalar, cfValue a', cfValue b')

    applyFloatOp scalar f v =
      let v' = f v
      in ConstFloat scalar (if scalar == F16 then quantizeF16 v' else v')

evalConstBoolExpr :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Expr -> Either CompileError Bool
evalConstBoolExpr ctx constIndex fnIndex structIndex expr =
  evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty expr

evalConstBoolExprWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError Bool
evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr = go seenConsts seenFns expr
  where
    go seen fnSeen ex =
      case ex of
        EBool b -> Right b
        EUnary OpNot a -> not <$> go seen fnSeen a
        EUnary OpDeref inner -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen (EUnary OpDeref inner)
          constValueToBool val
        EBinary OpAnd a b -> (&&) <$> go seen fnSeen a <*> go seen fnSeen b
        EBinary OpOr a b -> (||) <$> go seen fnSeen a <*> go seen fnSeen b
        EBinary OpEq a b -> evalEq seen fnSeen a b
        EBinary OpNe a b -> not <$> evalEq seen fnSeen a b
        EBinary OpLt a b -> evalCmp seen fnSeen (<) (<) a b
        EBinary OpLe a b -> evalCmp seen fnSeen (<=) (<=) a b
        EBinary OpGt a b -> evalCmp seen fnSeen (>) (>) a b
        EBinary OpGe a b -> evalCmp seen fnSeen (>=) (>=) a b
        ECall "select" [aExpr, bExpr, condExpr] -> do
          cond <- go seen fnSeen condExpr
          if cond then go seen fnSeen bExpr else go seen fnSeen aExpr
        EBitcast _ _ ->
          Left (CompileError "bitcast is not allowed in const boolean expressions" Nothing Nothing)
        ECall name args
          | not (isBuiltinName name) -> do
              val <- evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
              constValueToBool val
        EVar name ->
          case Map.lookup name env of
            Just binding ->
              case cbValue binding of
                CVBool b -> Right b
                CVInt _ -> Left (CompileError "const bool expression references an int value" Nothing Nothing)
                CVFloat _ -> Left (CompileError "const bool expression references a float value" Nothing Nothing)
                _ -> Left (CompileError "const bool expression references a composite value" Nothing Nothing)
            Nothing -> do
              (path, ident) <- resolveConstRef ctx name
              let key = T.intercalate "::" (path <> [ident])
              when (Set.member key seen) $
                Left (CompileError "cycle detected while evaluating const_assert" Nothing Nothing)
              let entry = lookupConstIndex constIndex path ident
              case entry of
                Nothing -> Left (CompileError ("unknown constant: " <> textToString ident) Nothing Nothing)
                Just expr' -> evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
        EField _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToBool val
        EIndex _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToBool val
        _ -> Left (CompileError "const_assert requires a constant boolean expression" Nothing Nothing)

    evalEq seen fnSeen a b =
      case (go seen fnSeen a, go seen fnSeen b) of
        (Right x, Right y) -> Right (x == y)
        _ ->
          let valA = evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
              valB = evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
              isPointerConst v =
                case v of
                  CVPointer _ _ -> True
                  _ -> False
              isCompositeConst v =
                case v of
                  CVVector _ _ _ -> True
                  CVMatrix _ _ _ _ -> True
                  CVArray _ _ -> True
                  CVStruct _ _ -> True
                  _ -> False
          in case (valA, valB) of
              (Right va, Right vb)
                | isPointerConst va || isPointerConst vb ->
                    Left (CompileError "const_assert cannot compare pointer values" Nothing Nothing)
                | isCompositeConst va || isCompositeConst vb ->
                    if constValueType va == constValueType vb
                      then Right (va == vb)
                      else Left (CompileError "const_assert comparison requires matching types" Nothing Nothing)
              _ ->
                let intA = evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
                    intB = evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
                in case (intA, intB) of
                    (Right (ConstInt _ x), Right (ConstInt _ y)) -> Right (x == y)
                    _ ->
                      let floatA = evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
                          floatB = evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
                      in case (floatA, floatB) of
                          (Right (ConstFloat _ x), Right (ConstFloat _ y)) -> Right (x == y)
                          _ -> Left (firstError intA intB)

    evalCmp seen fnSeen cmpInt cmpFloat a b =
      let intA = evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
          intB = evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
      in case (intA, intB) of
          (Right (ConstInt _ x), Right (ConstInt _ y)) -> Right (cmpInt x y)
          _ ->
            let floatA = evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
                floatB = evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
            in case (floatA, floatB) of
                (Right (ConstFloat _ x), Right (ConstFloat _ y)) -> Right (cmpFloat x y)
                _ -> Left (firstError intA intB)

    firstError ea eb =
      case ea of
        Left err -> err
        _ ->
          case eb of
            Left err -> err
            _ -> CompileError "const_assert requires a constant boolean expression" Nothing Nothing

coerceConstIntToScalar :: Scalar -> ConstInt -> Either CompileError ConstInt
coerceConstIntToScalar target (ConstInt scalar val) =
  case (target, scalar) of
    (I32, I32) -> Right (ConstInt I32 val)
    (U32, U32) -> Right (ConstInt U32 val)
    (I32, U32) ->
      if val <= 0x7FFFFFFF
        then Right (ConstInt I32 val)
        else Left (CompileError "constant u32 is out of range for i32" Nothing Nothing)
    (U32, I32) ->
      if val >= 0 && val <= fromIntegral (maxBound :: Word32)
        then Right (ConstInt U32 val)
        else Left (CompileError "constant i32 is out of range for u32" Nothing Nothing)
    _ -> Left (CompileError "unsupported const integer coercion" Nothing Nothing)


resolveConstRef :: ModuleContext -> Text -> Either CompileError ([Text], Text)
resolveConstRef ctx name =
  case splitQName name of
    [] -> Left (CompileError "invalid constant reference" Nothing Nothing)
    [single] ->
      if Set.member single (mcConstNames ctx)
        then Right (mcPath ctx, single)
        else case Map.lookup single (mcItemAliases ctx) of
          Just target ->
            case splitLast target of
              Nothing -> Left (CompileError "invalid constant reference" Nothing Nothing)
              Just (path, item) -> Right (path, item)
          Nothing -> Left (CompileError ("unknown constant: " <> textToString single) Nothing Nothing)
    seg0 : segRest ->
      case Map.lookup seg0 (mcModuleAliases ctx) of
        Nothing -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> Left (CompileError "invalid constant reference" Nothing Nothing)
            Just (path, item) -> Right (path, item)

resolveFunctionRef :: ModuleContext -> Text -> Either CompileError ([Text], Text)
resolveFunctionRef ctx name =
  case splitQName name of
    [] -> Left (CompileError "invalid function reference" Nothing Nothing)
    [single] ->
      if Set.member single (mcFunctionNames ctx)
        then Right (mcPath ctx, single)
        else case Map.lookup single (mcItemAliases ctx) of
          Just target ->
            case splitLast target of
              Nothing -> Left (CompileError "invalid function reference" Nothing Nothing)
              Just (path, item) -> Right (path, item)
          Nothing -> Left (CompileError ("unknown function: " <> textToString single) Nothing Nothing)
    seg0 : segRest ->
      case Map.lookup seg0 (mcModuleAliases ctx) of
        Nothing -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> Left (CompileError "invalid function reference" Nothing Nothing)
            Just (path, item) -> Right (path, item)

validateLValue :: ModuleContext -> Scope -> LValue -> Either CompileError ()
validateLValue ctx scope lv =
  case lv of
    LVVar name -> validateName ctx scope name
    LVField base _ -> validateLValue ctx scope base
    LVIndex base idx -> validateLValue ctx scope base >> validateExpr ctx scope idx
    LVDeref expr -> validateExpr ctx scope expr

validateExpr :: ModuleContext -> Scope -> Expr -> Either CompileError ()
validateExpr ctx scope expr =
  case expr of
    EVar name -> validateName ctx scope name
    EInt _ -> Right ()
    EFloat _ -> Right ()
    EBool _ -> Right ()
    EBinary _ a b -> validateExpr ctx scope a >> validateExpr ctx scope b
    EUnary OpAddr a ->
      case exprToLValue a of
        Nothing -> Left (CompileError "address-of requires an addressable expression" Nothing Nothing)
        Just _ -> validateExpr ctx scope a
    EUnary OpDeref a -> validateExpr ctx scope a
    EUnary _ a -> validateExpr ctx scope a
    ECall name args -> validateName ctx scope name >> mapM_ (validateExpr ctx scope) args
    EBitcast ty arg -> validateType ctx scope ty >> validateExpr ctx scope arg
    EField base _ -> validateExpr ctx scope base
    EIndex base idx -> validateExpr ctx scope base >> validateExpr ctx scope idx

validateType :: ModuleContext -> Scope -> Type -> Either CompileError ()
validateType ctx scope ty =
  case ty of
    TyStructRef name ->
      case lookupNameId scope name of
        Just sid | IntSet.member sid (scTypeAliases scope) -> Right ()
        _ -> validateName ctx scope name
    TyArray elemTy _ -> validateType ctx scope elemTy
    TyVector _ _ -> Right ()
    TyMatrix _ _ _ -> Right ()
    TyTexture1D _ -> Right ()
    TyTexture1DArray _ -> Right ()
    TyTexture2D _ -> Right ()
    TyTexture2DArray _ -> Right ()
    TyTexture3D _ -> Right ()
    TyTextureCube _ -> Right ()
    TyTextureCubeArray _ -> Right ()
    TyTextureMultisampled2D _ -> Right ()
    TyTextureDepth2D -> Right ()
    TyTextureDepth2DArray -> Right ()
    TyTextureDepthCube -> Right ()
    TyTextureDepthCubeArray -> Right ()
    TyTextureDepthMultisampled2D -> Right ()
    TyStorageTexture1D _ _ -> Right ()
    TyStorageTexture2D _ _ -> Right ()
    TyStorageTexture2DArray _ _ -> Right ()
    TyStorageTexture3D _ _ -> Right ()
    TyAtomic _ -> Right ()
    TyPtr _ _ inner -> validateType ctx scope inner
    TySampler -> Right ()
    TySamplerComparison -> Right ()
    TyScalar _ -> Right ()

validateName :: ModuleContext -> Scope -> Text -> Either CompileError ()
validateName _ scope name =
  case splitQName name of
    [] -> Right ()
    [single] ->
      if isBuiltinName single
        then Right ()
        else
          case lookupNameId scope single of
            Just sid
              | IntSet.member sid (scopeLocals scope)
                  || IntSet.member sid (scGlobals scope)
                  || IntSet.member sid (scItemAliases scope) -> Right ()
            _ -> Left (CompileError ("unknown identifier: " <> textToString single) Nothing Nothing)
    seg0 : _ ->
      case lookupNameId scope seg0 of
        Just sid | IntSet.member sid (scModuleAliases scope) -> Right ()
        _ -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)

lookupNameId :: Scope -> Text -> Maybe Int
lookupNameId scope name = Map.lookup name (ntMap (scNameTable scope))

scopeLocals :: Scope -> IntSet.IntSet
scopeLocals scope = foldl' IntSet.union IntSet.empty (scScopes scope)

currentScope :: Scope -> IntSet.IntSet
currentScope scope =
  case scScopes scope of
    [] -> IntSet.empty
    s : _ -> s

scopeAdd :: Scope -> Text -> Either CompileError Scope
scopeAdd scope name =
  let (nameId, table') = internName name (scNameTable scope)
      scope' = scope { scNameTable = table' }
  in if IntSet.member nameId (currentScope scope')
      then Left (CompileError ("duplicate local declaration: " <> textToString name) Nothing Nothing)
      else
        if not (scAllowShadowing scope')
          && not (T.isPrefixOf "_" name)
          && IntSet.member nameId (scopeOuterLocals scope')
          then Left (CompileError ("shadowing is not allowed: " <> textToString name) Nothing Nothing)
          else
            case scScopes scope' of
              [] ->
                Right scope' { scScopes = [IntSet.singleton nameId] }
              current : rest ->
                Right scope' { scScopes = IntSet.insert nameId current : rest }

scopeOuterLocals :: Scope -> IntSet.IntSet
scopeOuterLocals scope =
  case scScopes scope of
    [] -> IntSet.empty
    _ : rest -> foldl' IntSet.union IntSet.empty rest

enterBlock :: Scope -> Scope
enterBlock scope = scope { scScopes = IntSet.empty : scScopes scope }

scopeWithParams :: Scope -> [Text] -> Scope
scopeWithParams scope names =
  let (table', ids) = internNames (scNameTable scope) names
  in scope { scNameTable = table', scScopes = [IntSet.fromList ids] }

ensureNoDuplicates :: Text -> [Text] -> Either CompileError ()
ensureNoDuplicates label names =
  let (dups, _) = foldl' collect ([], Set.empty) names
  in if null dups
      then Right ()
      else Left (CompileError ("duplicate " <> textToString label <> ": " <> T.unpack (T.intercalate ", " dups)) Nothing Nothing)
  where
    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)

isBuiltinName :: Text -> Bool
isBuiltinName name =
  name `Set.member` builtinNames || maybe False (const True) (parseMatrixName name)

builtinNames :: Set.Set Text
builtinNames =
  Set.fromList
    [ "vec2"
    , "vec3"
    , "vec4"
    , "array"
    , "f16"
    , "f32"
    , "u32"
    , "i32"
    , "abs"
    , "min"
    , "max"
    , "clamp"
    , "mix"
    , "select"
    , "any"
    , "all"
    , "round"
    , "roundEven"
    , "trunc"
    , "step"
    , "smoothstep"
    , "floor"
    , "ceil"
    , "fract"
    , "radians"
    , "degrees"
    , "exp"
    , "log"
    , "exp2"
    , "log2"
    , "sin"
    , "cos"
    , "tan"
    , "asin"
    , "acos"
    , "atan"
    , "atan2"
    , "sinh"
    , "cosh"
    , "tanh"
    , "asinh"
    , "acosh"
    , "atanh"
    , "pow"
    , "sqrt"
    , "inverseSqrt"
    , "fma"
    , "sign"
    , "length"
    , "normalize"
    , "dot"
    , "cross"
    , "distance"
    , "faceForward"
    , "reflect"
    , "refract"
    , "transpose"
    , "determinant"
    , "inverse"
    , "modf"
    , "frexp"
    , "ldexp"
    , "pack4x8snorm"
    , "pack4x8unorm"
    , "pack2x16snorm"
    , "pack2x16unorm"
    , "pack2x16float"
    , "unpack4x8snorm"
    , "unpack4x8unorm"
    , "unpack2x16snorm"
    , "unpack2x16unorm"
    , "unpack2x16float"
    , "firstLeadingBit"
    , "firstTrailingBit"
    , "saturate"
    , "quantizeToF16"
    , "countOneBits"
    , "countLeadingZeros"
    , "countTrailingZeros"
    , "reverseBits"
    , "extractBits"
    , "insertBits"
    , "dot4U8Packed"
    , "dot4I8Packed"
    , "arrayLength"
    , "textureSample"
    , "textureSampleCompare"
    , "textureSampleLevel"
    , "textureSampleBias"
    , "textureSampleGrad"
    , "textureSampleCompareLevel"
    , "textureGather"
    , "textureGatherCompare"
    , "textureDimensions"
    , "textureNumLevels"
    , "textureNumLayers"
    , "textureNumSamples"
    , "textureLoad"
    , "textureStore"
    , "dpdx"
    , "dpdy"
    , "fwidth"
    , "atomicLoad"
    , "atomicStore"
    , "atomicAdd"
    , "atomicSub"
    , "atomicMin"
    , "atomicMax"
    , "atomicAnd"
    , "atomicOr"
    , "atomicXor"
    , "atomicExchange"
    ]

qualifyModule :: [Text] -> Map.Map FilePath ModuleContext -> ModuleNode -> ModuleNode
qualifyModule rootPath ctxs node =
  let ctx = fromMaybe (buildModuleContext rootPath "" node) (Map.lookup (mnFile node) ctxs)
      ast = mnAst node
      prefix = mcPath ctx
      rename = qualNameWithRoot (mcRootPath ctx) prefix
      renameDecl name = rename name
      rewriteTy = rewriteType ctx
      rewriteParam = rewriteParamType ctx
      rewriteField = rewriteFieldDecl ctx
      rewriteStmt = rewriteStmtNames ctx
      rewriteExpr = rewriteExprNames ctx
      rewriteFn fn =
        fn
          { fnName = renameDecl (fnName fn)
          , fnParams = map rewriteParam (fnParams fn)
          , fnReturnType = fmap rewriteTy (fnReturnType fn)
          , fnBody = map rewriteStmt (fnBody fn)
          }
      rewriteConst c =
        c
          { cdName = renameDecl (cdName c)
          , cdExpr = rewriteExpr (cdExpr c)
          }
      rewriteAlias a =
        a
          { adName = renameDecl (adName a)
          , adType = rewriteTy (adType a)
          }
      rewriteOverride o =
        o
          { odName = renameDecl (odName o)
          , odType = rewriteTy (odType o)
          , odExpr = fmap rewriteExpr (odExpr o)
          }
      rewriteBinding b =
        b
          { bdName = renameDecl (bdName b)
          , bdType = rewriteTy (bdType b)
          }
      rewriteGlobal g =
        g
          { gvName = renameDecl (gvName g)
          , gvType = rewriteTy (gvType g)
          , gvInit = fmap rewriteExpr (gvInit g)
          }
      rewriteStruct s =
        s
          { sdName = renameDecl (sdName s)
          , sdFields = map rewriteField (sdFields s)
          }
      rewriteConstAssert (ConstAssert pos expr) =
        ConstAssert pos (rewriteExpr expr)
      entry =
        case modEntry ast of
          Nothing -> Nothing
          Just e ->
            if prefix == rootPath
              then Just
                e
                  { epParams = map rewriteParam (epParams e)
                  , epReturnType = fmap rewriteTy (epReturnType e)
                  , epBody = map rewriteStmt (epBody e)
                  }
              else Nothing
      ast' =
        ast
          { modImports = []
          , modAliases = map rewriteAlias (modAliases ast)
          , modStructs = map rewriteStruct (modStructs ast)
          , modBindings = map rewriteBinding (modBindings ast)
          , modGlobals = map rewriteGlobal (modGlobals ast)
          , modConsts = map rewriteConst (modConsts ast)
          , modOverrides = map rewriteOverride (modOverrides ast)
          , modConstAsserts = map rewriteConstAssert (modConstAsserts ast)
          , modFunctions = map rewriteFn (modFunctions ast)
          , modEntry = entry
          }
  in node { mnAst = ast' }

qualNameWithRoot :: [Text] -> [Text] -> Text -> Text
qualNameWithRoot rootPath path name =
  if null path || path == rootPath
    then name
    else "__wesl__" <> T.intercalate "__" path <> "__" <> name

rewriteIdent :: ModuleContext -> Text -> Text
rewriteIdent ctx name =
  case splitQName name of
    [] -> name
    [single] ->
      if Set.member single (mcLocals ctx)
        then
          if mcPath ctx == mcRootPath ctx
            then single
            else qualNameWithRoot (mcRootPath ctx) (mcPath ctx) single
        else
          case Map.lookup single (mcItemAliases ctx) of
            Just target ->
              case splitLast target of
                Nothing -> single
                Just (path, item) -> qualNameWithRoot (mcRootPath ctx) path item
            Nothing -> single
    seg0 : segRest ->
      case Map.lookup seg0 (mcModuleAliases ctx) of
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> name
            Just (path, item) -> qualNameWithRoot (mcRootPath ctx) path item
        Nothing ->
          case splitLast (seg0 : segRest) of
            Nothing -> name
            Just (path, item) -> qualNameWithRoot (mcRootPath ctx) path item

rewriteType :: ModuleContext -> Type -> Type
rewriteType ctx ty =
  case ty of
    TyStructRef name -> TyStructRef (rewriteIdent ctx name)
    TyArray elemTy n -> TyArray (rewriteType ctx elemTy) n
    TyVector n s -> TyVector n s
    TyMatrix c r s -> TyMatrix c r s
    TyTexture1D s -> TyTexture1D s
    TyTexture1DArray s -> TyTexture1DArray s
    TyTexture2D s -> TyTexture2D s
    TyTexture2DArray s -> TyTexture2DArray s
    TyTexture3D s -> TyTexture3D s
    TyTextureCube s -> TyTextureCube s
    TyTextureCubeArray s -> TyTextureCubeArray s
    TyTextureMultisampled2D s -> TyTextureMultisampled2D s
    TyTextureDepth2D -> TyTextureDepth2D
    TyTextureDepth2DArray -> TyTextureDepth2DArray
    TyTextureDepthCube -> TyTextureDepthCube
    TyTextureDepthCubeArray -> TyTextureDepthCubeArray
    TyTextureDepthMultisampled2D -> TyTextureDepthMultisampled2D
    TyStorageTexture1D fmt acc -> TyStorageTexture1D fmt acc
    TyStorageTexture2D fmt acc -> TyStorageTexture2D fmt acc
    TyStorageTexture2DArray fmt acc -> TyStorageTexture2DArray fmt acc
    TyStorageTexture3D fmt acc -> TyStorageTexture3D fmt acc
    TyAtomic s -> TyAtomic s
    TyPtr addr access inner -> TyPtr addr access (rewriteType ctx inner)
    TySampler -> TySampler
    TySamplerComparison -> TySamplerComparison
    TyScalar s -> TyScalar s

rewriteParamType :: ModuleContext -> Param -> Param
rewriteParamType ctx param =
  param { paramType = rewriteType ctx (paramType param) }

rewriteFieldDecl :: ModuleContext -> FieldDecl -> FieldDecl
rewriteFieldDecl ctx field =
  field { fdType = rewriteType ctx (fdType field) }

rewriteStmtNames :: ModuleContext -> Stmt -> Stmt
rewriteStmtNames ctx stmt =
  case stmt of
    SLet name expr -> SLet name (rewriteExprNames ctx expr)
    SVar name expr -> SVar name (rewriteExprNames ctx expr)
    SAssign lv expr -> SAssign (rewriteLValueNames ctx lv) (rewriteExprNames ctx expr)
    SAssignOp lv op expr -> SAssignOp (rewriteLValueNames ctx lv) op (rewriteExprNames ctx expr)
    SInc lv -> SInc (rewriteLValueNames ctx lv)
    SDec lv -> SDec (rewriteLValueNames ctx lv)
    SExpr expr -> SExpr (rewriteExprNames ctx expr)
    SIf cond thenBody elseBody ->
      SIf (rewriteExprNames ctx cond)
          (map (rewriteStmtNames ctx) thenBody)
          (fmap (map (rewriteStmtNames ctx)) elseBody)
    SWhile cond body ->
      SWhile (rewriteExprNames ctx cond) (map (rewriteStmtNames ctx) body)
    SLoop body continuing ->
      SLoop (map (rewriteStmtNames ctx) body) (fmap (map (rewriteStmtNames ctx)) continuing)
    SFor initStmt condExpr contStmt body ->
      SFor (fmap (rewriteStmtNames ctx) initStmt)
           (fmap (rewriteExprNames ctx) condExpr)
           (fmap (rewriteStmtNames ctx) contStmt)
           (map (rewriteStmtNames ctx) body)
    SSwitch expr cases defBody ->
      SSwitch (rewriteExprNames ctx expr)
        (map rewriteCase cases)
        (fmap (map (rewriteStmtNames ctx)) defBody)
    SBreak -> SBreak
    SBreakIf cond -> SBreakIf (rewriteExprNames ctx cond)
    SContinue -> SContinue
    SDiscard -> SDiscard
    SFallthrough -> SFallthrough
    SReturn expr -> SReturn (fmap (rewriteExprNames ctx) expr)
  where
    rewriteCase sc =
      sc { scSelectors = map (rewriteExprNames ctx) (scSelectors sc)
         , scBody = map (rewriteStmtNames ctx) (scBody sc)
         }

rewriteLValueNames :: ModuleContext -> LValue -> LValue
rewriteLValueNames ctx lv =
  case lv of
    LVVar name -> LVVar (rewriteIdent ctx name)
    LVField base field -> LVField (rewriteLValueNames ctx base) field
    LVIndex base idx -> LVIndex (rewriteLValueNames ctx base) (rewriteExprNames ctx idx)
    LVDeref expr -> LVDeref (rewriteExprNames ctx expr)

rewriteExprNames :: ModuleContext -> Expr -> Expr
rewriteExprNames ctx expr =
  case expr of
    EVar name -> EVar (rewriteIdent ctx name)
    EInt _ -> expr
    EFloat _ -> expr
    EBool _ -> expr
    EBinary op a b -> EBinary op (rewriteExprNames ctx a) (rewriteExprNames ctx b)
    EUnary op a -> EUnary op (rewriteExprNames ctx a)
    ECall name args -> ECall (rewriteIdent ctx name) (map (rewriteExprNames ctx) args)
    EBitcast ty arg -> EBitcast (rewriteType ctx ty) (rewriteExprNames ctx arg)
    EField base field -> EField (rewriteExprNames ctx base) field
    EIndex base idx -> EIndex (rewriteExprNames ctx base) (rewriteExprNames ctx idx)

splitQName :: Text -> [Text]
splitQName name =
  let (headSeg, rest) = T.breakOn "::" name
  in if T.null rest
       then [headSeg]
       else headSeg : splitQName (T.drop 2 rest)

splitLast :: [a] -> Maybe ([a], a)
splitLast xs =
  case xs of
    [] -> Nothing
    [x] -> Just ([], x)
    (x:rest) -> do
      (prefix, lastVal) <- splitLast rest
      Just (x : prefix, lastVal)

builtinInputType :: Stage -> Text -> Maybe Type
builtinInputType stage name =
  case (stage, name) of
    (StageCompute, "global_invocation_id") -> Just (TyVector 3 U32)
    (StageCompute, "local_invocation_id") -> Just (TyVector 3 U32)
    (StageCompute, "workgroup_id") -> Just (TyVector 3 U32)
    (StageCompute, "local_invocation_index") -> Just (TyScalar U32)
    (StageCompute, "num_workgroups") -> Just (TyVector 3 U32)
    (StageVertex, "vertex_index") -> Just (TyScalar U32)
    (StageVertex, "instance_index") -> Just (TyScalar U32)
    (StageFragment, "position") -> Just (TyVector 4 F32)
    (StageFragment, "front_facing") -> Just (TyScalar Bool)
    (StageFragment, "sample_index") -> Just (TyScalar U32)
    _ -> Nothing

builtinOutputType :: Stage -> Text -> Maybe Type
builtinOutputType stage name =
  case (stage, name) of
    (StageVertex, "position") -> Just (TyVector 4 F32)
    (StageFragment, "frag_depth") -> Just (TyScalar F32)
    _ -> Nothing

builtinInputDecoration :: Stage -> Text -> Maybe Word32
builtinInputDecoration stage name =
  case (stage, name) of
    (StageCompute, "global_invocation_id") -> Just builtInGlobalInvocationId
    (StageCompute, "local_invocation_id") -> Just builtInLocalInvocationId
    (StageCompute, "workgroup_id") -> Just builtInWorkgroupId
    (StageCompute, "local_invocation_index") -> Just builtInLocalInvocationIndex
    (StageCompute, "num_workgroups") -> Just builtInNumWorkgroups
    (StageVertex, "vertex_index") -> Just builtInVertexIndex
    (StageVertex, "instance_index") -> Just builtInInstanceIndex
    (StageFragment, "position") -> Just builtInFragCoord
    (StageFragment, "front_facing") -> Just builtInFrontFacing
    (StageFragment, "sample_index") -> Just builtInSampleIndex
    _ -> Nothing

builtinOutputDecoration :: Stage -> Text -> Maybe Word32
builtinOutputDecoration stage name =
  case (stage, name) of
    (StageVertex, "position") -> Just builtInPosition
    (StageFragment, "frag_depth") -> Just builtInFragDepth
    _ -> Nothing

paramLocation :: [Attr] -> Maybe Word32
paramLocation = attrLocation

parseType :: [Token] -> Either CompileError (Type, [Token])
parseType toks = do
  (name, rest) <- parseFullIdent toks
  if "::" `T.isInfixOf` name
    then Right (TyStructRef name, rest)
    else
      case name of
        _ | name `elem` ["i32", "u32", "f16", "f32", "bool"] ->
              Right (TyScalar (parseScalar name), rest)
        "sampler" ->
              Right (TySampler, rest)
        "sampler_comparison" ->
              Right (TySamplerComparison, rest)
        "ref" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              Right (TyPtr "function" Nothing inner, rest3)
        "ptr" -> do
              rest1 <- expectSymbol "<" rest
              (addrName, rest2) <- parseIdent rest1
              rest3 <- expectSymbol "," rest2
              (inner, rest4) <- parseType rest3
              (access, rest5) <- parsePtrAccessMaybe rest4
              rest6 <- expectSymbol ">" rest5
              Right (TyPtr addrName access inner, rest6)
        "atomic" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar ->
                  case scalar of
                    I32 -> Right (TyAtomic scalar, rest3)
                    U32 -> Right (TyAtomic scalar, rest3)
                    _ -> Left (errorAt rest "atomic element must be i32 or u32")
                _ -> Left (errorAt rest "atomic element must be i32 or u32")
        "texture_1d" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture1D scalar, rest3)
                _ -> Left (errorAt rest "texture_1d element must be a scalar")
        "texture_1d_array" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture1DArray scalar, rest3)
                _ -> Left (errorAt rest "texture_1d_array element must be a scalar")
        "texture_2d" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture2D scalar, rest3)
                _ -> Left (errorAt rest "texture_2d element must be a scalar")
        "texture_2d_array" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture2DArray scalar, rest3)
                _ -> Left (errorAt rest "texture_2d_array element must be a scalar")
        "texture_3d" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture3D scalar, rest3)
                _ -> Left (errorAt rest "texture_3d element must be a scalar")
        "texture_cube" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTextureCube scalar, rest3)
                _ -> Left (errorAt rest "texture_cube element must be a scalar")
        "texture_cube_array" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTextureCubeArray scalar, rest3)
                _ -> Left (errorAt rest "texture_cube_array element must be a scalar")
        "texture_multisampled_2d" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTextureMultisampled2D scalar, rest3)
                _ -> Left (errorAt rest "texture_multisampled_2d element must be a scalar")
        "texture_depth_2d" ->
              Right (TyTextureDepth2D, rest)
        "texture_depth_2d_array" ->
              Right (TyTextureDepth2DArray, rest)
        "texture_depth_cube" ->
              Right (TyTextureDepthCube, rest)
        "texture_depth_cube_array" ->
              Right (TyTextureDepthCubeArray, rest)
        "texture_depth_multisampled_2d" ->
              Right (TyTextureDepthMultisampled2D, rest)
        "texture_storage_1d" -> do
              rest1 <- expectSymbol "<" rest
              (fmt, rest2) <- parseStorageFormat rest1
              rest3 <- expectSymbol "," rest2
              (access, rest4) <- parseStorageAccess rest3
              rest5 <- expectSymbol ">" rest4
              Right (TyStorageTexture1D fmt access, rest5)
        "texture_storage_2d" -> do
              rest1 <- expectSymbol "<" rest
              (fmt, rest2) <- parseStorageFormat rest1
              rest3 <- expectSymbol "," rest2
              (access, rest4) <- parseStorageAccess rest3
              rest5 <- expectSymbol ">" rest4
              Right (TyStorageTexture2D fmt access, rest5)
        "texture_storage_2d_array" -> do
              rest1 <- expectSymbol "<" rest
              (fmt, rest2) <- parseStorageFormat rest1
              rest3 <- expectSymbol "," rest2
              (access, rest4) <- parseStorageAccess rest3
              rest5 <- expectSymbol ">" rest4
              Right (TyStorageTexture2DArray fmt access, rest5)
        "texture_storage_3d" -> do
              rest1 <- expectSymbol "<" rest
              (fmt, rest2) <- parseStorageFormat rest1
              rest3 <- expectSymbol "," rest2
              (access, rest4) <- parseStorageAccess rest3
              rest5 <- expectSymbol ">" rest4
              Right (TyStorageTexture3D fmt access, rest5)
        _ | Just (cols, rows) <- parseMatrixName name -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyMatrix cols rows scalar, rest3)
                _ -> Left (errorAt rest "matrix element must be a scalar")
        _ | name `elem` ["vec2", "vec3", "vec4"] -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyVector (vecSize name) scalar, rest3)
                _ -> Left (errorAt rest "vector element must be a scalar")
        "array" -> do
              rest1 <- expectSymbol "<" rest
              (elemTy, rest2) <- parseType rest1
              case rest2 of
                (Token (TkSymbol ",") _ : Token (TkInt n) _ : rest3) -> do
                  rest4 <- expectSymbol ">" rest3
                  Right (TyArray elemTy (Just (fromIntegral n)), rest4)
                _ -> do
                  rest3 <- expectSymbol ">" rest2
                  Right (TyArray elemTy Nothing, rest3)
        _ -> Right (TyStructRef name, rest)

parseFullIdent :: [Token] -> Either CompileError (Text, [Token])
parseFullIdent toks = do
  (name, rest) <- parseIdent toks
  go name rest
  where
    go acc rest =
      case rest of
        (Token (TkSymbol "::") _ : more) -> do
          (next, rest1) <- parseIdent more
          go (acc <> "::" <> next) rest1
        _ -> Right (acc, rest)

parseIdent :: [Token] -> Either CompileError (Text, [Token])
parseIdent toks =
  case toks of
    (Token (TkIdent name) _ : rest) -> Right (name, rest)
    _ -> Left (errorAt toks "expected identifier")

expectSymbol :: Text -> [Token] -> Either CompileError [Token]
expectSymbol sym toks =
  case toks of
    (Token (TkSymbol s) _ : rest) | s == sym -> Right rest
    _ -> Left (errorAt toks ("expected symbol '" <> textToString sym <> "'"))

parseScalar :: Text -> Scalar
parseScalar name = case name of
  "i32" -> I32
  "u32" -> U32
  "f16" -> F16
  "f32" -> F32
  "bool" -> Bool
  _ -> I32

parseStorageFormat :: [Token] -> Either CompileError (StorageFormat, [Token])
parseStorageFormat toks =
  case toks of
    (Token (TkIdent name) _ : rest) ->
      case name of
        "rgba8unorm" -> Right (FormatRgba8Unorm, rest)
        "rgba8snorm" -> Right (FormatRgba8Snorm, rest)
        "rgba8uint" -> Right (FormatRgba8Uint, rest)
        "rgba8sint" -> Right (FormatRgba8Sint, rest)
        "rgba16float" -> Right (FormatRgba16Float, rest)
        "rgba16uint" -> Right (FormatRgba16Uint, rest)
        "rgba16sint" -> Right (FormatRgba16Sint, rest)
        "rgba16unorm" -> Right (FormatRgba16Unorm, rest)
        "rgba16snorm" -> Right (FormatRgba16Snorm, rest)
        "rgba32float" -> Right (FormatRgba32Float, rest)
        "rgba32uint" -> Right (FormatRgba32Uint, rest)
        "rgba32sint" -> Right (FormatRgba32Sint, rest)
        "r32float" -> Right (FormatR32Float, rest)
        "r32uint" -> Right (FormatR32Uint, rest)
        "r32sint" -> Right (FormatR32Sint, rest)
        "r16float" -> Right (FormatR16Float, rest)
        "r16uint" -> Right (FormatR16Uint, rest)
        "r16sint" -> Right (FormatR16Sint, rest)
        "r16unorm" -> Right (FormatR16Unorm, rest)
        "r16snorm" -> Right (FormatR16Snorm, rest)
        "r8unorm" -> Right (FormatR8Unorm, rest)
        "r8snorm" -> Right (FormatR8Snorm, rest)
        "r8uint" -> Right (FormatR8Uint, rest)
        "r8sint" -> Right (FormatR8Sint, rest)
        "rg32float" -> Right (FormatRg32Float, rest)
        "rg32uint" -> Right (FormatRg32Uint, rest)
        "rg32sint" -> Right (FormatRg32Sint, rest)
        "rg16float" -> Right (FormatRg16Float, rest)
        "rg16uint" -> Right (FormatRg16Uint, rest)
        "rg16sint" -> Right (FormatRg16Sint, rest)
        "rg16unorm" -> Right (FormatRg16Unorm, rest)
        "rg16snorm" -> Right (FormatRg16Snorm, rest)
        "rg8unorm" -> Right (FormatRg8Unorm, rest)
        "rg8snorm" -> Right (FormatRg8Snorm, rest)
        "rg8uint" -> Right (FormatRg8Uint, rest)
        "rg8sint" -> Right (FormatRg8Sint, rest)
        "rgb10a2unorm" -> Right (FormatRgb10a2Unorm, rest)
        "rgb10a2uint" -> Right (FormatRgb10a2Uint, rest)
        "rg11b10float" -> Right (FormatRg11b10Float, rest)
        _ -> Left (errorAt toks "unknown storage texture format")
    _ -> Left (errorAt toks "expected storage texture format")

parseStorageAccess :: [Token] -> Either CompileError (StorageAccess, [Token])
parseStorageAccess toks =
  case toks of
    (Token (TkIdent name) _ : rest) ->
      case name of
        "read" -> Right (StorageRead, rest)
        "write" -> Right (StorageWrite, rest)
        "read_write" -> Right (StorageReadWrite, rest)
        _ -> Left (errorAt toks "unknown storage texture access")
    _ -> Left (errorAt toks "expected storage texture access")

parsePtrAccessMaybe :: [Token] -> Either CompileError (Maybe StorageAccess, [Token])
parsePtrAccessMaybe toks =
  case toks of
    (Token (TkSymbol ",") _ : Token (TkIdent access) _ : rest) ->
      case access of
        "read" -> Right (Just StorageRead, rest)
        "write" -> Right (Just StorageWrite, rest)
        "read_write" -> Right (Just StorageReadWrite, rest)
        _ -> Left (errorAt toks "unknown pointer access")
    _ -> Right (Nothing, toks)

storageFormatScalar :: StorageFormat -> Scalar
storageFormatScalar fmt =
  case fmt of
    FormatRgba8Unorm -> F32
    FormatRgba8Snorm -> F32
    FormatRgba8Uint -> U32
    FormatRgba8Sint -> I32
    FormatRgba16Float -> F32
    FormatRgba16Uint -> U32
    FormatRgba16Sint -> I32
    FormatRgba16Unorm -> F32
    FormatRgba16Snorm -> F32
    FormatRgba32Float -> F32
    FormatRgba32Uint -> U32
    FormatRgba32Sint -> I32
    FormatR32Float -> F32
    FormatR32Uint -> U32
    FormatR32Sint -> I32
    FormatR16Float -> F32
    FormatR16Uint -> U32
    FormatR16Sint -> I32
    FormatR16Unorm -> F32
    FormatR16Snorm -> F32
    FormatR8Unorm -> F32
    FormatR8Snorm -> F32
    FormatR8Uint -> U32
    FormatR8Sint -> I32
    FormatRg32Float -> F32
    FormatRg32Uint -> U32
    FormatRg32Sint -> I32
    FormatRg16Float -> F32
    FormatRg16Uint -> U32
    FormatRg16Sint -> I32
    FormatRg16Unorm -> F32
    FormatRg16Snorm -> F32
    FormatRg8Unorm -> F32
    FormatRg8Snorm -> F32
    FormatRg8Uint -> U32
    FormatRg8Sint -> I32
    FormatRgb10a2Unorm -> F32
    FormatRgb10a2Uint -> U32
    FormatRg11b10Float -> F32

storageFormatToImageFormat :: StorageFormat -> Word32
storageFormatToImageFormat fmt =
  case fmt of
    FormatRgba8Unorm -> imageFormatRgba8
    FormatRgba8Snorm -> imageFormatRgba8Snorm
    FormatRgba8Uint -> imageFormatRgba8ui
    FormatRgba8Sint -> imageFormatRgba8i
    FormatRgba16Float -> imageFormatRgba16f
    FormatRgba16Uint -> imageFormatRgba16ui
    FormatRgba16Sint -> imageFormatRgba16i
    FormatRgba16Unorm -> imageFormatRgba16
    FormatRgba16Snorm -> imageFormatRgba16Snorm
    FormatRgba32Float -> imageFormatRgba32f
    FormatRgba32Uint -> imageFormatRgba32ui
    FormatRgba32Sint -> imageFormatRgba32i
    FormatR32Float -> imageFormatR32f
    FormatR32Uint -> imageFormatR32ui
    FormatR32Sint -> imageFormatR32i
    FormatR16Float -> imageFormatR16f
    FormatR16Uint -> imageFormatR16ui
    FormatR16Sint -> imageFormatR16i
    FormatR16Unorm -> imageFormatR16
    FormatR16Snorm -> imageFormatR16Snorm
    FormatR8Unorm -> imageFormatR8
    FormatR8Snorm -> imageFormatR8Snorm
    FormatR8Uint -> imageFormatR8ui
    FormatR8Sint -> imageFormatR8i
    FormatRg32Float -> imageFormatRg32f
    FormatRg32Uint -> imageFormatRg32ui
    FormatRg32Sint -> imageFormatRg32i
    FormatRg16Float -> imageFormatRg16f
    FormatRg16Uint -> imageFormatRg16ui
    FormatRg16Sint -> imageFormatRg16i
    FormatRg16Unorm -> imageFormatRg16
    FormatRg16Snorm -> imageFormatRg16Snorm
    FormatRg8Unorm -> imageFormatRg8
    FormatRg8Snorm -> imageFormatRg8Snorm
    FormatRg8Uint -> imageFormatRg8ui
    FormatRg8Sint -> imageFormatRg8i
    FormatRgb10a2Unorm -> imageFormatRgb10A2
    FormatRgb10a2Uint -> imageFormatRgb10a2ui
    FormatRg11b10Float -> imageFormatR11fG11fB10f

toBindingKind :: Text -> Maybe Text -> Type -> Either CompileError BindingKind
toBindingKind addrSpace access ty =
  case addrSpace of
    "uniform" -> Right BUniform
    "storage" ->
      case ty of
        TyStorageTexture1D _ _ -> Right BStorageTexture1D
        TyStorageTexture2D _ _ -> Right BStorageTexture2D
        TyStorageTexture2DArray _ _ -> Right BStorageTexture2DArray
        TyStorageTexture3D _ _ -> Right BStorageTexture3D
        _ -> case access of
          Just "read" -> Right BStorageRead
          Just "read_write" -> Right BStorageReadWrite
          Nothing -> Right BStorageRead
          Just other -> Left (CompileError ("unsupported storage access: " <> textToString other) Nothing Nothing)
    "sampler" -> Right BSampler
    "sampler_comparison" -> Right BSamplerComparison
    "texture" ->
      case ty of
        TyTexture1D _ -> Right BTexture1D
        TyTexture1DArray _ -> Right BTexture1DArray
        TyTexture2D _ -> Right BTexture2D
        TyTexture2DArray _ -> Right BTexture2DArray
        TyTexture3D _ -> Right BTexture3D
        TyTextureCube _ -> Right BTextureCube
        TyTextureCubeArray _ -> Right BTextureCubeArray
        TyTextureMultisampled2D _ -> Right BTextureMultisampled2D
        TyTextureDepth2D -> Right BTextureDepth2D
        TyTextureDepth2DArray -> Right BTextureDepth2DArray
        TyTextureDepthCube -> Right BTextureDepthCube
        TyTextureDepthCubeArray -> Right BTextureDepthCubeArray
        TyTextureDepthMultisampled2D -> Right BTextureDepthMultisampled2D
        _ -> Left (CompileError "texture address space requires a texture type" Nothing Nothing)
    other -> Left (CompileError ("unsupported address space: " <> textToString other) Nothing Nothing)

bindingKindFromType :: Type -> Either CompileError BindingKind
bindingKindFromType ty =
  case ty of
    TySampler -> Right BSampler
    TySamplerComparison -> Right BSamplerComparison
    TyTexture1D _ -> Right BTexture1D
    TyTexture1DArray _ -> Right BTexture1DArray
    TyTexture2D _ -> Right BTexture2D
    TyTexture2DArray _ -> Right BTexture2DArray
    TyTexture3D _ -> Right BTexture3D
    TyTextureCube _ -> Right BTextureCube
    TyTextureCubeArray _ -> Right BTextureCubeArray
    TyTextureMultisampled2D _ -> Right BTextureMultisampled2D
    TyTextureDepth2D -> Right BTextureDepth2D
    TyTextureDepth2DArray -> Right BTextureDepth2DArray
    TyTextureDepthCube -> Right BTextureDepthCube
    TyTextureDepthCubeArray -> Right BTextureDepthCubeArray
    TyTextureDepthMultisampled2D -> Right BTextureDepthMultisampled2D
    TyStorageTexture1D _ _ -> Right BStorageTexture1D
    TyStorageTexture2D _ _ -> Right BStorageTexture2D
    TyStorageTexture2DArray _ _ -> Right BStorageTexture2DArray
    TyStorageTexture3D _ _ -> Right BStorageTexture3D
    _ -> Left (CompileError "bindings without address space must be sampler or texture types" Nothing Nothing)

vecSize :: Text -> Int
vecSize name = case T.unpack name of
  "vec2" -> 2
  "vec3" -> 3
  "vec4" -> 4
  _ -> 4

parseMatrixName :: Text -> Maybe (Int, Int)
parseMatrixName name =
  case T.unpack name of
    "mat2" -> Just (2, 2)
    "mat3" -> Just (3, 3)
    "mat4" -> Just (4, 4)
    _ ->
      case T.unpack name of
        'm':'a':'t':rest ->
          let (a, rest1) = span isDigit rest
          in case rest1 of
               ('x':rest2) ->
                 let (b, rest3) = span isDigit rest2
                 in if null a || null b || not (null rest3)
                      then Nothing
                      else Just (read a, read b)
               _ -> Nothing
        _ -> Nothing

bindingNumbers :: [Attr] -> Either CompileError (Word32, Word32)
bindingNumbers attrs =
  let grp = attrInt "group" attrs
      bind = attrInt "binding" attrs
  in case (grp, bind) of
       (Just g, Just b) -> Right (fromIntegral g, fromIntegral b)
       _ -> Left (CompileError "@group and @binding are required for bindings" Nothing Nothing)

entryAttributesMaybe :: [Attr] -> Maybe (Stage, Maybe (Word32, Word32, Word32))
entryAttributesMaybe attrs =
  let isCompute = any (hasAttr "compute") attrs
      isFragment = any (hasAttr "fragment") attrs
      isVertex = any (hasAttr "vertex") attrs
      wg = attrInts "workgroup_size" attrs
  in case (isCompute, isFragment, isVertex) of
       (True, True, _) -> Nothing
       (True, _, True) -> Nothing
       (_, True, True) -> Nothing
       (True, False, False) ->
         case wg of
           Just [x] -> Just (StageCompute, Just (fromIntegral x, 1, 1))
           Just [x, y] -> Just (StageCompute, Just (fromIntegral x, fromIntegral y, 1))
           Just [x, y, z] -> Just (StageCompute, Just (fromIntegral x, fromIntegral y, fromIntegral z))
           _ -> Nothing
       (False, True, False) ->
         case wg of
           Nothing -> Just (StageFragment, Nothing)
           Just _ -> Nothing
       (False, False, True) ->
         case wg of
           Nothing -> Just (StageVertex, Nothing)
           Just _ -> Nothing
       (False, False, False) -> Nothing
  where
    hasAttr target attr =
      case attr of
        Attr name _ -> name == target
        AttrIf _ -> False

paramBuiltin :: [Attr] -> Maybe Text
paramBuiltin = attrBuiltin

errorAt :: [Token] -> String -> CompileError
errorAt toks msg =
  case toks of
    (Token _ (SrcPos l c) : _) -> CompileError msg (Just l) (Just c)
    [] -> CompileError msg Nothing Nothing

errorAtPos :: SrcPos -> String -> CompileError
errorAtPos (SrcPos l c) msg = CompileError msg (Just l) (Just c)

withPos :: SrcPos -> Either CompileError a -> Either CompileError a
withPos pos result =
  case result of
    Left err ->
      let err' =
            case (ceLine err, ceColumn err) of
              (Nothing, Nothing) -> err { ceLine = Just (spLine pos), ceColumn = Just (spCol pos) }
              _ -> err
      in Left err'
    Right val -> Right val

renderError :: CompileError -> String
renderError err =
  let loc = case (ceLine err, ceColumn err) of
        (Just l, Just c) -> " at " <> show l <> ":" <> show c
        _ -> ""
  in ceMessage err <> loc

textToString :: Text -> String
textToString = T.unpack

-- Attributes

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

attrInt :: Text -> [Attr] -> Maybe Integer
attrInt name attrs =
  case [v | Attr n args <- attrs, n == name, AttrInt v <- args] of
    (x:_) -> Just x
    _ -> Nothing

attrIntMaybe :: Text -> [Attr] -> Maybe Word32
attrIntMaybe name attrs = fmap fromIntegral (attrInt name attrs)

attrLocation :: [Attr] -> Maybe Word32
attrLocation = attrIntMaybe "location"

attrBuiltin :: [Attr] -> Maybe Text
attrBuiltin attrs =
  case [name | Attr n args <- attrs, n == "builtin", AttrIdent name <- args] of
    (x:_) -> Just x
    _ -> Nothing

attrInts :: Text -> [Attr] -> Maybe [Integer]
attrInts name attrs =
  case [nums | Attr n args <- attrs, n == name, let nums = [v | AttrInt v <- args], not (null nums)] of
    (x:_) -> Just x
    _ -> Nothing

-- Interface and layout

buildInterface :: OverrideSpecMode -> ModuleAst -> Either CompileError ShaderInterface
buildInterface specMode modAst = do
  let structEnv = [(sdName s, s) | s <- modStructs modAst]
  bindings <- mapM (layoutBinding structEnv) (modBindings modAst)
  overrides <- buildOverrideInfo specMode structEnv (modOverrides modAst)
  pure (ShaderInterface bindings overrides)

buildOverrideInfo :: OverrideSpecMode -> [(Text, StructDecl)] -> [OverrideDecl] -> Either CompileError [OverrideInfo]
buildOverrideInfo specMode structEnv decls = do
  specIds <- assignOverrideSpecIds decls
  let depsMap = overrideDependencies decls
  _ <- topoSortOverrides (map odName decls) depsMap
  mapM (layoutOverride specMode specIds depsMap structEnv) decls

layoutOverride :: OverrideSpecMode -> Map.Map Text Word32 -> Map.Map Text (Set.Set Text) -> [(Text, StructDecl)] -> OverrideDecl -> Either CompileError OverrideInfo
layoutOverride specMode specIds depsMap structEnv decl = do
  layout <- resolveTypeLayout structEnv [] (odType decl)
  let deps = Map.findWithDefault Set.empty (odName decl) depsMap
  specId <-
    case specMode of
      SpecParity ->
        case Map.lookup (odName decl) specIds of
          Just sid -> Right (Just sid)
          Nothing -> Left (CompileError "missing specialization id for override" Nothing Nothing)
      SpecStrict ->
        if Set.null deps
          then case Map.lookup (odName decl) specIds of
            Just sid -> Right (Just sid)
            Nothing -> Left (CompileError "missing specialization id for override" Nothing Nothing)
          else Right Nothing
  pure (OverrideInfo (textToString (odName decl)) (odId decl) specId layout)

assignOverrideSpecIds :: [OverrideDecl] -> Either CompileError (Map.Map Text Word32)
assignOverrideSpecIds overrides = do
  let explicit = [(odName d, i) | d <- overrides, Just i <- [odId d]]
  let ids = map snd explicit
  let (dups, _) = foldl' collect ([], Set.empty) ids
  when (not (null dups)) $
    Left (CompileError ("duplicate override ids: " <> intercalate ", " (map show dups)) Nothing Nothing)
  let used0 = Set.fromList ids
  let acc0 = Map.fromList explicit
  (acc, _, _) <- foldM assign (acc0, used0, 0) overrides
  Right acc
  where
    collect (acc, seen) n =
      if Set.member n seen
        then (n : acc, seen)
        else (acc, Set.insert n seen)

    assign (acc, used, next) decl =
      case odId decl of
        Just _ -> Right (acc, used, next)
        Nothing -> do
          (specId, used', next') <- nextSpecId used next
          Right (Map.insert (odName decl) specId acc, used', next')

    nextSpecId used next =
      if Set.member next used
        then
          if next == maxBound
            then Left (CompileError "ran out of override specialization ids" Nothing Nothing)
          else nextSpecId used (next + 1)
        else Right (next, Set.insert next used, next + 1)

overrideDependencies :: [OverrideDecl] -> Map.Map Text (Set.Set Text)
overrideDependencies decls =
  let names = Set.fromList (map odName decls)
  in Map.fromList [(odName d, maybe Set.empty (collectOverrideRefs names) (odExpr d)) | d <- decls]

collectOverrideRefs :: Set.Set Text -> Expr -> Set.Set Text
collectOverrideRefs names expr =
  case expr of
    EVar n | Set.member n names -> Set.singleton n
    EUnary _ e -> collectOverrideRefs names e
    EBinary _ a b -> collectOverrideRefs names a <> collectOverrideRefs names b
    ECall _ args -> foldMap (collectOverrideRefs names) args
    EBitcast _ e -> collectOverrideRefs names e
    EField e _ -> collectOverrideRefs names e
    EIndex a b -> collectOverrideRefs names a <> collectOverrideRefs names b
    _ -> Set.empty

topoSortOverrides :: [Text] -> Map.Map Text (Set.Set Text) -> Either CompileError [Text]
topoSortOverrides order depsMap = go Set.empty Set.empty [] order
  where
    go _ _ acc [] = Right (reverse acc)
    go temp perm acc (n:ns)
      | Set.member n perm = go temp perm acc ns
      | Set.member n temp =
          Left (CompileError ("override dependency cycle involving " <> textToString n) Nothing Nothing)
      | otherwise = do
          (perm', acc') <- visit temp (perm, acc) n
          go temp perm' acc' ns

    visit temp (perm, acc) n
      | Set.member n perm = Right (perm, acc)
      | Set.member n temp =
          Left (CompileError ("override dependency cycle involving " <> textToString n) Nothing Nothing)
      | otherwise = do
          let temp' = Set.insert n temp
          let deps = Set.toList (Map.findWithDefault Set.empty n depsMap)
          (perm', acc') <- foldM (visit temp') (perm, acc) deps
          let perm'' = Set.insert n perm'
          Right (perm'', n : acc')

layoutBinding :: [(Text, StructDecl)] -> BindingDecl -> Either CompileError BindingInfo
layoutBinding env decl = do
  case bdKind decl of
    BUniform -> ensureStructBinding
    BStorageRead -> ensureStructBinding
    BStorageReadWrite -> ensureStructBinding
    BSampler ->
      case bdType decl of
        TySampler -> pure ()
        _ -> Left (CompileError "sampler bindings must use type sampler" Nothing Nothing)
    BSamplerComparison ->
      case bdType decl of
        TySamplerComparison -> pure ()
        _ -> Left (CompileError "sampler_comparison bindings must use type sampler_comparison" Nothing Nothing)
    BTexture1D ->
      case bdType decl of
        TyTexture1D _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_1d<scalar>" Nothing Nothing)
    BTexture1DArray ->
      case bdType decl of
        TyTexture1DArray _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_1d_array<scalar>" Nothing Nothing)
    BTexture2D ->
      case bdType decl of
        TyTexture2D _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_2d<scalar>" Nothing Nothing)
    BTexture2DArray ->
      case bdType decl of
        TyTexture2DArray _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_2d_array<scalar>" Nothing Nothing)
    BTexture3D ->
      case bdType decl of
        TyTexture3D _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_3d<scalar>" Nothing Nothing)
    BTextureCube ->
      case bdType decl of
        TyTextureCube _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_cube<scalar>" Nothing Nothing)
    BTextureCubeArray ->
      case bdType decl of
        TyTextureCubeArray _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_cube_array<scalar>" Nothing Nothing)
    BTextureMultisampled2D ->
      case bdType decl of
        TyTextureMultisampled2D _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_multisampled_2d<scalar>" Nothing Nothing)
    BTextureDepth2D ->
      case bdType decl of
        TyTextureDepth2D -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_2d" Nothing Nothing)
    BTextureDepth2DArray ->
      case bdType decl of
        TyTextureDepth2DArray -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_2d_array" Nothing Nothing)
    BTextureDepthCube ->
      case bdType decl of
        TyTextureDepthCube -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_cube" Nothing Nothing)
    BTextureDepthCubeArray ->
      case bdType decl of
        TyTextureDepthCubeArray -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_cube_array" Nothing Nothing)
    BTextureDepthMultisampled2D ->
      case bdType decl of
        TyTextureDepthMultisampled2D -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_multisampled_2d" Nothing Nothing)
    BStorageTexture1D ->
      case bdType decl of
        TyStorageTexture1D _ _ -> pure ()
        _ -> Left (CompileError "storage texture bindings must use type texture_storage_1d<format, access>" Nothing Nothing)
    BStorageTexture2D ->
      case bdType decl of
        TyStorageTexture2D _ _ -> pure ()
        _ -> Left (CompileError "storage texture bindings must use type texture_storage_2d<format, access>" Nothing Nothing)
    BStorageTexture2DArray ->
      case bdType decl of
        TyStorageTexture2DArray _ _ -> pure ()
        _ -> Left (CompileError "storage texture bindings must use type texture_storage_2d_array<format, access>" Nothing Nothing)
    BStorageTexture3D ->
      case bdType decl of
        TyStorageTexture3D _ _ -> pure ()
        _ -> Left (CompileError "storage texture bindings must use type texture_storage_3d<format, access>" Nothing Nothing)
  tyLayout <- resolveTypeLayout env [] (bdType decl)
  when (containsPointer tyLayout) $
    Left (CompileError "bindings cannot contain pointer types" Nothing Nothing)
  case bdKind decl of
    BUniform ->
      if containsAtomic tyLayout
        then Left (CompileError "uniform bindings cannot contain atomic types" Nothing Nothing)
        else if containsRuntimeArray tyLayout
          then Left (CompileError "uniform bindings cannot contain runtime arrays" Nothing Nothing)
          else pure ()
    _ -> pure ()
  pure (BindingInfo (textToString (bdName decl)) (bdKind decl) (bdGroup decl) (bdBinding decl) tyLayout)
  where
    ensureStructBinding =
      case bdType decl of
        TyStructRef _ -> pure ()
        _ -> Left (CompileError "bindings must use a struct type (wrap arrays in a struct)" Nothing Nothing)

resolveTypeLayout :: [(Text, StructDecl)] -> [Text] -> Type -> Either CompileError TypeLayout
resolveTypeLayout env stack ty =
  case ty of
    TyScalar s ->
      let (a, sz) = scalarLayout s
      in Right (TLScalar s a sz)
    TyVector n s ->
      let (a, sz) = vectorLayout s n
      in Right (TLVector n s a sz)
    TyMatrix cols rows s ->
      let (a, sz) = vectorLayout s rows
          stride = roundUp sz a
          total = stride * fromIntegral cols
      in Right (TLMatrix cols rows s a total stride)
    TyArray elemTy mlen -> do
      elemLayout <- resolveTypeLayout env stack elemTy
      let elemAlign = layoutAlign elemLayout
      let elemSize = layoutSize elemLayout
      let stride = roundUp elemSize elemAlign
      let total = case mlen of
            Nothing -> stride
            Just n -> stride * fromIntegral n
      Right (TLArray mlen stride elemLayout elemAlign total)
    TySampler -> Right TLSampler
    TySamplerComparison -> Right TLSamplerComparison
    TyTexture1D s -> Right (TLTexture1D s)
    TyTexture1DArray s -> Right (TLTexture1DArray s)
    TyTexture2D s -> Right (TLTexture2D s)
    TyTexture2DArray s -> Right (TLTexture2DArray s)
    TyTexture3D s -> Right (TLTexture3D s)
    TyTextureCube s -> Right (TLTextureCube s)
    TyTextureCubeArray s -> Right (TLTextureCubeArray s)
    TyTextureMultisampled2D s -> Right (TLTextureMultisampled2D s)
    TyTextureDepth2D -> Right TLTextureDepth2D
    TyTextureDepth2DArray -> Right TLTextureDepth2DArray
    TyTextureDepthCube -> Right TLTextureDepthCube
    TyTextureDepthCubeArray -> Right TLTextureDepthCubeArray
    TyTextureDepthMultisampled2D -> Right TLTextureDepthMultisampled2D
    TyStorageTexture1D fmt access -> Right (TLStorageTexture1D fmt access)
    TyStorageTexture2D fmt access -> Right (TLStorageTexture2D fmt access)
    TyStorageTexture2DArray fmt access -> Right (TLStorageTexture2DArray fmt access)
    TyStorageTexture3D fmt access -> Right (TLStorageTexture3D fmt access)
    TyAtomic s -> Right (TLAtomic s)
    TyPtr addr access elemTy -> do
      elemLayout <- resolveTypeLayout env stack elemTy
      when (containsResource elemLayout) $
        Left (CompileError "pointer element types cannot be resources" Nothing Nothing)
      storageClass <- case addr of
        "function" -> Right storageClassFunction
        "private" -> Right storageClassPrivate
        "workgroup" -> Right storageClassWorkgroup
        "uniform" -> Right storageClassUniform
        "storage" -> Right storageClassStorageBuffer
        _ -> Left (CompileError ("unsupported pointer address space: " <> textToString addr) Nothing Nothing)
      access' <- case addr of
        "storage" ->
          case access of
            Nothing -> Right (Just StorageRead)
            Just a -> Right (Just a)
        "uniform" ->
          case access of
            Nothing -> Right (Just StorageRead)
            Just StorageRead -> Right (Just StorageRead)
            _ -> Left (CompileError "uniform pointers must be read-only" Nothing Nothing)
        "function" ->
          if access == Nothing then Right Nothing else Left (CompileError "function pointers cannot specify access" Nothing Nothing)
        "private" ->
          if access == Nothing then Right Nothing else Left (CompileError "private pointers cannot specify access" Nothing Nothing)
        "workgroup" ->
          if access == Nothing then Right Nothing else Left (CompileError "workgroup pointers cannot specify access" Nothing Nothing)
        _ -> Right access
      Right (TLPointer storageClass access' elemLayout)
    TyStructRef name ->
      if name `elem` stack
        then Left (CompileError ("recursive struct: " <> textToString name) Nothing Nothing)
        else case lookup name env of
          Nothing -> Left (CompileError ("unknown struct: " <> textToString name) Nothing Nothing)
          Just decl -> do
            let stack' = name : stack
            fields <- resolveFields env stack' (sdFields decl)
            let align = maximum (1 : map flAlign fields)
            let size = structSize fields align
            Right (TLStruct (textToString name) fields align size)

resolveFields :: [(Text, StructDecl)] -> [Text] -> [FieldDecl] -> Either CompileError [FieldLayout]
resolveFields env stack fields =
  let go _ acc [] = Right (reverse acc)
      go offset acc (FieldDecl name fty attrs : rest) = do
        fLayout <- resolveTypeLayout env stack fty
        if containsResource fLayout
          then Left (CompileError "resource types are not allowed in struct fields" Nothing Nothing)
          else if containsPointer fLayout
            then Left (CompileError "pointer types are not allowed in struct fields" Nothing Nothing)
          else do
            alignAttr <- parseFieldAlign attrs
            sizeAttr <- parseFieldSize attrs
            let baseAlign = layoutAlign fLayout
            let baseSize = layoutSize fLayout
            let requestedAlign = fromMaybe baseAlign alignAttr
            when (requestedAlign < baseAlign) $
              Left (CompileError "@align must be at least the natural alignment" Nothing Nothing)
            let fieldAlign = max baseAlign requestedAlign
            fieldSize <- case sizeAttr of
              Nothing -> Right baseSize
              Just sz ->
                if sz < baseSize
                  then Left (CompileError "field @size must be at least the natural size" Nothing Nothing)
                  else if sz `mod` fieldAlign /= 0
                    then Left (CompileError "field @size must be a multiple of its alignment" Nothing Nothing)
                  else Right sz
            let aligned = roundUp offset fieldAlign
            let entry = FieldLayout (textToString name) aligned fLayout fieldAlign fieldSize
            let offset' = aligned + fieldSize
            go offset' (entry:acc) rest
  in go 0 [] fields

parseFieldAlign :: [Attr] -> Either CompileError (Maybe Word32)
parseFieldAlign attrs = do
  mval <- attrSingleInt "align" attrs
  case mval of
    Nothing -> Right Nothing
    Just v ->
      if isPowerOfTwo v
        then Right (Just v)
        else Left (CompileError "@align must be a power of two" Nothing Nothing)

parseFieldSize :: [Attr] -> Either CompileError (Maybe Word32)
parseFieldSize attrs = attrSingleInt "size" attrs

attrSingleInt :: Text -> [Attr] -> Either CompileError (Maybe Word32)
attrSingleInt name attrs =
  case [args | Attr n args <- attrs, n == name] of
    [] -> Right Nothing
    [args] ->
      case args of
        [AttrInt v]
          | v <= 0 -> Left (CompileError ("@" <> textToString name <> " must be positive") Nothing Nothing)
          | v > fromIntegral (maxBound :: Word32) -> Left (CompileError ("@" <> textToString name <> " is out of range") Nothing Nothing)
          | otherwise -> Right (Just (fromIntegral v))
        _ -> Left (CompileError ("@" <> textToString name <> " expects a single integer argument") Nothing Nothing)
    _ -> Left (CompileError ("duplicate @" <> textToString name <> " attribute") Nothing Nothing)

isPowerOfTwo :: Word32 -> Bool
isPowerOfTwo v =
  v /= 0 && (v .&. (v - 1)) == 0

containsResource :: TypeLayout -> Bool
containsResource layout =
  case layout of
    TLSampler -> True
    TLSamplerComparison -> True
    TLTexture1D _ -> True
    TLTexture1DArray _ -> True
    TLTexture2D _ -> True
    TLTexture2DArray _ -> True
    TLTexture3D _ -> True
    TLTextureCube _ -> True
    TLTextureCubeArray _ -> True
    TLTextureMultisampled2D _ -> True
    TLTextureDepth2D -> True
    TLTextureDepth2DArray -> True
    TLTextureDepthCube -> True
    TLTextureDepthCubeArray -> True
    TLTextureDepthMultisampled2D -> True
    TLStorageTexture1D _ _ -> True
    TLStorageTexture2D _ _ -> True
    TLStorageTexture2DArray _ _ -> True
    TLStorageTexture3D _ _ -> True
    TLPointer _ _ elemLayout -> containsResource elemLayout
    TLArray _ _ elemLayout _ _ -> containsResource elemLayout
    TLMatrix _ _ _ _ _ _ -> False
    TLStruct _ fields _ _ -> any (containsResource . flType) fields
    _ -> False

containsAtomic :: TypeLayout -> Bool
containsAtomic layout =
  case layout of
    TLAtomic _ -> True
    TLPointer _ _ elemLayout -> containsAtomic elemLayout
    TLArray _ _ elemLayout _ _ -> containsAtomic elemLayout
    TLStruct _ fields _ _ -> any (containsAtomic . flType) fields
    _ -> False

containsPointer :: TypeLayout -> Bool
containsPointer layout =
  case layout of
    TLPointer _ _ _ -> True
    TLArray _ _ elemLayout _ _ -> containsPointer elemLayout
    TLStruct _ fields _ _ -> any (containsPointer . flType) fields
    _ -> False

containsRuntimeArray :: TypeLayout -> Bool
containsRuntimeArray layout =
  case layout of
    TLArray Nothing _ _ _ _ -> True
    TLArray _ _ elemLayout _ _ -> containsRuntimeArray elemLayout
    TLStruct _ fields _ _ -> any (containsRuntimeArray . flType) fields
    _ -> False

structSize :: [FieldLayout] -> Word32 -> Word32
structSize fields align =
  case fields of
    [] -> 0
    _ ->
      let lastField = last fields
          end = flOffset lastField + flSize lastField
      in roundUp end align

roundUp :: Word32 -> Word32 -> Word32
roundUp val align =
  if align == 0 then val else ((val + align - 1) `div` align) * align

-- SPIR-V emission

emitSpirv :: CompileOptions -> ModuleAst -> ShaderInterface -> Either CompileError ByteString
emitSpirv opts modAst iface = do
  entry <- case modEntry modAst of
    Nothing -> Left (CompileError "missing entry point" Nothing Nothing)
    Just e -> Right e
  validateEntry entry
  let structEnv = [(sdName s, s) | s <- modStructs modAst]
  structLayouts <- mapM (resolveStructLayout structEnv) (modStructs modAst)
  retLayout <- case (epStage entry, epReturnType entry) of
    (StageFragment, Just ty) -> Just <$> resolveTypeLayout structEnv [] ty
    (StageFragment, Nothing) -> Left (CompileError "fragment entry point missing return type" Nothing Nothing)
    (StageVertex, Just ty) -> Just <$> resolveTypeLayout structEnv [] ty
    (StageVertex, Nothing) -> Left (CompileError "vertex entry point missing return type" Nothing Nothing)
    _ -> Right Nothing
  let blockStructs = [T.pack name | BindingInfo _ _ _ _ (TLStruct name _ _ _) <- siBindings iface]
  let state0 = emptyGenState (epStage entry) structLayouts blockStructs
  let ((), state1) = emitStructs state0
  let node = ModuleNode "<merged>" [] modAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let ctx = buildModuleContext [] "" node
  state2 <- emitModuleOverrides ctx constIndex fnIndex structIndex (overrideSpecMode opts) structEnv (modOverrides modAst) state1
  state3 <- emitModuleConstants (modConsts modAst) state2
  (envGlobals, entryInits, _ifaceIds, outTargets, state4) <- emitGlobals structEnv iface entry retLayout (modGlobals modAst) state3
  state5 <- registerFunctions structEnv (modFunctions modAst) state4
  state6 <- emitFunctionBodies structEnv (modFunctions modAst) state5
  state7 <- emitMainFunction entry envGlobals entryInits outTargets state6
  let spirvWords = buildSpirvWords opts entry state7
  pure (spirvToBytes spirvWords)

emitModuleOverrides :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideSpecMode -> [(Text, StructDecl)] -> [OverrideDecl] -> GenState -> Either CompileError GenState
emitModuleOverrides ctx constIndex fnIndex structIndex specMode structEnv decls st0 =
  case decls of
    [] -> Right st0
    _ -> do
      specIds <- assignOverrideSpecIds decls
      let declMap = Map.fromList [(odName d, d) | d <- decls]
      let depsMap = overrideDependencies decls
      order <- topoSortOverrides (map odName decls) depsMap
      foldM (emitOne specIds depsMap) st0 (map (\n -> declMap Map.! n) order)
  where
    emitOne specIds depsMap st decl = do
      layout <- resolveTypeLayout structEnv [] (odType decl)
      let deps = Map.findWithDefault Set.empty (odName decl) depsMap
      (st2, val) <- case odExpr decl of
        Just expr ->
          case emitSpecConstExpr ctx constIndex fnIndex structIndex st expr of
              Right (st1, val0) -> do
                (st2', val1) <-
                  if valType val0 == layout
                    then Right (st1, val0)
                    else coerceSpecConstValueToLayout layout val0 st1
                if Set.null deps
                  then
                    if isSpecConstantLiteral (valId val1) st2'
                      then Right (st2', val1)
                      else fallbackLiteral layout expr st
                  else Right (st2', val1)
              Left err ->
                if Set.null deps
                  then fallbackLiteral layout expr st
                  else
                    Left
                      ( CompileError
                          ("override initializer depends on other overrides but is not a valid specialization-constant expression: " <> ceMessage err)
                          (ceLine err)
                          (ceColumn err)
                      )
        Nothing -> do
          defaultVal <- defaultConstValueForType structEnv (odType decl)
          emitSpecConstValueToLayout layout defaultVal st
      st3 <-
        if specMode == SpecParity || Set.null deps
          then do
            specId <- case Map.lookup (odName decl) specIds of
              Nothing -> Left (CompileError "missing specialization id for override" Nothing Nothing)
              Just i -> Right i
            Right (addDecoration (Instr opDecorate [valId val, decorationSpecId, specId]) st2)
          else Right st2
      let st4 = addName (Instr opName (valId val : encodeString (textToString (odName decl)))) st3
      let st5 = st4 { gsConstValues = (odName decl, val) : gsConstValues st4 }
      Right st5

    fallbackLiteral layout expr st = do
      constVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty expr
      emitSpecConstValueToLayout layout constVal st


defaultConstValueForType :: [(Text, StructDecl)] -> Type -> Either CompileError ConstValue
defaultConstValueForType structEnv ty =
  case ty of
    TyScalar Bool -> Right (CVBool False)
    TyScalar I32 -> Right (CVInt (ConstInt I32 0))
    TyScalar U32 -> Right (CVInt (ConstInt U32 0))
    TyScalar F32 -> Right (CVFloat (ConstFloat F32 0))
    TyScalar F16 -> Right (CVFloat (ConstFloat F16 0))
    TyVector n scalar -> do
      scalarVal <- defaultConstValueForType structEnv (TyScalar scalar)
      Right (CVVector n scalar (replicate n scalarVal))
    TyMatrix cols rows scalar -> do
      col <- defaultConstValueForType structEnv (TyVector rows scalar)
      Right (CVMatrix cols rows scalar (replicate cols col))
    TyArray elemTy (Just n) -> do
      elemVal <- defaultConstValueForType structEnv elemTy
      Right (CVArray elemTy (replicate n elemVal))
    TyArray _ Nothing ->
      Left (CompileError "override defaults cannot target runtime-sized arrays" Nothing Nothing)
    TyStructRef name ->
      case lookup name structEnv of
        Nothing -> Left (CompileError ("unknown struct: " <> textToString name) Nothing Nothing)
        Just decl -> do
          fields <- mapM fieldDefault (sdFields decl)
          Right (CVStruct name fields)
    _ -> Left (CompileError "override defaults are only supported for scalar, vector, matrix, array, and struct types" Nothing Nothing)
  where
    fieldDefault fld = do
      val <- defaultConstValueForType structEnv (fdType fld)
      Right (fdName fld, val)

emitModuleConstants :: [ConstDecl] -> GenState -> Either CompileError GenState
emitModuleConstants decls st0 = foldM emitOne st0 decls
  where
    emitOne st decl =
      case lookup (cdName decl) (gsConstValues st) of
        Just _ -> Left (CompileError ("duplicate constant: " <> textToString (cdName decl)) Nothing Nothing)
        Nothing -> do
          (st1, val) <- emitConstExpr st (cdExpr decl)
          let st2 = st1 { gsConstValues = (cdName decl, val) : gsConstValues st1 }
          pure st2

emitConstExpr :: GenState -> Expr -> Either CompileError (GenState, Value)
emitConstExpr st expr =
  case expr of
    EInt n -> do
      (scalar, val) <- selectIntLiteralScalar n
      case scalar of
        I32 -> do
          let (cid, st1) = emitConstI32 st (fromIntegral val)
          let (a, sz) = scalarLayout I32
          let layout = TLScalar I32 a sz
          Right (st1, Value layout cid)
        U32 -> do
          let (cid, st1) = emitConstU32 st (fromIntegral val)
          let (a, sz) = scalarLayout U32
          let layout = TLScalar U32 a sz
          Right (st1, Value layout cid)
        _ -> Left (CompileError "integer literal must be i32 or u32" Nothing Nothing)
    EFloat f -> do
      let (cid, st1) = emitConstF32 st f
      let (a, sz) = scalarLayout F32
      let layout = TLScalar F32 a sz
      Right (st1, Value layout cid)
    EBool b -> do
      let (cid, st1) = emitConstBool st b
      let (a, sz) = scalarLayout Bool
      let layout = TLScalar Bool a sz
      Right (st1, Value layout cid)
    EUnary OpNeg inner -> do
      (st1, val) <- emitConstExpr st inner
      case valType val of
        TLScalar I32 _ _ -> do
          key <- maybe (Left (CompileError "constant integer literal required" Nothing Nothing)) Right (lookupConstKeyById st1 (valId val))
          v <- constKeyToI32 key
          let out = negate (fromIntegral v :: Integer)
          (cid, st2) <- emitConstIntScalar I32 out st1
          let (a, sz) = scalarLayout I32
          let layout = TLScalar I32 a sz
          Right (st2, Value layout cid)
        TLScalar U32 _ _ -> Left (CompileError "unary minus is not supported for u32 constants" Nothing Nothing)
        _ -> Left (CompileError "unary minus expects an integer constant" Nothing Nothing)
    EBinary op a b -> do
      (st1, v1) <- emitConstExpr st a
      (st2, v2) <- emitConstExpr st1 b
      case (valType v1, valType v2) of
        (TLScalar s1 _ _, TLScalar s2 _ _) -> do
          key1 <- maybe (Left (CompileError "constant integer literal required" Nothing Nothing)) Right (lookupConstKeyById st2 (valId v1))
          key2 <- maybe (Left (CompileError "constant integer literal required" Nothing Nothing)) Right (lookupConstKeyById st2 (valId v2))
          i1 <- constKeyToInt key1
          i2 <- constKeyToInt key2
          let (scalar, x, y) = coerceConstPair s1 i1 s2 i2
          out <- case op of
            OpAdd -> pure (x + y)
            OpSub -> pure (x - y)
            OpMul -> pure (x * y)
            OpDiv ->
              if y == 0 then Left (CompileError "division by zero in constant expression" Nothing Nothing) else pure (x `quot` y)
            OpMod ->
              if y == 0 then Left (CompileError "modulo by zero in constant expression" Nothing Nothing) else pure (x `rem` y)
            OpBitAnd -> pure (x .&. y)
            OpBitOr -> pure (x .|. y)
            OpBitXor -> pure (xor x y)
            OpShl ->
              if y < 0 then Left (CompileError "shift amount must be non-negative" Nothing Nothing) else pure (shiftL x (fromIntegral y))
            OpShr ->
              if y < 0 then Left (CompileError "shift amount must be non-negative" Nothing Nothing) else pure (shiftR x (fromIntegral y))
            _ -> Left (CompileError "unsupported constant integer operation" Nothing Nothing)
          case scalar of
            I32 -> do
              let out' = out
              when (out' < 0 || out' > 0x7FFFFFFF) $
                Left (CompileError "constant i32 is out of range" Nothing Nothing)
              (cid, st3) <- emitConstIntScalar I32 out' st2
              let (a', sz') = scalarLayout I32
              Right (st3, Value (TLScalar I32 a' sz') cid)
            U32 -> do
              let out' = out
              when (out' < 0 || out' > fromIntegral (maxBound :: Word32)) $
                Left (CompileError "constant u32 is out of range" Nothing Nothing)
              (cid, st3) <- emitConstIntScalar U32 out' st2
              let (a', sz') = scalarLayout U32
              Right (st3, Value (TLScalar U32 a' sz') cid)
            _ -> Left (CompileError "unsupported constant integer operation" Nothing Nothing)
        _ -> Left (CompileError "constant integer operation expects scalar ints" Nothing Nothing)
    EVar name ->
      case lookup name (gsConstValues st) of
        Just val -> Right (st, val)
        Nothing -> Left (CompileError ("unknown constant: " <> textToString name) Nothing Nothing)
    ECall name args ->
      case name of
        "vec2" -> emitConstVectorCtor 2 args st
        "vec3" -> emitConstVectorCtor 3 args st
        "vec4" -> emitConstVectorCtor 4 args st
        "array" -> emitConstArrayCtor args st
        "f16" -> emitConstScalarCtor F16 args st
        "f32" -> emitConstScalarCtor F32 args st
        "u32" -> emitConstScalarCtor U32 args st
        "i32" -> emitConstScalarCtor I32 args st
        _ ->
          case parseMatrixName name of
            Just (cols, rows) -> emitConstMatrixCtor cols rows args st
            Nothing ->
              case lookup name (gsStructLayouts st) of
                Just layout -> emitConstStructCtor name layout args st
                Nothing -> Left (CompileError ("unsupported constant constructor: " <> textToString name) Nothing Nothing)
    _ -> Left (CompileError "unsupported constant expression" Nothing Nothing)
  where
    constKeyToInt key =
      case key of
        ConstI32 v -> Right (fromIntegral (fromIntegral v :: Int32))
        ConstU32 v -> Right (fromIntegral v)
        _ -> Left (CompileError "expected integer constant" Nothing Nothing)
    coerceConstPair s1 v1 s2 v2 =
      case (s1, s2) of
        (I32, I32) -> (I32, v1, v2)
        (U32, U32) -> (U32, v1, v2)
        (I32, U32) ->
          if v2 <= 0x7FFFFFFF
            then (I32, v1, v2)
            else (I32, v1, v2)
        (U32, I32) ->
          if v1 <= 0x7FFFFFFF
            then (I32, v1, v2)
            else (I32, v1, v2)
        _ -> (I32, v1, v2)

selectIntLiteralScalar :: Integer -> Either CompileError (Scalar, Integer)
selectIntLiteralScalar n =
  let minI32 = fromIntegral (minBound :: Int32)
      maxI32 = fromIntegral (maxBound :: Int32)
      maxU32 = fromIntegral (maxBound :: Word32)
  in if n < minI32
       then Left (CompileError "integer literal is out of range" Nothing Nothing)
       else if n <= maxI32
         then Right (I32, n)
         else if n <= maxU32
           then Right (U32, n)
           else Left (CompileError "integer literal is out of range" Nothing Nothing)

emitConstValueToLayout :: TypeLayout -> ConstValue -> GenState -> Either CompileError (GenState, Value)
emitConstValueToLayout layout val st =
  case layout of
    TLScalar scalar _ _ -> do
      val' <- coerceConstScalarValue scalar val
      case val' of
        CVBool b ->
          let (cid, st1) = emitConstBool st b
          in Right (st1, Value layout cid)
        CVInt (ConstInt _ v) -> do
          (cid, st1) <- emitConstIntScalar scalar v st
          Right (st1, Value layout cid)
        CVFloat (ConstFloat _ v) -> do
          (cid, st1) <- emitConstFloatScalar scalar (realToFrac v) st
          Right (st1, Value layout cid)
        _ -> Left (CompileError "expected scalar constant" Nothing Nothing)
    TLVector n scalar _ _ ->
      case val of
        CVVector m _ comps | m == n -> do
          comps' <- mapM (coerceConstScalarValue scalar) comps
          let (a, sz) = scalarLayout scalar
          (st1, vals) <- emitConstValues (TLScalar scalar a sz) comps' st
          let (cid, st2) = emitConstComposite layout (map valId vals) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "vector constant does not match type" Nothing Nothing)
    TLMatrix cols rows scalar _ _ _ ->
      case val of
        CVMatrix c r _ colsVals | c == cols && r == rows -> do
          let (a, sz) = vectorLayout scalar rows
          let colLayout = TLVector rows scalar a sz
          (st1, colsEmitted) <- emitConstValues colLayout colsVals st
          let (cid, st2) = emitConstComposite layout (map valId colsEmitted) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "matrix constant does not match type" Nothing Nothing)
    TLArray mlen _ elemLayout _ _ ->
      case (mlen, val) of
        (Just n, CVArray _ elems) | length elems == n -> do
          (st1, vals) <- emitConstValues elemLayout elems st
          let (cid, st2) = emitConstComposite layout (map valId vals) st1
          Right (st2, Value layout cid)
        (Nothing, _) -> Left (CompileError "runtime array constants are not supported" Nothing Nothing)
        _ -> Left (CompileError "array constant does not match type" Nothing Nothing)
    TLStruct name fields _ _ ->
      let nameT = T.pack name
      in case val of
        CVStruct structName pairs | structName == nameT -> do
          fieldVals <- mapM (lookupField pairs) fields
          (st1, emitted) <- emitConstValuesFromFields fields fieldVals st
          let (cid, st2) = emitConstComposite layout (map valId emitted) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "struct constant does not match type" Nothing Nothing)
    _ -> Left (CompileError "unsupported constant layout" Nothing Nothing)
  where
    lookupField pairs fld =
      case lookup (T.pack (flName fld)) pairs of
        Just v -> Right v
        Nothing -> Left (CompileError ("missing field: " <> flName fld) Nothing Nothing)

    emitConstValues elemLayout vals st0 = foldM go (st0, []) vals
      where
        go (stAcc, acc) v = do
          (st', val') <- emitConstValueToLayout elemLayout v stAcc
          Right (st', acc <> [val'])

    emitConstValuesFromFields fieldLayouts vals st0 = foldM go (st0, []) (zip fieldLayouts vals)
      where
        go (stAcc, acc) (fld, v) = do
          (st', val') <- emitConstValueToLayout (flType fld) v stAcc
          Right (st', acc <> [val'])

emitSpecConstValueToLayout :: TypeLayout -> ConstValue -> GenState -> Either CompileError (GenState, Value)
emitSpecConstValueToLayout layout val st =
  case layout of
    TLScalar scalar _ _ -> do
      val' <- coerceConstScalarValue scalar val
      case val' of
        CVBool b ->
          let (cid, st1) = emitSpecConstBool st b
          in Right (st1, Value layout cid)
        CVInt (ConstInt _ v) -> do
          (cid, st1) <- emitSpecConstIntScalar scalar v st
          Right (st1, Value layout cid)
        CVFloat (ConstFloat _ v) -> do
          (cid, st1) <- emitSpecConstFloatScalar scalar (realToFrac v) st
          Right (st1, Value layout cid)
        _ -> Left (CompileError "expected scalar constant" Nothing Nothing)
    TLVector n scalar _ _ ->
      case val of
        CVVector m _ comps | m == n -> do
          comps' <- mapM (coerceConstScalarValue scalar) comps
          let (a, sz) = scalarLayout scalar
          (st1, vals) <- emitConstValues (TLScalar scalar a sz) comps' st
          let (cid, st2) = emitSpecConstComposite layout (map valId vals) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "vector constant does not match type" Nothing Nothing)
    TLMatrix cols rows scalar _ _ _ ->
      case val of
        CVMatrix c r _ colsVals | c == cols && r == rows -> do
          let (a, sz) = vectorLayout scalar rows
          let colLayout = TLVector rows scalar a sz
          (st1, colsEmitted) <- emitConstValues colLayout colsVals st
          let (cid, st2) = emitSpecConstComposite layout (map valId colsEmitted) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "matrix constant does not match type" Nothing Nothing)
    TLArray mlen _ elemLayout _ _ ->
      case (mlen, val) of
        (Just n, CVArray _ elems) | length elems == n -> do
          (st1, vals) <- emitConstValues elemLayout elems st
          let (cid, st2) = emitSpecConstComposite layout (map valId vals) st1
          Right (st2, Value layout cid)
        (Nothing, _) -> Left (CompileError "runtime array constants are not supported" Nothing Nothing)
        _ -> Left (CompileError "array constant does not match type" Nothing Nothing)
    TLStruct name fields _ _ ->
      let nameT = T.pack name
      in case val of
        CVStruct structName pairs | structName == nameT -> do
          fieldVals <- mapM (lookupField pairs) fields
          (st1, emitted) <- emitConstValuesFromFields fields fieldVals st
          let (cid, st2) = emitSpecConstComposite layout (map valId emitted) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "struct constant does not match type" Nothing Nothing)
    _ -> Left (CompileError "unsupported constant layout" Nothing Nothing)
  where
    lookupField pairs fld =
      case lookup (T.pack (flName fld)) pairs of
        Just v -> Right v
        Nothing -> Left (CompileError ("missing field: " <> flName fld) Nothing Nothing)

    emitConstValues elemLayout vals st0 = foldM go (st0, []) vals
      where
        go (stAcc, acc) v = do
          (st', val') <- emitConstValueToLayout elemLayout v stAcc
          Right (st', acc <> [val'])

    emitConstValuesFromFields fieldLayouts vals st0 = foldM go (st0, []) (zip fieldLayouts vals)
      where
        go (stAcc, acc) (fld, v) = do
          (st', val') <- emitConstValueToLayout (flType fld) v stAcc
          Right (st', acc <> [val'])

constValueLayout :: GenState -> ConstValue -> Either CompileError TypeLayout
constValueLayout st val =
  case val of
    CVBool _ ->
      let (a, sz) = scalarLayout Bool
      in Right (TLScalar Bool a sz)
    CVInt (ConstInt scalar _) ->
      let (a, sz) = scalarLayout scalar
      in Right (TLScalar scalar a sz)
    CVFloat (ConstFloat scalar _) ->
      let (a, sz) = scalarLayout scalar
      in Right (TLScalar scalar a sz)
    CVVector n scalar _ ->
      let (a, sz) = vectorLayout scalar n
      in Right (TLVector n scalar a sz)
    CVMatrix cols rows scalar _ ->
      let layout = matrixLayout cols rows scalar
      in Right layout
    CVArray elemTy elems -> do
      elemLayout <- typeLayoutFromValue elemTy
      let elemAlign = layoutAlign elemLayout
      let elemSize = layoutSize elemLayout
      let stride = roundUp elemSize elemAlign
      let total = stride * fromIntegral (length elems)
      Right (TLArray (Just (length elems)) stride elemLayout elemAlign total)
    CVStruct name _ ->
      case lookup name (gsStructLayouts st) of
        Just layout -> Right layout
        Nothing -> Left (CompileError ("unknown struct layout for constant: " <> textToString name) Nothing Nothing)
    CVPointer _ _ ->
      Left (CompileError "pointer constants are not supported" Nothing Nothing)
  where
    typeLayoutFromValue ty =
      case ty of
        TyScalar scalar ->
          let (a, sz) = scalarLayout scalar
          in Right (TLScalar scalar a sz)
        TyVector n scalar ->
          let (a, sz) = vectorLayout scalar n
          in Right (TLVector n scalar a sz)
        TyMatrix cols rows scalar ->
          Right (matrixLayout cols rows scalar)
        TyArray elemTy (Just count) -> do
          elemLayout <- typeLayoutFromValue elemTy
          let elemAlign = layoutAlign elemLayout
          let elemSize = layoutSize elemLayout
          let stride = roundUp elemSize elemAlign
          let total = stride * fromIntegral count
          Right (TLArray (Just count) stride elemLayout elemAlign total)
        TyArray _ Nothing ->
          Left (CompileError "runtime array constants are not supported" Nothing Nothing)
        TyStructRef name ->
          case lookup name (gsStructLayouts st) of
            Just layout -> Right layout
            Nothing -> Left (CompileError ("unknown struct layout for constant: " <> textToString name) Nothing Nothing)
        _ -> Left (CompileError "unsupported constant type layout" Nothing Nothing)

emitSpecConstOp :: TypeLayout -> Word16 -> [Word32] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstOp layout opcode operands st = do
  let (tyId, st1) = emitTypeFromLayout st layout
  let (resId, st2) = freshId st1
  let instr = Instr opSpecConstantOp (tyId : resId : fromIntegral opcode : operands)
  let st3 = addConst instr st2
  Right (st3, Value layout resId)

isSpecConstantLiteral :: Word32 -> GenState -> Bool
isSpecConstantLiteral cid st = any matches (gsConstants st)
  where
    matches (Instr op ops)
      | op == opSpecConstantTrue || op == opSpecConstantFalse =
          case ops of
            (_ty:rid:_) -> rid == cid
            _ -> False
      | op == opSpecConstant || op == opSpecConstantComposite =
          case ops of
            (_ty:rid:_) -> rid == cid
            _ -> False
      | otherwise = False

emitSpecConstScalarConvert :: Scalar -> Scalar -> Value -> GenState -> Either CompileError (GenState, Value)
emitSpecConstScalarConvert from to val st = do
  opcode <- case (from, to) of
    (U32, F32) -> Right opConvertUToF
    (I32, F32) -> Right opConvertSToF
    (U32, F16) -> Right opConvertUToF
    (I32, F16) -> Right opConvertSToF
    (F32, U32) -> Right opConvertFToU
    (F32, I32) -> Right opConvertFToS
    (F16, U32) -> Right opConvertFToU
    (F16, I32) -> Right opConvertFToS
    (F16, F32) -> Right opFConvert
    (F32, F16) -> Right opFConvert
    (U32, I32) -> Right opBitcast
    (I32, U32) -> Right opBitcast
    _ -> Left (CompileError "unsupported scalar conversion for spec constant" Nothing Nothing)
  let (a, sz) = scalarLayout to
  let layout = TLScalar to a sz
  emitSpecConstOp layout opcode [valId val] st

coerceSpecConstValueToLayout :: TypeLayout -> Value -> GenState -> Either CompileError (GenState, Value)
coerceSpecConstValueToLayout target val st
  | valType val == target = Right (st, val)
  | otherwise =
      case (target, valType val) of
        (TLScalar targetScalar _ _, TLScalar srcScalar _ _) -> do
          case targetScalar of
            Bool ->
              Left (CompileError "implicit conversion to bool is not supported for spec constants" Nothing Nothing)
            _ ->
              emitSpecConstScalarConvert srcScalar targetScalar val st
        _ -> Left (CompileError "type mismatch" Nothing Nothing)

coerceSpecConstValuesToLayout :: TypeLayout -> [Value] -> GenState -> Either CompileError (GenState, [Value])
coerceSpecConstValuesToLayout target vals st = go st [] vals
  where
    go st' acc [] = Right (st', reverse acc)
    go st' acc (v:vs) = do
      (st1, v') <- coerceSpecConstValueToLayout target v st'
      go st1 (v':acc) vs

emitSpecConstExpr :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> GenState -> Expr -> Either CompileError (GenState, Value)
emitSpecConstExpr ctx constIndex fnIndex structIndex st expr =
  case tryEmit st expr of
    Right res -> Right res
    Left _ -> do
      constVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty expr
      layout <- constValueLayout st constVal
      emitSpecConstValueToLayout layout constVal st
  where
    tryEmit st0 ex =
      case ex of
        EInt n -> do
          (scalar, val) <- selectIntLiteralScalar n
          case scalar of
            I32 -> do
              (cid, st1) <- emitSpecConstIntScalar I32 val st0
              let (a, sz) = scalarLayout I32
              Right (st1, Value (TLScalar I32 a sz) cid)
            U32 -> do
              (cid, st1) <- emitSpecConstIntScalar U32 val st0
              let (a, sz) = scalarLayout U32
              Right (st1, Value (TLScalar U32 a sz) cid)
            _ -> Left (CompileError "integer literal must be i32 or u32" Nothing Nothing)
        EFloat f -> do
          (cid, st1) <- emitSpecConstFloatScalar F32 f st0
          let (a, sz) = scalarLayout F32
          Right (st1, Value (TLScalar F32 a sz) cid)
        EBool b -> do
          let (cid, st1) = emitSpecConstBool st0 b
          let (a, sz) = scalarLayout Bool
          Right (st1, Value (TLScalar Bool a sz) cid)
        EUnary OpNeg inner -> do
          (st1, v) <- tryEmit st0 inner
          case valType v of
            TLScalar I32 _ _ -> emitSpecConstOp (valType v) opSNegate [valId v] st1
            TLScalar U32 _ _ -> Left (CompileError "unary minus is not supported for u32 spec constants" Nothing Nothing)
            TLScalar F32 _ _ -> emitSpecConstOp (valType v) opFNegate [valId v] st1
            TLScalar F16 _ _ -> emitSpecConstOp (valType v) opFNegate [valId v] st1
            _ -> Left (CompileError "unary minus expects a scalar" Nothing Nothing)
        EUnary OpNot inner -> do
          (st1, v) <- tryEmit st0 inner
          case valType v of
            TLScalar Bool _ _ -> emitSpecConstOp (valType v) opLogicalNot [valId v] st1
            _ -> Left (CompileError "logical not expects bool" Nothing Nothing)
        EBinary op a b -> do
          (st1, v1) <- tryEmit st0 a
          (st2, v2) <- tryEmit st1 b
          case (valType v1, valType v2) of
            (TLScalar s1 _ _, TLScalar s2 _ _) | s1 == s2 -> do
              let layout = valType v1
              let opIds = [valId v1, valId v2]
              case s1 of
                I32 ->
                  emitSpecConstIntOp layout op opIds st2
                U32 ->
                  emitSpecConstIntOp layout op opIds st2
                F32 ->
                  emitSpecConstFloatOp layout op opIds st2
                F16 ->
                  emitSpecConstFloatOp layout op opIds st2
                Bool ->
                  emitSpecConstBoolOp layout op opIds st2
            _ -> Left (CompileError "unsupported spec constant binary operation" Nothing Nothing)
        ECall "select" [aExpr, bExpr, condExpr] -> do
          (st1, vA) <- tryEmit st0 aExpr
          (st2, vB) <- tryEmit st1 bExpr
          (st3, vCond) <- tryEmit st2 condExpr
          case (valType vA, valType vB, valType vCond) of
            (layoutA, layoutB, TLScalar Bool _ _) | layoutA == layoutB ->
              emitSpecConstOp layoutA opSelect [valId vCond, valId vA, valId vB] st3
            _ -> Left (CompileError "select requires matching value types and a bool condition" Nothing Nothing)
        ECall name args ->
          case name of
            "vec2" -> emitSpecConstVectorCtor ctx constIndex fnIndex structIndex 2 args st0
            "vec3" -> emitSpecConstVectorCtor ctx constIndex fnIndex structIndex 3 args st0
            "vec4" -> emitSpecConstVectorCtor ctx constIndex fnIndex structIndex 4 args st0
            "array" -> emitSpecConstArrayCtor ctx constIndex fnIndex structIndex args st0
            "f16" -> emitSpecConstScalarCtor ctx constIndex fnIndex structIndex F16 args st0
            "f32" -> emitSpecConstScalarCtor ctx constIndex fnIndex structIndex F32 args st0
            "u32" -> emitSpecConstScalarCtor ctx constIndex fnIndex structIndex U32 args st0
            "i32" -> emitSpecConstScalarCtor ctx constIndex fnIndex structIndex I32 args st0
            _ ->
              case parseMatrixName name of
                Just (cols, rows) -> emitSpecConstMatrixCtor ctx constIndex fnIndex structIndex cols rows args st0
                Nothing ->
                  case lookup name (gsStructLayouts st) of
                    Just layout -> emitSpecConstStructCtor ctx constIndex fnIndex structIndex name layout args st0
                    Nothing -> Left (CompileError ("unsupported spec constant constructor: " <> textToString name) Nothing Nothing)
        EVar name ->
          case lookup name (gsConstValues st0) of
            Just val -> Right (st0, val)
            Nothing -> Left (CompileError "const reference requires fallback" Nothing Nothing)
        _ -> Left (CompileError "unsupported spec constant expression" Nothing Nothing)

    emitSpecConstIntOp layout op opIds st' =
      case op of
        OpAdd -> emitSpecConstOp layout opIAdd opIds st'
        OpSub -> emitSpecConstOp layout opISub opIds st'
        OpMul -> emitSpecConstOp layout opIMul opIds st'
        OpDiv ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp layout opUDiv opIds st'
            _ -> emitSpecConstOp layout opSDiv opIds st'
        OpMod ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp layout opUMod opIds st'
            _ -> emitSpecConstOp layout opSRem opIds st'
        OpBitAnd -> emitSpecConstOp layout opBitwiseAnd opIds st'
        OpBitOr -> emitSpecConstOp layout opBitwiseOr opIds st'
        OpBitXor -> emitSpecConstOp layout opBitwiseXor opIds st'
        OpShl -> emitSpecConstOp layout opShiftLeftLogical opIds st'
        OpShr ->
          case layout of
            TLScalar I32 _ _ -> emitSpecConstOp layout opShiftRightArithmetic opIds st'
            _ -> emitSpecConstOp layout opShiftRightLogical opIds st'
        OpEq -> emitSpecConstOp (boolLayout) opIEqual opIds st'
        OpNe -> emitSpecConstOp (boolLayout) opINotEqual opIds st'
        OpLt ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp boolLayout opULessThan opIds st'
            _ -> emitSpecConstOp boolLayout opSLessThan opIds st'
        OpLe ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp boolLayout opULessThanEqual opIds st'
            _ -> emitSpecConstOp boolLayout opSLessThanEqual opIds st'
        OpGt ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp boolLayout opUGreaterThan opIds st'
            _ -> emitSpecConstOp boolLayout opSGreaterThan opIds st'
        OpGe ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp boolLayout opUGreaterThanEqual opIds st'
            _ -> emitSpecConstOp boolLayout opSGreaterThanEqual opIds st'
        _ -> Left (CompileError "unsupported int spec constant op" Nothing Nothing)

    emitSpecConstFloatOp layout op opIds st' =
      case op of
        OpAdd -> emitSpecConstOp layout opFAdd opIds st'
        OpSub -> emitSpecConstOp layout opFSub opIds st'
        OpMul -> emitSpecConstOp layout opFMul opIds st'
        OpDiv -> emitSpecConstOp layout opFDiv opIds st'
        OpEq -> emitSpecConstOp boolLayout opFOrdEqual opIds st'
        OpNe -> emitSpecConstOp boolLayout opFOrdNotEqual opIds st'
        OpLt -> emitSpecConstOp boolLayout opFOrdLessThan opIds st'
        OpLe -> emitSpecConstOp boolLayout opFOrdLessThanEqual opIds st'
        OpGt -> emitSpecConstOp boolLayout opFOrdGreaterThan opIds st'
        OpGe -> emitSpecConstOp boolLayout opFOrdGreaterThanEqual opIds st'
        _ -> Left (CompileError "unsupported float spec constant op" Nothing Nothing)

    emitSpecConstBoolOp layout op opIds st' =
      case op of
        OpAnd -> emitSpecConstOp layout opLogicalAnd opIds st'
        OpOr -> emitSpecConstOp layout opLogicalOr opIds st'
        OpEq -> emitSpecConstOp layout opLogicalEqual opIds st'
        OpNe -> emitSpecConstOp layout opLogicalNotEqual opIds st'
        _ -> Left (CompileError "unsupported bool spec constant op" Nothing Nothing)

    boolLayout =
      let (a, sz) = scalarLayout Bool
      in TLScalar Bool a sz

emitSpecConstExprList :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> GenState -> [Expr] -> Either CompileError (GenState, [Value])
emitSpecConstExprList ctx constIndex fnIndex structIndex st exprs = go st [] exprs
  where
    go st' acc [] = Right (st', reverse acc)
    go st' acc (e:es) = do
      (st1, v) <- emitSpecConstExpr ctx constIndex fnIndex structIndex st' e
      go st1 (v:acc) es

emitSpecConstVectorCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Int -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstVectorCtor ctx constIndex fnIndex structIndex n args st =
  if length args /= n
    then Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
    else do
      (st1, vals) <- emitSpecConstExprList ctx constIndex fnIndex structIndex st args
      case vals of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        _ -> do
          let baseLayout = pickBaseLayout st1 vals
          (st2, vals') <- coerceSpecConstValuesToLayout baseLayout vals st1
          scalar <- case baseLayout of
            TLScalar s _ _ -> Right s
            _ -> Left (CompileError "vector constructor arguments must be scalars" Nothing Nothing)
          let (align, size) = vectorLayout scalar n
          let layout = TLVector n scalar align size
          let (cid, st3) = emitSpecConstComposite layout (map valId vals') st2
          Right (st3, Value layout cid)

emitSpecConstMatrixCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Int -> Int -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstMatrixCtor ctx constIndex fnIndex structIndex cols rows args st =
  if null args
    then Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
    else do
      (st1, vals) <- emitSpecConstExprList ctx constIndex fnIndex structIndex st args
      case vals of
        [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        _ -> do
          let baseLayout = pickBaseLayout st1 vals
          let scalarCount = cols * rows
          case baseLayout of
            TLScalar scalar _ _ | length vals == scalarCount -> do
              (st1', vals') <- coerceSpecConstValuesToLayout baseLayout vals st1
              let (a, sz) = vectorLayout scalar rows
              let vecLayout = TLVector rows scalar a sz
              (st2, colsVals) <- buildColumns vecLayout vals' st1'
              let layout = matrixLayout cols rows scalar
              let (cid, st3) = emitSpecConstComposite layout (map valId colsVals) st2
              Right (st3, Value layout cid)
            TLVector n scalar _ _ | n == rows && length vals == cols -> do
              mapM_ (ensureColumn scalar) vals
              let layout = matrixLayout cols rows scalar
              let (cid, st2) = emitSpecConstComposite layout (map valId vals) st1
              Right (st2, Value layout cid)
            _ -> Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
  where
    ensureColumn scalar val =
      case valType val of
        TLVector n s _ _ | n == rows && s == scalar -> Right ()
        _ -> Left (CompileError "matrix constructor expects column vectors" Nothing Nothing)
    buildColumns vecLayout vals st' =
      case splitAt rows vals of
        ([], _) -> Right (st', [])
        (col, rest) -> do
          let (cid, st1) = emitSpecConstComposite vecLayout (map valId col) st'
          (st2, colsVals) <- buildColumns vecLayout rest st1
          Right (st2, Value vecLayout cid : colsVals)

emitSpecConstStructCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Text -> TypeLayout -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstStructCtor ctx constIndex fnIndex structIndex name layout args st =
  case layout of
    TLStruct _ fields _ _ -> do
      when (length args /= length fields) $
        Left (CompileError ("struct constructor arity mismatch for " <> textToString name) Nothing Nothing)
      (st1, vals) <- emitSpecConstExprList ctx constIndex fnIndex structIndex st args
      (st2, vals') <- coerceSpecConstArgsToLayouts vals (map flType fields) st1
      let (cid, st3) = emitSpecConstComposite layout (map valId vals') st2
      Right (st3, Value layout cid)
    _ -> Left (CompileError ("unsupported constructor: " <> textToString name) Nothing Nothing)

emitSpecConstArrayCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstArrayCtor ctx constIndex fnIndex structIndex args st =
  case args of
    [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
    _ -> do
      (st1, vals) <- emitSpecConstExprList ctx constIndex fnIndex structIndex st args
      case vals of
        [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
        _ -> do
          let firstLayout = pickBaseLayout st1 vals
          (st2, vals') <- coerceSpecConstValuesToLayout firstLayout vals st1
          when (containsResource firstLayout) $
            Left (CompileError "arrays of resources are not supported" Nothing Nothing)
          when (containsAtomic firstLayout) $
            Left (CompileError "arrays of atomic types are not supported" Nothing Nothing)
          let elemAlign = layoutAlign firstLayout
          let elemSize = layoutSize firstLayout
          let stride = roundUp elemSize elemAlign
          let total = stride * fromIntegral (length vals')
          let layout = TLArray (Just (length vals)) stride firstLayout elemAlign total
          let (cid, st3) = emitSpecConstComposite layout (map valId vals') st2
          Right (st3, Value layout cid)

emitSpecConstScalarCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Scalar -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstScalarCtor ctx constIndex fnIndex structIndex scalar args st =
  case args of
    [arg] -> do
      (st1, val) <- emitSpecConstExpr ctx constIndex fnIndex structIndex st arg
      case valType val of
        TLScalar s _ _ | s == scalar -> Right (st1, val)
        TLScalar s _ _ -> do
          (st2, val') <- emitSpecConstScalarConvert s scalar val st1
          Right (st2, val')
        _ -> Left (CompileError "scalar constant cast requires a scalar argument" Nothing Nothing)
    _ -> Left (CompileError "scalar constant cast requires a single argument" Nothing Nothing)

coerceSpecConstArgsToLayouts :: [Value] -> [TypeLayout] -> GenState -> Either CompileError (GenState, [Value])
coerceSpecConstArgsToLayouts vals tys st = go st [] vals tys
  where
    go st' acc [] [] = Right (st', reverse acc)
    go st' acc (v:vs) (t:ts) = do
      (st1, v') <- coerceSpecConstValueToLayout t v st'
      go st1 (v':acc) vs ts
    go _ _ _ _ = Left (CompileError "constructor arity mismatch" Nothing Nothing)

emitConstExprList :: GenState -> [Expr] -> Either CompileError (GenState, [Value])
emitConstExprList st exprs = go st [] exprs
  where
    go st' acc [] = Right (st', reverse acc)
    go st' acc (e:es) = do
      (st1, v) <- emitConstExpr st' e
      go st1 (v:acc) es

emitConstVectorCtor :: Int -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstVectorCtor n args st =
  if length args /= n
    then Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
    else do
      (st1, vals) <- emitConstExprList st args
      case vals of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        _ -> do
          let baseLayout = pickBaseLayout st1 vals
          (st2, vals') <- coerceConstValuesToLayout baseLayout vals st1
          scalar <- case baseLayout of
            TLScalar s _ _ -> Right s
            _ -> Left (CompileError "vector constructor arguments must be scalars" Nothing Nothing)
          let (align, size) = vectorLayout scalar n
          let layout = TLVector n scalar align size
          let (cid, st3) = emitConstComposite layout (map valId vals') st2
          Right (st3, Value layout cid)

emitConstMatrixCtor :: Int -> Int -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstMatrixCtor cols rows args st =
  if null args
    then Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
    else do
      (st1, vals) <- emitConstExprList st args
      case vals of
        [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        _ -> do
          let baseLayout = pickBaseLayout st1 vals
          (st2, vals') <- coerceConstValuesToLayout baseLayout vals st1
          let scalarCount = cols * rows
          case baseLayout of
            TLScalar scalar _ _ | length vals' == scalarCount -> do
              let (a, sz) = vectorLayout scalar rows
              let vecLayout = TLVector rows scalar a sz
              (st3, colsVals) <- buildColumns vecLayout vals' st2
              let layout = matrixLayout cols rows scalar
              let (cid, st4) = emitConstComposite layout (map valId colsVals) st3
              Right (st4, Value layout cid)
            TLVector n scalar _ _ | n == rows && length vals' == cols -> do
              let layout = matrixLayout cols rows scalar
              let (cid, st3) = emitConstComposite layout (map valId vals') st2
              Right (st3, Value layout cid)
            _ -> Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
  where
    buildColumns vecLayout vals st' =
      case splitAt rows vals of
        ([], _) -> Right (st', [])
        (col, rest) -> do
          let (cid, st1) = emitConstComposite vecLayout (map valId col) st'
          (st2, colsVals) <- buildColumns vecLayout rest st1
          Right (st2, Value vecLayout cid : colsVals)

emitConstStructCtor :: Text -> TypeLayout -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstStructCtor name layout args st =
  case layout of
    TLStruct _ fields _ _ -> do
      when (length args /= length fields) $
        Left (CompileError ("struct constructor arity mismatch for " <> textToString name) Nothing Nothing)
      (st1, vals) <- emitConstExprList st args
      (st2, vals') <- coerceConstArgsToLayouts vals (map flType fields) st1
      let (cid, st3) = emitConstComposite layout (map valId vals') st2
      Right (st3, Value layout cid)
    _ -> Left (CompileError ("unsupported constructor: " <> textToString name) Nothing Nothing)

emitConstArrayCtor :: [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstArrayCtor args st =
  case args of
    [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
    _ -> do
      (st1, vals) <- emitConstExprList st args
      case vals of
        [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
        _ -> do
          let firstLayout = pickBaseLayout st1 vals
          (st2, vals') <- coerceConstValuesToLayout firstLayout vals st1
          when (containsResource firstLayout) $
            Left (CompileError "arrays of resources are not supported" Nothing Nothing)
          when (containsAtomic firstLayout) $
            Left (CompileError "arrays of atomic types are not supported" Nothing Nothing)
          let elemAlign = layoutAlign firstLayout
          let elemSize = layoutSize firstLayout
          let stride = roundUp elemSize elemAlign
          let total = stride * fromIntegral (length vals')
          let layout = TLArray (Just (length vals)) stride firstLayout elemAlign total
          let (cid, st3) = emitConstComposite layout (map valId vals') st2
          Right (st3, Value layout cid)

emitConstScalarCtor :: Scalar -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstScalarCtor scalar args st =
  case args of
    [arg] -> do
      (st1, val) <- emitConstExpr st arg
      case valType val of
        TLScalar s _ _ | s == scalar -> Right (st1, val)
        TLScalar _ _ _ ->
          case lookupConstKeyById st1 (valId val) of
            Nothing -> Left (CompileError "scalar constant cast requires a literal value" Nothing Nothing)
            Just key -> do
              key' <- convertConstKey key scalar
              let (cid, st2) = emitConstFromKey st1 key'
              let (a, sz) = scalarLayout scalar
              let layout = TLScalar scalar a sz
              Right (st2, Value layout cid)
        _ -> Left (CompileError "scalar constant cast requires a scalar argument" Nothing Nothing)
    _ -> Left (CompileError "scalar constant cast requires a single argument" Nothing Nothing)

lookupConstKeyById :: GenState -> Word32 -> Maybe ConstKey
lookupConstKeyById st cid =
  case [key | (key, cid') <- gsConstCache st, cid' == cid] of
    (k:_) -> Just k
    [] -> Nothing

emitConstFromKey :: GenState -> ConstKey -> (Word32, GenState)
emitConstFromKey st key =
  case key of
    ConstU32 v -> emitConstU32 st v
    ConstI32 v -> emitConstI32 st v
    ConstF32 bits -> emitConstF32Bits st bits
    ConstF16 bits -> emitConstF16Bits st bits
    ConstBool b -> emitConstBool st b

convertConstKey :: ConstKey -> Scalar -> Either CompileError ConstKey
convertConstKey key target =
  case target of
    U32 -> ConstU32 <$> constKeyToU32 key
    I32 -> ConstI32 . fromIntegral <$> constKeyToI32 key
    F32 -> ConstF32 . castFloatToWord32 <$> constKeyToFloat key
    F16 -> ConstF16 . floatToHalfBits <$> constKeyToFloat key
    Bool ->
      case key of
        ConstBool b -> Right (ConstBool b)
        _ -> Left (CompileError "bool constant cast requires a bool literal" Nothing Nothing)

constKeyToFloat :: ConstKey -> Either CompileError Float
constKeyToFloat key =
  case key of
    ConstF32 bits -> Right (castWord32ToFloat bits)
    ConstF16 bits -> Right (halfBitsToFloat bits)
    ConstU32 v -> Right (fromIntegral v)
    ConstI32 v -> Right (fromIntegral (fromIntegral v :: Int32))
    ConstBool _ -> Left (CompileError "cannot convert bool literal to float" Nothing Nothing)

constKeyToI32 :: ConstKey -> Either CompileError Int32
constKeyToI32 key =
  case key of
    ConstI32 v -> Right (fromIntegral v)
    ConstU32 v -> Right (fromIntegral v)
    ConstF32 bits -> Right (truncate (castWord32ToFloat bits))
    ConstF16 bits -> Right (truncate (halfBitsToFloat bits))
    ConstBool _ -> Left (CompileError "cannot convert bool literal to i32" Nothing Nothing)

constKeyToU32 :: ConstKey -> Either CompileError Word32
constKeyToU32 key =
  case key of
    ConstU32 v -> Right v
    ConstI32 v -> Right (fromIntegral (fromIntegral v :: Int32))
    ConstF32 bits -> Right (fromIntegral (truncate (castWord32ToFloat bits) :: Integer))
    ConstF16 bits -> Right (fromIntegral (truncate (halfBitsToFloat bits) :: Integer))
    ConstBool _ -> Left (CompileError "cannot convert bool literal to u32" Nothing Nothing)

resolveStructLayout :: [(Text, StructDecl)] -> StructDecl -> Either CompileError (Text, TypeLayout)
resolveStructLayout env decl = do
  layout <- resolveTypeLayout env [] (TyStructRef (sdName decl))
  Right (sdName decl, layout)

-- SPIR-V builder types

data Instr = Instr Word16 [Word32] deriving (Eq, Show)

encodeInstr :: Instr -> [Word32]
encodeInstr (Instr opcode ops) =
  let wc = 1 + length ops
      first = (fromIntegral wc `shiftL` 16) .|. fromIntegral opcode
  in first : ops

-- Minimal subset of opcodes and enums

opCapability :: Word16
opCapability = 17

opMemoryModel :: Word16
opMemoryModel = 14

opEntryPoint :: Word16
opEntryPoint = 15

opExtInstImport :: Word16
opExtInstImport = 11

opExtInst :: Word16
opExtInst = 12

opExecutionMode :: Word16
opExecutionMode = 16

opName :: Word16
opName = 5

opMemberName :: Word16
opMemberName = 6

opDecorate :: Word16
opDecorate = 71

opMemberDecorate :: Word16
opMemberDecorate = 72

opTypeVoid :: Word16
opTypeVoid = 19

opTypeBool :: Word16
opTypeBool = 20

opTypeInt :: Word16
opTypeInt = 21

opTypeFloat :: Word16
opTypeFloat = 22

opTypeVector :: Word16
opTypeVector = 23

opTypeMatrix :: Word16
opTypeMatrix = 24

opTypeImage :: Word16
opTypeImage = 25

opTypeSampler :: Word16
opTypeSampler = 26

opTypeSampledImage :: Word16
opTypeSampledImage = 27

opTypeArray :: Word16
opTypeArray = 28

opTypeRuntimeArray :: Word16
opTypeRuntimeArray = 29

opTypeStruct :: Word16
opTypeStruct = 30

opTypePointer :: Word16
opTypePointer = 32

opTypeFunction :: Word16
opTypeFunction = 33

opConstantTrue :: Word16
opConstantTrue = 41

opConstantFalse :: Word16
opConstantFalse = 42

opConstant :: Word16
opConstant = 43

opConstantComposite :: Word16
opConstantComposite = 44

opSpecConstantTrue :: Word16
opSpecConstantTrue = 48

opSpecConstantFalse :: Word16
opSpecConstantFalse = 49

opSpecConstant :: Word16
opSpecConstant = 50

opSpecConstantComposite :: Word16
opSpecConstantComposite = 51

opSpecConstantOp :: Word16
opSpecConstantOp = 52


opLoad :: Word16
opLoad = 61

opStore :: Word16
opStore = 62

opAccessChain :: Word16
opAccessChain = 65

opArrayLength :: Word16
opArrayLength = 68

opVariable :: Word16
opVariable = 59

opFunction :: Word16
opFunction = 54

opFunctionParameter :: Word16
opFunctionParameter = 55

opLabel :: Word16
opLabel = 248

opReturn :: Word16
opReturn = 253

opReturnValue :: Word16
opReturnValue = 254

opKill :: Word16
opKill = 252

opFunctionCall :: Word16
opFunctionCall = 57

opFunctionEnd :: Word16
opFunctionEnd = 56

opUnreachable :: Word16
opUnreachable = 255

opCompositeConstruct :: Word16
opCompositeConstruct = 80

opVectorShuffle :: Word16
opVectorShuffle = 79

opCompositeExtract :: Word16
opCompositeExtract = 81

opTranspose :: Word16
opTranspose = 84

opSampledImage :: Word16
opSampledImage = 86

opImageSampleImplicitLod :: Word16
opImageSampleImplicitLod = 87

opImageSampleExplicitLod :: Word16
opImageSampleExplicitLod = 88

opImageSampleDrefImplicitLod :: Word16
opImageSampleDrefImplicitLod = 89

opImageSampleDrefExplicitLod :: Word16
opImageSampleDrefExplicitLod = 90

opImageFetch :: Word16
opImageFetch = 95

opImageGather :: Word16
opImageGather = 96

opImageDrefGather :: Word16
opImageDrefGather = 97

opImageRead :: Word16
opImageRead = 98

opImageWrite :: Word16
opImageWrite = 99

opImageQuerySizeLod :: Word16
opImageQuerySizeLod = 103

opImageQuerySize :: Word16
opImageQuerySize = 104

opImageQueryLevels :: Word16
opImageQueryLevels = 106

opImageQuerySamples :: Word16
opImageQuerySamples = 107

opSNegate :: Word16
opSNegate = 126

opFNegate :: Word16
opFNegate = 127

opConvertFToU :: Word16
opConvertFToU = 109

opConvertFToS :: Word16
opConvertFToS = 110

opConvertSToF :: Word16
opConvertSToF = 111

opConvertUToF :: Word16
opConvertUToF = 112

opFConvert :: Word16
opFConvert = 115

opBitcast :: Word16
opBitcast = 124

opIAdd :: Word16
opIAdd = 128

opFAdd :: Word16
opFAdd = 129

opISub :: Word16
opISub = 130

opFSub :: Word16
opFSub = 131

opIMul :: Word16
opIMul = 132

opFMul :: Word16
opFMul = 133

opUDiv :: Word16
opUDiv = 134

opSDiv :: Word16
opSDiv = 135

opFDiv :: Word16
opFDiv = 136

opUMod :: Word16
opUMod = 137

opSRem :: Word16
opSRem = 138

opDot :: Word16
opDot = 148

opAny :: Word16
opAny = 154

opAll :: Word16
opAll = 155

opLogicalEqual :: Word16
opLogicalEqual = 164

opLogicalNotEqual :: Word16
opLogicalNotEqual = 165

opLogicalOr :: Word16
opLogicalOr = 166

opLogicalAnd :: Word16
opLogicalAnd = 167

opLogicalNot :: Word16
opLogicalNot = 168

opSelect :: Word16
opSelect = 169

opIEqual :: Word16
opIEqual = 170

opINotEqual :: Word16
opINotEqual = 171

opUGreaterThan :: Word16
opUGreaterThan = 172

opSGreaterThan :: Word16
opSGreaterThan = 173

opUGreaterThanEqual :: Word16
opUGreaterThanEqual = 174

opSGreaterThanEqual :: Word16
opSGreaterThanEqual = 175

opULessThan :: Word16
opULessThan = 176

opSLessThan :: Word16
opSLessThan = 177

opULessThanEqual :: Word16
opULessThanEqual = 178

opSLessThanEqual :: Word16
opSLessThanEqual = 179

opFOrdEqual :: Word16
opFOrdEqual = 180

opFOrdNotEqual :: Word16
opFOrdNotEqual = 182

opFOrdLessThan :: Word16
opFOrdLessThan = 184

opFOrdGreaterThan :: Word16
opFOrdGreaterThan = 186

opFOrdLessThanEqual :: Word16
opFOrdLessThanEqual = 188

opFOrdGreaterThanEqual :: Word16
opFOrdGreaterThanEqual = 190

opShiftRightLogical :: Word16
opShiftRightLogical = 194

opShiftRightArithmetic :: Word16
opShiftRightArithmetic = 195

opShiftLeftLogical :: Word16
opShiftLeftLogical = 196

opBitwiseOr :: Word16
opBitwiseOr = 197

opBitwiseXor :: Word16
opBitwiseXor = 198

opBitwiseAnd :: Word16
opBitwiseAnd = 199

opBitFieldInsert :: Word16
opBitFieldInsert = 201

opBitFieldSExtract :: Word16
opBitFieldSExtract = 202

opBitFieldUExtract :: Word16
opBitFieldUExtract = 203

opBitReverse :: Word16
opBitReverse = 204

opBitCount :: Word16
opBitCount = 205

opDPdx :: Word16
opDPdx = 207

opDPdy :: Word16
opDPdy = 208

opFwidth :: Word16
opFwidth = 209

opAtomicLoad :: Word16
opAtomicLoad = 227

opAtomicStore :: Word16
opAtomicStore = 228

opAtomicIAdd :: Word16
opAtomicIAdd = 234

opAtomicISub :: Word16
opAtomicISub = 235

opAtomicSMin :: Word16
opAtomicSMin = 236

opAtomicUMin :: Word16
opAtomicUMin = 237

opAtomicSMax :: Word16
opAtomicSMax = 238

opAtomicUMax :: Word16
opAtomicUMax = 239

opAtomicAnd :: Word16
opAtomicAnd = 240

opAtomicOr :: Word16
opAtomicOr = 241

opAtomicXor :: Word16
opAtomicXor = 242

opAtomicExchange :: Word16
opAtomicExchange = 229

opLoopMerge :: Word16
opLoopMerge = 246

opSelectionMerge :: Word16
opSelectionMerge = 247

opBranch :: Word16
opBranch = 249

opBranchConditional :: Word16
opBranchConditional = 250

capabilityShader :: Word32
capabilityShader = 1

capabilityFloat16 :: Word32
capabilityFloat16 = 9

capabilitySampled1D :: Word32
capabilitySampled1D = 43

capabilityImage1D :: Word32
capabilityImage1D = 44

capabilityImageQuery :: Word32
capabilityImageQuery = 50

addressingLogical :: Word32
addressingLogical = 0

memoryModelGLSL450 :: Word32
memoryModelGLSL450 = 1

memoryScopeDevice :: Word32
memoryScopeDevice = 1

memorySemanticsRelaxed :: Word32
memorySemanticsRelaxed = 0

executionModelGLCompute :: Word32
executionModelGLCompute = 5

executionModelFragment :: Word32
executionModelFragment = 4

executionModelVertex :: Word32
executionModelVertex = 0

executionModeLocalSize :: Word32
executionModeLocalSize = 17

executionModeOriginUpperLeft :: Word32
executionModeOriginUpperLeft = 7

storageClassInput :: Word32
storageClassInput = 1

storageClassUniformConstant :: Word32
storageClassUniformConstant = 0

storageClassUniform :: Word32
storageClassUniform = 2

storageClassOutput :: Word32
storageClassOutput = 3

storageClassStorageBuffer :: Word32
storageClassStorageBuffer = 12

storageClassFunction :: Word32
storageClassFunction = 7

storageClassWorkgroup :: Word32
storageClassWorkgroup = 4

storageClassPrivate :: Word32
storageClassPrivate = 6

decorationBlock :: Word32
decorationBlock = 2

decorationSpecId :: Word32
decorationSpecId = 1

decorationArrayStride :: Word32
decorationArrayStride = 6

decorationDescriptorSet :: Word32
decorationDescriptorSet = 34

decorationBinding :: Word32
decorationBinding = 33

decorationOffset :: Word32
decorationOffset = 35

decorationBuiltIn :: Word32
decorationBuiltIn = 11

decorationLocation :: Word32
decorationLocation = 30

builtInGlobalInvocationId :: Word32
builtInGlobalInvocationId = 28

builtInLocalInvocationId :: Word32
builtInLocalInvocationId = 27

builtInLocalInvocationIndex :: Word32
builtInLocalInvocationIndex = 29

builtInWorkgroupId :: Word32
builtInWorkgroupId = 26

builtInNumWorkgroups :: Word32
builtInNumWorkgroups = 24

builtInPosition :: Word32
builtInPosition = 0

builtInVertexIndex :: Word32
builtInVertexIndex = 42

builtInInstanceIndex :: Word32
builtInInstanceIndex = 43

builtInFragCoord :: Word32
builtInFragCoord = 15

builtInFrontFacing :: Word32
builtInFrontFacing = 17

builtInSampleIndex :: Word32
builtInSampleIndex = 18

builtInFragDepth :: Word32
builtInFragDepth = 22

dim1D :: Word32
dim1D = 0

dim2D :: Word32
dim2D = 1

dim3D :: Word32
dim3D = 2

dimCube :: Word32
dimCube = 3

imageSampled :: Word32
imageSampled = 1

imageStorage :: Word32
imageStorage = 2

imageOperandsBias :: Word32
imageOperandsBias = 1

imageOperandsLod :: Word32
imageOperandsLod = 2

imageOperandsGrad :: Word32
imageOperandsGrad = 4

imageOperandsSample :: Word32
imageOperandsSample = 64

imageFormatUnknown :: Word32
imageFormatUnknown = 0

imageFormatRgba32f :: Word32
imageFormatRgba32f = 1

imageFormatRgba16f :: Word32
imageFormatRgba16f = 2

imageFormatR32f :: Word32
imageFormatR32f = 3

imageFormatRgba8 :: Word32
imageFormatRgba8 = 4

imageFormatRgba8Snorm :: Word32
imageFormatRgba8Snorm = 5

imageFormatRg32f :: Word32
imageFormatRg32f = 6

imageFormatRg16f :: Word32
imageFormatRg16f = 7

imageFormatR11fG11fB10f :: Word32
imageFormatR11fG11fB10f = 8

imageFormatR16f :: Word32
imageFormatR16f = 9

imageFormatRgba16 :: Word32
imageFormatRgba16 = 10

imageFormatRgb10A2 :: Word32
imageFormatRgb10A2 = 11

imageFormatRg16 :: Word32
imageFormatRg16 = 12

imageFormatRg8 :: Word32
imageFormatRg8 = 13

imageFormatR16 :: Word32
imageFormatR16 = 14

imageFormatR8 :: Word32
imageFormatR8 = 15

imageFormatRgba16Snorm :: Word32
imageFormatRgba16Snorm = 16

imageFormatRg16Snorm :: Word32
imageFormatRg16Snorm = 17

imageFormatRg8Snorm :: Word32
imageFormatRg8Snorm = 18

imageFormatR16Snorm :: Word32
imageFormatR16Snorm = 19

imageFormatR8Snorm :: Word32
imageFormatR8Snorm = 20

imageFormatRgba32i :: Word32
imageFormatRgba32i = 21

imageFormatRgba16i :: Word32
imageFormatRgba16i = 22

imageFormatRgba8i :: Word32
imageFormatRgba8i = 23

imageFormatR32i :: Word32
imageFormatR32i = 24

imageFormatRg32i :: Word32
imageFormatRg32i = 25

imageFormatRg16i :: Word32
imageFormatRg16i = 26

imageFormatRg8i :: Word32
imageFormatRg8i = 27

imageFormatR16i :: Word32
imageFormatR16i = 28

imageFormatR8i :: Word32
imageFormatR8i = 29

imageFormatRgba32ui :: Word32
imageFormatRgba32ui = 30

imageFormatRgba16ui :: Word32
imageFormatRgba16ui = 31

imageFormatRgba8ui :: Word32
imageFormatRgba8ui = 32

imageFormatR32ui :: Word32
imageFormatR32ui = 33

imageFormatRgb10a2ui :: Word32
imageFormatRgb10a2ui = 34

imageFormatRg32ui :: Word32
imageFormatRg32ui = 35

imageFormatRg16ui :: Word32
imageFormatRg16ui = 36

imageFormatRg8ui :: Word32
imageFormatRg8ui = 37

imageFormatR16ui :: Word32
imageFormatR16ui = 38

imageFormatR8ui :: Word32
imageFormatR8ui = 39

functionControlNone :: Word32
functionControlNone = 0

selectionControlNone :: Word32
selectionControlNone = 0

loopControlNone :: Word32
loopControlNone = 0

glslStd450FAbs :: Word32
glslStd450FAbs = 4

glslStd450Round :: Word32
glslStd450Round = 1

glslStd450RoundEven :: Word32
glslStd450RoundEven = 2

glslStd450Trunc :: Word32
glslStd450Trunc = 3

glslStd450Floor :: Word32
glslStd450Floor = 8

glslStd450Ceil :: Word32
glslStd450Ceil = 9

glslStd450Fract :: Word32
glslStd450Fract = 10

glslStd450Radians :: Word32
glslStd450Radians = 11

glslStd450Degrees :: Word32
glslStd450Degrees = 12

glslStd450Sin :: Word32
glslStd450Sin = 13

glslStd450Cos :: Word32
glslStd450Cos = 14

glslStd450Tan :: Word32
glslStd450Tan = 15

glslStd450Asin :: Word32
glslStd450Asin = 16

glslStd450Acos :: Word32
glslStd450Acos = 17

glslStd450Atan :: Word32
glslStd450Atan = 18

glslStd450Sinh :: Word32
glslStd450Sinh = 19

glslStd450Cosh :: Word32
glslStd450Cosh = 20

glslStd450Tanh :: Word32
glslStd450Tanh = 21

glslStd450Asinh :: Word32
glslStd450Asinh = 22

glslStd450Acosh :: Word32
glslStd450Acosh = 23

glslStd450Atanh :: Word32
glslStd450Atanh = 24

glslStd450Atan2 :: Word32
glslStd450Atan2 = 25

glslStd450Pow :: Word32
glslStd450Pow = 26

glslStd450Exp :: Word32
glslStd450Exp = 27

glslStd450Log :: Word32
glslStd450Log = 28

glslStd450Exp2 :: Word32
glslStd450Exp2 = 29

glslStd450Log2 :: Word32
glslStd450Log2 = 30

glslStd450Sqrt :: Word32
glslStd450Sqrt = 31

glslStd450InverseSqrt :: Word32
glslStd450InverseSqrt = 32

glslStd450Determinant :: Word32
glslStd450Determinant = 33

glslStd450MatrixInverse :: Word32
glslStd450MatrixInverse = 34

glslStd450ModfStruct :: Word32
glslStd450ModfStruct = 36

glslStd450FMin :: Word32
glslStd450FMin = 37

glslStd450FMax :: Word32
glslStd450FMax = 40

glslStd450FClamp :: Word32
glslStd450FClamp = 43

glslStd450FMix :: Word32
glslStd450FMix = 46

glslStd450Step :: Word32
glslStd450Step = 48

glslStd450SmoothStep :: Word32
glslStd450SmoothStep = 49

glslStd450Fma :: Word32
glslStd450Fma = 50

glslStd450FrexpStruct :: Word32
glslStd450FrexpStruct = 52

glslStd450Ldexp :: Word32
glslStd450Ldexp = 53

glslStd450PackSnorm4x8 :: Word32
glslStd450PackSnorm4x8 = 54

glslStd450PackUnorm4x8 :: Word32
glslStd450PackUnorm4x8 = 55

glslStd450PackSnorm2x16 :: Word32
glslStd450PackSnorm2x16 = 56

glslStd450PackUnorm2x16 :: Word32
glslStd450PackUnorm2x16 = 57

glslStd450PackHalf2x16 :: Word32
glslStd450PackHalf2x16 = 58

glslStd450UnpackSnorm2x16 :: Word32
glslStd450UnpackSnorm2x16 = 60

glslStd450UnpackUnorm2x16 :: Word32
glslStd450UnpackUnorm2x16 = 61

glslStd450UnpackHalf2x16 :: Word32
glslStd450UnpackHalf2x16 = 62

glslStd450UnpackSnorm4x8 :: Word32
glslStd450UnpackSnorm4x8 = 63

glslStd450UnpackUnorm4x8 :: Word32
glslStd450UnpackUnorm4x8 = 64

glslStd450Length :: Word32
glslStd450Length = 66

glslStd450Cross :: Word32
glslStd450Cross = 68

glslStd450Normalize :: Word32
glslStd450Normalize = 69

glslStd450FaceForward :: Word32
glslStd450FaceForward = 70

glslStd450Refract :: Word32
glslStd450Refract = 72

glslStd450FindILsb :: Word32
glslStd450FindILsb = 73

glslStd450FindSMsb :: Word32
glslStd450FindSMsb = 74

glslStd450FindUMsb :: Word32
glslStd450FindUMsb = 75

-- Generator state

data GenState = GenState
  { gsNextId :: Word32
  , gsStructLayouts :: [(Text, TypeLayout)]
  , gsStructIds :: [(Text, Word32)]
  , gsBlockStructs :: [Text]
  , gsTypeCache :: [(TypeKey, Word32)]
  , gsConstCache :: [(ConstKey, Word32)]
  , gsExtInstIds :: [(String, Word32)]
  , gsExtInstImports :: [Instr]
  , gsGlobalVars :: [(Text, VarInfo)]
  , gsFunctionTable :: [FunctionInfo]
  , gsEntryStage :: Stage
  , gsConstValues :: [(Text, Value)]
  , gsCapabilities :: [Word32]
  , gsNames :: [Instr]
  , gsDecorations :: [Instr]
  , gsTypes :: [Instr]
  , gsConstants :: [Instr]
  , gsGlobals :: [Instr]
  , gsFunctions :: [Instr]
  , gsEntryPoint :: Maybe Word32
  , gsInterfaceIds :: [Word32]
  }

emptyGenState :: Stage -> [(Text, TypeLayout)] -> [Text] -> GenState
emptyGenState stage structLayouts blockStructs =
  let (ids, nextId) = assignStructIds 1 structLayouts
  in GenState
      { gsNextId = nextId
      , gsStructLayouts = structLayouts
      , gsStructIds = ids
      , gsBlockStructs = blockStructs
      , gsTypeCache = []
      , gsConstCache = []
      , gsExtInstIds = []
      , gsExtInstImports = []
      , gsGlobalVars = []
      , gsFunctionTable = []
      , gsEntryStage = stage
      , gsConstValues = []
      , gsCapabilities = []
      , gsNames = []
      , gsDecorations = []
      , gsTypes = []
      , gsConstants = []
      , gsGlobals = []
      , gsFunctions = []
      , gsEntryPoint = Nothing
      , gsInterfaceIds = []
      }

assignStructIds :: Word32 -> [(Text, TypeLayout)] -> ([(Text, Word32)], Word32)
assignStructIds start layouts =
  let go next acc [] = (reverse acc, next)
      go next acc ((name, _):rest) = go (next + 1) ((name, next):acc) rest
  in go start [] layouts

freshId :: GenState -> (Word32, GenState)
freshId st = (gsNextId st, st { gsNextId = gsNextId st + 1 })

addInstr :: (GenState -> [Instr]) -> (GenState -> [Instr] -> GenState) -> Instr -> GenState -> GenState
addInstr getter setter instr st =
  let xs = getter st
  in setter st (xs <> [instr])

addCapability :: Word32 -> GenState -> GenState
addCapability cap st =
  if cap `elem` gsCapabilities st
    then st
    else st { gsCapabilities = gsCapabilities st <> [cap] }

addName :: Instr -> GenState -> GenState
addName = addInstr gsNames (\st v -> st { gsNames = v })

addDecoration :: Instr -> GenState -> GenState
addDecoration = addInstr gsDecorations (\st v -> st { gsDecorations = v })

addType :: Instr -> GenState -> GenState
addType = addInstr gsTypes (\st v -> st { gsTypes = v })

addConst :: Instr -> GenState -> GenState
addConst = addInstr gsConstants (\st v -> st { gsConstants = v })

addGlobal :: Instr -> GenState -> GenState
addGlobal = addInstr gsGlobals (\st v -> st { gsGlobals = v })

-- Emit struct types with member decorations
emitStructs :: GenState -> ((), GenState)
emitStructs st0 =
  let st1 = foldl' emitStruct st0 (gsStructLayouts st0)
  in ((), st1)
  where
    emitStruct st (name, layout) =
      case layout of
        TLStruct _ fields _ _ ->
          let structId = fromMaybe (error "missing struct id") (lookup name (gsStructIds st))
              (st1, fieldTypeIds) = mapAccumL emitFieldType st fields
              st2 = addType (Instr opTypeStruct (structId : fieldTypeIds)) st1
              st3 = addName (Instr opName (structId : encodeString (textToString name))) st2
              st4 = foldl' (emitMemberDecorate structId) st3 (zip [0 :: Int ..] fields)
              st5 = if name `elem` gsBlockStructs st4
                then addDecoration (Instr opDecorate [structId, decorationBlock]) st4
                else st4
              st6 = foldl' (emitMemberName structId) st5 (zip [0 :: Int ..] fields)
          in st6
        _ -> st

    emitFieldType st field =
      let (tyId, st') = emitTypeFromLayout st (flType field)
      in (st', tyId)

    emitMemberDecorate structId st (ix, field) =
      let offset = flOffset field
      in addDecoration (Instr opMemberDecorate [structId, fromIntegral ix, decorationOffset, offset]) st

    emitMemberName structId st (ix, field) =
      addName (Instr opMemberName (structId : fromIntegral ix : encodeString (flName field))) st

data VarAccess = ReadOnly | ReadWrite
  deriving (Eq, Show)

ptrAccessAllowsWrite :: Maybe StorageAccess -> Bool
ptrAccessAllowsWrite acc =
  case acc of
    Nothing -> True
    Just StorageRead -> False
    Just StorageReadWrite -> True
    Just StorageWrite -> True

ptrAccessToVarAccess :: Maybe StorageAccess -> VarAccess
ptrAccessToVarAccess acc =
  if ptrAccessAllowsWrite acc then ReadWrite else ReadOnly

varAccessToPtrAccess :: VarAccess -> Maybe StorageAccess
varAccessToPtrAccess acc =
  case acc of
    ReadOnly -> Just StorageRead
    ReadWrite -> Just StorageReadWrite

data VarInfo = VarInfo
  { viType :: !TypeLayout
  , viPtrId :: !Word32
  , viStorage :: !Word32
  , viAccess :: !VarAccess
  } deriving (Eq, Show)

data EntryFieldInit = EntryFieldInit
  { efiLayout :: !TypeLayout
  , efiVar :: !VarInfo
  } deriving (Eq, Show)

data EntryParamInit = EntryParamInit
  { epiName :: !Text
  , epiLayout :: !TypeLayout
  , epiFields :: ![EntryFieldInit]
  } deriving (Eq, Show)

data OutputTarget = OutputTarget
  { otVar :: !VarInfo
  , otLayout :: !TypeLayout
  , otPath :: ![Word32]
  } deriving (Eq, Show)

data Value = Value
  { valType :: !TypeLayout
  , valId :: !Word32
  } deriving (Eq, Show)

data FunctionInfo = FunctionInfo
  { fiName :: !Text
  , fiParams :: ![TypeLayout]
  , fiReturn :: !(Maybe TypeLayout)
  , fiId :: !Word32
  , fiTypeId :: !Word32
  } deriving (Eq, Show)

data FuncState = FuncState
  { fsLocals :: ![Instr]
  , fsInstrs :: ![Instr]
  , fsVars :: ![(Text, VarInfo)]
  , fsValues :: ![(Text, Value)]
  , fsTerminated :: !Bool
  , fsLoopStack :: ![(Word32, Word32)]
  , fsBreakStack :: ![Word32]
  } deriving (Eq, Show)

emitGlobals :: [(Text, StructDecl)] -> ShaderInterface -> EntryPoint -> Maybe TypeLayout -> [GlobalVarDecl] -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], [OutputTarget], GenState)
emitGlobals structEnv iface entry retLayout globals st0 = do
  let (envBindings, idsBindings, st1) = foldl' emitBinding ([], [], st0) (siBindings iface)
  (envGlobals, st2) <- emitModuleGlobals structEnv globals st1
  (envInputs, entryInits, idsInputs, st3) <- emitEntryInputs structEnv entry st2
  (outTargets, idsOut, st4) <- emitStageOutput structEnv entry retLayout st3
  let envAll = envBindings <> envGlobals <> envInputs
  let idsGlobals = map (viPtrId . snd) envGlobals
  let ifaceIds = idsBindings <> idsInputs <> idsOut <> idsGlobals
  let st5 = st4 { gsInterfaceIds = ifaceIds, gsGlobalVars = envBindings <> envGlobals }
  pure (envAll, entryInits, ifaceIds, outTargets, st5)
  where
    emitBinding (envAcc, idAcc, st) binding =
      let (ptrTy, st1) = emitPointerForBinding st (biKind binding) (biType binding)
          (varId, st2) = freshId st1
          storageClass = case biKind binding of
            BUniform -> storageClassUniform
            BStorageRead -> storageClassStorageBuffer
            BStorageReadWrite -> storageClassStorageBuffer
            BSampler -> storageClassUniformConstant
            BSamplerComparison -> storageClassUniformConstant
            BTexture1D -> storageClassUniformConstant
            BTexture1DArray -> storageClassUniformConstant
            BTexture2D -> storageClassUniformConstant
            BTexture2DArray -> storageClassUniformConstant
            BTexture3D -> storageClassUniformConstant
            BTextureCube -> storageClassUniformConstant
            BTextureCubeArray -> storageClassUniformConstant
            BTextureMultisampled2D -> storageClassUniformConstant
            BTextureDepth2D -> storageClassUniformConstant
            BTextureDepth2DArray -> storageClassUniformConstant
            BTextureDepthCube -> storageClassUniformConstant
            BTextureDepthCubeArray -> storageClassUniformConstant
            BTextureDepthMultisampled2D -> storageClassUniformConstant
            BStorageTexture1D -> storageClassUniformConstant
            BStorageTexture2D -> storageClassUniformConstant
            BStorageTexture2DArray -> storageClassUniformConstant
            BStorageTexture3D -> storageClassUniformConstant
          st3 = addGlobal (Instr opVariable [ptrTy, varId, storageClass]) st2
          st4 = addDecoration (Instr opDecorate [varId, decorationDescriptorSet, biGroup binding]) st3
          st5 = addDecoration (Instr opDecorate [varId, decorationBinding, biBinding binding]) st4
          st6 = addName (Instr opName (varId : encodeString (biName binding))) st5
          access = case biKind binding of
            BStorageReadWrite -> ReadWrite
            _ -> ReadOnly
          info = VarInfo (biType binding) varId storageClass access
      in (envAcc <> [(T.pack (biName binding), info)], idAcc <> [varId], st6)

emitModuleGlobals :: [(Text, StructDecl)] -> [GlobalVarDecl] -> GenState -> Either CompileError ([(Text, VarInfo)], GenState)
emitModuleGlobals structEnv decls st0 = foldM emitOne ([], st0) decls
  where
    emitOne (envAcc, st) decl = do
      layout <- resolveTypeLayout structEnv [] (gvType decl)
      when (containsResource layout) $
        Left (CompileError "global variables cannot contain resource types" Nothing Nothing)
      when (containsAtomic layout) $
        Left (CompileError "atomic types are only supported in storage buffers for now" Nothing Nothing)
      when (containsRuntimeArray layout) $
        Left (CompileError "runtime arrays are only supported in storage buffers" Nothing Nothing)
      storageClass <- case gvSpace decl of
        "private" -> Right storageClassPrivate
        "workgroup" -> Right storageClassWorkgroup
        other -> Left (CompileError ("unsupported global address space: " <> textToString other) Nothing Nothing)
      (initId, st1) <- case gvInit decl of
        Nothing -> Right (Nothing, st)
        Just expr -> do
          when (storageClass == storageClassWorkgroup) $
            Left (CompileError "workgroup variables cannot have initializers" Nothing Nothing)
          (st2, val) <- emitConstExpr st expr
          (st3, val') <- coerceConstValueToLayout layout val st2
          Right (Just (valId val'), st3)
      let (baseTy, st2) = emitTypeFromLayout st1 layout
      let (ptrTy, st3) = emitPointerType st2 storageClass baseTy
      let (varId, st4) = freshId st3
      let operands =
            case initId of
              Nothing -> [ptrTy, varId, storageClass]
              Just cid -> [ptrTy, varId, storageClass, cid]
      let st5 = addGlobal (Instr opVariable operands) st4
      let st6 = addName (Instr opName (varId : encodeString (textToString (gvName decl)))) st5
      let info = VarInfo layout varId storageClass ReadWrite
      Right (envAcc <> [(gvName decl, info)], st6)

emitEntryInputs :: [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitEntryInputs structEnv entry st0 =
  case epStage entry of
    StageCompute -> emitComputeInputs structEnv entry st0
    StageFragment -> emitFragmentInputs structEnv entry st0
    StageVertex -> emitVertexInputs structEnv entry st0

emitComputeInputs :: [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitComputeInputs structEnv entry st0 =
  emitStageInputs StageCompute structEnv entry st0

emitFragmentInputs :: [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitFragmentInputs structEnv entry st0 = do
  emitStageInputs StageFragment structEnv entry st0

emitVertexInputs :: [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitVertexInputs structEnv entry st0 =
  emitStageInputs StageVertex structEnv entry st0

data InputDecoration = InputBuiltin Word32 | InputLocation Word32

emitStageInputs :: Stage -> [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitStageInputs stage structEnv entry st0 = do
  let params = epParams entry
  let go envAcc initAcc idAcc _ _ st [] =
        Right (reverse envAcc, reverse initAcc, reverse idAcc, st)
      go envAcc initAcc idAcc usedLocs usedBuiltins st (param:rest) = do
        (envNext, initNext, idNext, usedLocsNext, usedBuiltinsNext, stNext) <- emitParam stage structEnv param (envAcc, initAcc, idAcc, usedLocs, usedBuiltins, st)
        go envNext initNext idNext usedLocsNext usedBuiltinsNext stNext rest
  go [] [] [] [] [] st0 params
  where
    emitParam stg env (Param name ty attrs) (envAcc, initAcc, idAcc, usedLocs, usedBuiltins, st) =
      case ty of
        TyStructRef structName -> do
          when (paramBuiltin attrs /= Nothing || paramLocation attrs /= Nothing) $
            Left (CompileError "struct parameters cannot have @location or @builtin attributes" Nothing Nothing)
          structDecl <- case lookup structName env of
            Nothing -> Left (CompileError ("unknown struct: " <> textToString structName) Nothing Nothing)
            Just decl -> Right decl
          structLayout <- resolveTypeLayout env [] ty
          when (containsResource structLayout) $
            Left (CompileError "resource types are not allowed as stage inputs" Nothing Nothing)
          when (containsAtomic structLayout) $
            Left (CompileError "atomic types are not allowed as stage inputs" Nothing Nothing)
          let fields = sdFields structDecl
          (fieldVars, usedLocs', usedBuiltins', ids', st') <- emitStructFields stg name env fields usedLocs usedBuiltins idAcc st
          let initEntry = EntryParamInit name structLayout fieldVars
          pure (envAcc, initEntry:initAcc, ids', usedLocs', usedBuiltins', st')
        _ -> do
          layout <- resolveTypeLayout env [] ty
          when (containsResource layout) $
            Left (CompileError "resource types are not allowed as stage inputs" Nothing Nothing)
          when (containsAtomic layout) $
            Left (CompileError "atomic types are not allowed as stage inputs" Nothing Nothing)
          when (paramBuiltin attrs /= Nothing && paramLocation attrs /= Nothing) $
            Left (CompileError "parameters cannot have both @location and @builtin" Nothing Nothing)
          case paramBuiltin attrs of
            Just builtin -> do
              when (builtin `elem` usedBuiltins) $
                Left (CompileError ("duplicate @builtin(" <> textToString builtin <> ") on stage inputs") Nothing Nothing)
              expected <- case builtinInputType stg builtin of
                Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage input") Nothing Nothing)
                Just ty' -> Right ty'
              if expected /= ty
                then Left (CompileError ("@builtin(" <> textToString builtin <> ") has wrong type") Nothing Nothing)
                else pure ()
              builtinId <- case builtinInputDecoration stg builtin of
                Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage input") Nothing Nothing)
                Just bid -> Right bid
              (info, varId, st1) <- emitInputVar name layout (InputBuiltin builtinId) st
              pure ((name, info):envAcc, initAcc, varId:idAcc, usedLocs, builtin:usedBuiltins, st1)
            Nothing -> do
              loc <- case paramLocation attrs of
                Nothing ->
                  case stg of
                    StageCompute -> Left (CompileError "compute parameters must use @builtin" Nothing Nothing)
                    _ -> Left (CompileError "stage parameters must use @location or @builtin" Nothing Nothing)
                Just n -> Right n
              when (stg == StageCompute) $
                Left (CompileError "compute parameters cannot use @location" Nothing Nothing)
              when (loc `elem` usedLocs) $
                Left (CompileError "duplicate @location on stage inputs" Nothing Nothing)
              (info, varId, st1) <- emitInputVar name layout (InputLocation loc) st
              pure ((name, info):envAcc, initAcc, varId:idAcc, loc:usedLocs, usedBuiltins, st1)

    emitStructFields stg paramName env fields usedLocs usedBuiltins idAcc st = do
      let go accVars accIds accLocs accBuiltins st' [] = Right (reverse accVars, accLocs, accBuiltins, accIds, st')
          go accVars accIds accLocs accBuiltins st' (field:rest) = do
            let fieldName = fdName field
                fullName = paramName <> "_" <> fieldName
                attrs = fdAttrs field
                fty = fdType field
            layout <- resolveTypeLayout env [] fty
            when (containsResource layout) $
              Left (CompileError "resource types are not allowed as stage inputs" Nothing Nothing)
            when (containsAtomic layout) $
              Left (CompileError "atomic types are not allowed as stage inputs" Nothing Nothing)
            when (attrBuiltin attrs /= Nothing && attrLocation attrs /= Nothing) $
              Left (CompileError "struct fields cannot have both @location and @builtin" Nothing Nothing)
            case attrBuiltin attrs of
              Just builtin -> do
                when (builtin `elem` accBuiltins) $
                  Left (CompileError ("duplicate @builtin(" <> textToString builtin <> ") on stage inputs") Nothing Nothing)
                expected <- case builtinInputType stg builtin of
                  Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage input") Nothing Nothing)
                  Just ty' -> Right ty'
                if expected /= fty
                  then Left (CompileError ("@builtin(" <> textToString builtin <> ") has wrong type") Nothing Nothing)
                  else pure ()
                builtinId <- case builtinInputDecoration stg builtin of
                  Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage input") Nothing Nothing)
                  Just bid -> Right bid
                (info, varId, st1) <- emitInputVar fullName layout (InputBuiltin builtinId) st'
                let fieldInit = EntryFieldInit layout info
                go (fieldInit:accVars) (varId:accIds) accLocs (builtin:accBuiltins) st1 rest
              Nothing -> do
                loc <- case attrLocation attrs of
                  Nothing ->
                    case stg of
                      StageCompute -> Left (CompileError "compute parameters must use @builtin" Nothing Nothing)
                      _ -> Left (CompileError "struct fields must use @location or @builtin" Nothing Nothing)
                  Just n -> Right n
                when (stg == StageCompute) $
                  Left (CompileError "compute parameters cannot use @location" Nothing Nothing)
                when (loc `elem` accLocs) $
                  Left (CompileError "duplicate @location on stage inputs" Nothing Nothing)
                (info, varId, st1) <- emitInputVar fullName layout (InputLocation loc) st'
                let fieldInit = EntryFieldInit layout info
                go (fieldInit:accVars) (varId:accIds) (loc:accLocs) accBuiltins st1 rest
      (vars, locs, builtins, ids, st1) <- go [] idAcc usedLocs usedBuiltins st fields
      pure (vars, locs, builtins, ids, st1)

emitInputVar :: Text -> TypeLayout -> InputDecoration -> GenState -> Either CompileError (VarInfo, Word32, GenState)
emitInputVar name layout deco st0 = do
  let (baseTy, st1) = emitTypeFromLayout st0 layout
  let (ptrTy, st2) = emitPointerType st1 storageClassInput baseTy
  let (varId, st3) = freshId st2
  let st4 = addGlobal (Instr opVariable [ptrTy, varId, storageClassInput]) st3
  let st5 = case deco of
        InputBuiltin bid -> addDecoration (Instr opDecorate [varId, decorationBuiltIn, bid]) st4
        InputLocation loc -> addDecoration (Instr opDecorate [varId, decorationLocation, loc]) st4
  let st6 = addName (Instr opName (varId : encodeString (textToString name))) st5
  let info = VarInfo layout varId storageClassInput ReadOnly
  pure (info, varId, st6)

emitStageOutput :: [(Text, StructDecl)] -> EntryPoint -> Maybe TypeLayout -> GenState -> Either CompileError ([OutputTarget], [Word32], GenState)
emitStageOutput structEnv entry retLayout st0 =
  case epStage entry of
    StageCompute ->
      case retLayout of
        Nothing -> Right ([], [], st0)
        Just _ -> Left (CompileError "compute entry points must return void" Nothing Nothing)
    StageFragment ->
      emitFragmentOutput structEnv entry retLayout st0
    StageVertex ->
      emitVertexOutput structEnv entry retLayout st0

emitFragmentOutput :: [(Text, StructDecl)] -> EntryPoint -> Maybe TypeLayout -> GenState -> Either CompileError ([OutputTarget], [Word32], GenState)
emitFragmentOutput structEnv entry retLayout st0 =
  case (retLayout, epReturnType entry) of
    (Nothing, _) -> Left (CompileError "fragment entry point missing return type" Nothing Nothing)
    (Just layout, Just ty) ->
      case ty of
        TyStructRef structName -> do
          when (epReturnBuiltin entry /= Nothing || epReturnLocation entry /= Nothing) $
            Left (CompileError "struct return values cannot use @location or @builtin on the function" Nothing Nothing)
          structDecl <- case lookup structName structEnv of
            Nothing -> Left (CompileError ("unknown struct: " <> textToString structName) Nothing Nothing)
            Just decl -> Right decl
          emitStructOutputs StageFragment structName layout structDecl st0
        _ -> do
          when (containsResource layout) $
            Left (CompileError "resource types are not allowed as fragment outputs" Nothing Nothing)
          when (containsAtomic layout) $
            Left (CompileError "atomic types are not allowed as fragment outputs" Nothing Nothing)
          when (epReturnBuiltin entry /= Nothing && epReturnLocation entry /= Nothing) $
            Left (CompileError "fragment returns cannot use both @location and @builtin" Nothing Nothing)
          case epReturnBuiltin entry of
            Just builtin -> do
              expected <- case builtinOutputType StageFragment builtin of
                Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for fragment output") Nothing Nothing)
                Just t -> Right t
              when (expected /= ty) $
                Left (CompileError ("@builtin(" <> textToString builtin <> ") has wrong type") Nothing Nothing)
              builtinId <- case builtinOutputDecoration StageFragment builtin of
                Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for fragment output") Nothing Nothing)
                Just bid -> Right bid
              (info, varId, st1) <- emitOutputVar "frag_output" layout (InputBuiltin builtinId) st0
              let target = OutputTarget info layout []
              pure ([target], [varId], st1)
            Nothing -> do
              let loc = fromMaybe 0 (epReturnLocation entry)
              (info, varId, st1) <- emitOutputVar "frag_output" layout (InputLocation loc) st0
              let target = OutputTarget info layout []
              pure ([target], [varId], st1)
    _ -> Left (CompileError "fragment entry point missing return type" Nothing Nothing)

emitVertexOutput :: [(Text, StructDecl)] -> EntryPoint -> Maybe TypeLayout -> GenState -> Either CompileError ([OutputTarget], [Word32], GenState)
emitVertexOutput structEnv entry retLayout st0 =
  case (retLayout, epReturnType entry) of
    (Nothing, _) -> Left (CompileError "vertex entry point missing return type" Nothing Nothing)
    (Just layout, Just ty) ->
      case ty of
        TyStructRef structName -> do
          when (epReturnBuiltin entry /= Nothing || epReturnLocation entry /= Nothing) $
            Left (CompileError "struct return values cannot use @location or @builtin on the function" Nothing Nothing)
          structDecl <- case lookup structName structEnv of
            Nothing -> Left (CompileError ("unknown struct: " <> textToString structName) Nothing Nothing)
            Just decl -> Right decl
          emitStructOutputs StageVertex structName layout structDecl st0
        _ -> do
          when (epReturnLocation entry /= Nothing) $
            Left (CompileError "vertex entry points do not support @location returns" Nothing Nothing)
          when (epReturnBuiltin entry == Nothing) $
            Left (CompileError "vertex entry point must return @builtin(position)" Nothing Nothing)
          case epReturnBuiltin entry of
            Just "position" -> do
              when (TyVector 4 F32 /= ty) $
                Left (CompileError "@builtin(position) must be vec4<f32>" Nothing Nothing)
              (info, varId, st1) <- emitOutputVar "position" layout (InputBuiltin builtInPosition) st0
              let target = OutputTarget info layout []
              pure ([target], [varId], st1)
            _ -> Left (CompileError "vertex entry point must return @builtin(position)" Nothing Nothing)
    _ -> Left (CompileError "vertex entry point missing return type" Nothing Nothing)

emitStructOutputs :: Stage -> Text -> TypeLayout -> StructDecl -> GenState -> Either CompileError ([OutputTarget], [Word32], GenState)
emitStructOutputs stage structName layout structDecl st0 =
  case layout of
    TLStruct _ fieldLayouts _ _ -> do
      let fields = sdFields structDecl
      when (length fields /= length fieldLayouts) $
        Left (CompileError ("struct layout mismatch for " <> textToString structName) Nothing Nothing)
      let go _idx accTargets accIds usedLocs usedBuiltins st [] = Right (reverse accTargets, reverse accIds, st, usedLocs, usedBuiltins)
          go idx accTargets accIds usedLocs usedBuiltins st (field:rest) = do
            let fieldName = fdName field
                attrs = fdAttrs field
                fty = fdType field
                layoutField = fieldLayouts !! idx
                fieldLayout = flType layoutField
            when (containsResource fieldLayout) $
              Left (CompileError "resource types are not allowed as stage outputs" Nothing Nothing)
            when (containsAtomic fieldLayout) $
              Left (CompileError "atomic types are not allowed as stage outputs" Nothing Nothing)
            when (attrBuiltin attrs /= Nothing && attrLocation attrs /= Nothing) $
              Left (CompileError "struct fields cannot have both @location and @builtin" Nothing Nothing)
            case attrBuiltin attrs of
              Just builtin -> do
                when (builtin `elem` usedBuiltins) $
                  Left (CompileError ("duplicate @builtin(" <> textToString builtin <> ") on stage outputs") Nothing Nothing)
                expected <- case builtinOutputType stage builtin of
                  Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage output") Nothing Nothing)
                  Just t -> Right t
                when (expected /= fty) $
                  Left (CompileError ("@builtin(" <> textToString builtin <> ") has wrong type") Nothing Nothing)
                builtinId <- case builtinOutputDecoration stage builtin of
                  Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage output") Nothing Nothing)
                  Just bid -> Right bid
                (info, varId, st1) <- emitOutputVar (structName <> "_" <> fieldName) fieldLayout (InputBuiltin builtinId) st
                let target = OutputTarget info fieldLayout [fromIntegral idx]
                go (idx + 1) (target:accTargets) (varId:accIds) usedLocs (builtin:usedBuiltins) st1 rest
              Nothing -> do
                loc <- case attrLocation attrs of
                  Nothing -> Left (CompileError "struct output fields must use @location or @builtin" Nothing Nothing)
                  Just n -> Right n
                when (stage == StageCompute) $
                  Left (CompileError "compute outputs cannot use @location" Nothing Nothing)
                when (loc `elem` usedLocs) $
                  Left (CompileError "duplicate @location on stage outputs" Nothing Nothing)
                (info, varId, st1) <- emitOutputVar (structName <> "_" <> fieldName) fieldLayout (InputLocation loc) st
                let target = OutputTarget info fieldLayout [fromIntegral idx]
                go (idx + 1) (target:accTargets) (varId:accIds) (loc:usedLocs) usedBuiltins st1 rest
      (targets, ids, st1, _usedLocs, _usedBuiltins) <- go 0 [] [] [] [] st0 fields
      case stage of
        StageVertex ->
          if any (\f -> attrBuiltin (fdAttrs f) == Just "position") fields
            then pure ()
            else Left (CompileError "vertex output struct must include @builtin(position)" Nothing Nothing)
        _ -> pure ()
      pure (targets, ids, st1)
    _ -> Left (CompileError "expected struct return type" Nothing Nothing)

emitOutputVar :: Text -> TypeLayout -> InputDecoration -> GenState -> Either CompileError (VarInfo, Word32, GenState)
emitOutputVar name layout deco st0 = do
  let (baseTy, st1) = emitTypeFromLayout st0 layout
  let (ptrTy, st2) = emitPointerType st1 storageClassOutput baseTy
  let (varId, st3) = freshId st2
  let st4 = addGlobal (Instr opVariable [ptrTy, varId, storageClassOutput]) st3
  let st5 = case deco of
        InputBuiltin bid -> addDecoration (Instr opDecorate [varId, decorationBuiltIn, bid]) st4
        InputLocation loc -> addDecoration (Instr opDecorate [varId, decorationLocation, loc]) st4
  let st6 = addName (Instr opName (varId : encodeString (textToString name))) st5
  let info = VarInfo layout varId storageClassOutput ReadWrite
  pure (info, varId, st6)

emitMainFunction :: EntryPoint -> [(Text, VarInfo)] -> [EntryParamInit] -> [OutputTarget] -> GenState -> Either CompileError GenState
emitMainFunction entry env entryInits outTargets st0 = do
  let (voidTy, st1) = emitVoidType st0
  let (fnTy, st2) = emitFunctionType st1 voidTy []
  let (fnId, st3) = freshId st2
  let (labelId, st4) = freshId st3
  let st5 = addName (Instr opName (fnId : encodeString (textToString (epName entry)))) st4
  let fs0 = FuncState [] [] env [] False [] []
  (st6, fs1) <- emitEntryParamInits entryInits st5 fs0
  (st7, fs2) <- emitStatements entry outTargets st6 fs1
  let funcInstrs =
        [ Instr opFunction [voidTy, fnId, functionControlNone, fnTy]
        , Instr opLabel [labelId]
        ] <> fsLocals fs2 <> fsInstrs fs2 <> [Instr opReturn [], Instr opFunctionEnd []]
  let st8 = addFunctions funcInstrs st7
  let st9 = st8 { gsEntryPoint = Just fnId }
  pure st9

emitEntryParamInits :: [EntryParamInit] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitEntryParamInits inits st fs = foldM emitOne (st, fs) inits
  where
    emitOne (st', fs') initParam = do
      (st1, fs1, fieldVals) <- emitEntryFieldValues st' fs' (epiFields initParam)
      let (tyId, st2) = emitTypeFromLayout st1 (epiLayout initParam)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map valId fieldVals)) fs1
      let (ptrTy, st4) = emitPointerType st3 storageClassFunction tyId
      let (varId, st5) = freshId st4
      let fs3 = addFuncLocal (Instr opVariable [ptrTy, varId, storageClassFunction]) fs2
      let fs4 = addFuncInstr (Instr opStore [varId, resId]) fs3
      let info = VarInfo (epiLayout initParam) varId storageClassFunction ReadOnly
      let fs5 = fs4 { fsVars = (epiName initParam, info) : fsVars fs4 }
      pure (st5, fs5)

emitEntryFieldValues :: GenState -> FuncState -> [EntryFieldInit] -> Either CompileError (GenState, FuncState, [Value])
emitEntryFieldValues st fs fields = go st fs fields []
  where
    go st' fs' [] acc = Right (st', fs', reverse acc)
    go st' fs' (EntryFieldInit _ info:rest) acc = do
      (st1, fs1, val) <- emitLoadVar st' fs' info
      go st1 fs1 rest (val:acc)

emitLoadVar :: GenState -> FuncState -> VarInfo -> Either CompileError (GenState, FuncState, Value)
emitLoadVar st fs info = do
  let (tyId, st1) = emitTypeFromLayout st (viType info)
  let (resId, st2) = freshId st1
  let fs1 = addFuncInstr (Instr opLoad [tyId, resId, viPtrId info]) fs
  pure (st2, fs1, Value (viType info) resId)

registerFunctions :: [(Text, StructDecl)] -> [FunctionDecl] -> GenState -> Either CompileError GenState
registerFunctions structEnv decls st0 = foldM registerOne st0 decls
  where
    registerOne st decl =
      do
        paramLayouts <- mapM (resolveTypeLayout structEnv []) (map paramType (fnParams decl))
        mapM_ (ensureNoResources "function parameter") paramLayouts
        retLayout <- case fnReturnType decl of
          Nothing -> Right Nothing
          Just ty -> do
            layout <- resolveTypeLayout structEnv [] ty
            ensureNoResources "function return type" layout
            Right (Just layout)
        let existing = [fi | fi <- gsFunctionTable st, fiName fi == fnName decl]
        when (any (\fi -> fiParams fi == paramLayouts) existing) $
          Left (CompileError ("duplicate function overload: " <> textToString (fnName decl)) Nothing Nothing)
        let (retTyId, st1) = case retLayout of
              Nothing -> emitVoidType st
              Just layout -> emitTypeFromLayout st layout
        let (st2, paramTypeIds) =
              mapAccumL (\acc layout -> let (tid, acc') = emitTypeFromLayout acc layout in (acc', tid)) st1 paramLayouts
        let (fnTypeId, st3) = emitFunctionType st2 retTyId paramTypeIds
        let (fnId, st4) = freshId st3
        let info = FunctionInfo (fnName decl) paramLayouts retLayout fnId fnTypeId
        let st5 = st4 { gsFunctionTable = info : gsFunctionTable st4 }
        pure st5

emitFunctionBodies :: [(Text, StructDecl)] -> [FunctionDecl] -> GenState -> Either CompileError GenState
emitFunctionBodies structEnv decls st0 = foldM emitOne st0 decls
  where
    emitOne st decl = do
      paramLayouts <- mapM (resolveTypeLayout structEnv []) (map paramType (fnParams decl))
      case findFunctionInfo (fnName decl) paramLayouts (gsFunctionTable st) of
        Nothing -> Left (CompileError ("missing function info for " <> textToString (fnName decl)) Nothing Nothing)
        Just info -> emitFunctionBody info decl st

findFunctionInfo :: Text -> [TypeLayout] -> [FunctionInfo] -> Maybe FunctionInfo
findFunctionInfo name paramLayouts infos =
  case filter (\fi -> fiName fi == name && fiParams fi == paramLayouts) infos of
    (x:_) -> Just x
    [] -> Nothing

emitFunctionBody :: FunctionInfo -> FunctionDecl -> GenState -> Either CompileError GenState
emitFunctionBody info decl st0 = do
  let (retTyId, st1) = case fiReturn info of
        Nothing -> emitVoidType st0
        Just layout -> emitTypeFromLayout st0 layout
  let (fnLabel, st2) = freshId st1
  let st3 = addName (Instr opName (fiId info : encodeString (textToString (fnName decl)))) st2
  let (paramInstrs, paramLocals, paramStores, env, st4) = emitFunctionParams (fnParams decl) (fiParams info) st3
  let envWithGlobals = gsGlobalVars st4 <> env
  let fs0 = FuncState paramLocals paramStores envWithGlobals [] False [] []
  (st5, fs1) <- emitStmtListFn (fiReturn info) st4 fs0 (fnBody decl)
  fs2 <- finalizeFunctionReturn (fiReturn info) fs1
  let funcInstrs =
        [ Instr opFunction [retTyId, fiId info, functionControlNone, fiTypeId info]
        ] <> paramInstrs <>
        [ Instr opLabel [fnLabel]
        ] <> fsLocals fs2 <> fsInstrs fs2 <> [Instr opFunctionEnd []]
  let st6 = addFunctions funcInstrs st5
  pure st6

emitFunctionParams :: [Param] -> [TypeLayout] -> GenState -> ([Instr], [Instr], [Instr], [(Text, VarInfo)], GenState)
emitFunctionParams params layouts st0 =
  let go st accInstrs accLocals accStores accEnv [] [] = (reverse accInstrs, reverse accLocals, reverse accStores, reverse accEnv, st)
      go st accInstrs accLocals accStores accEnv (p:ps) (l:ls) =
        let (paramTyId, st1) = emitTypeFromLayout st l
            (paramId, st2) = freshId st1
            paramInstr = Instr opFunctionParameter [paramTyId, paramId]
            (ptrTy, st3) = emitPointerType st2 storageClassFunction paramTyId
            (varId, st4) = freshId st3
            localInstr = Instr opVariable [ptrTy, varId, storageClassFunction]
            storeInstr = Instr opStore [varId, paramId]
            info = VarInfo l varId storageClassFunction ReadOnly
        in go st4 (paramInstr:accInstrs) (localInstr:accLocals) (storeInstr:accStores) ((paramName p, info):accEnv) ps ls
      go st accInstrs accLocals accStores accEnv _ _ = (reverse accInstrs, reverse accLocals, reverse accStores, reverse accEnv, st)
  in go st0 [] [] [] [] params layouts

emitStmtListFn :: Maybe TypeLayout -> GenState -> FuncState -> [Stmt] -> Either CompileError (GenState, FuncState)
emitStmtListFn retLayout st fs = go st fs
  where
    go st' fs' [] = Right (st', fs')
    go st' fs' (s:ss) = do
      (st1, fs1) <- emitStmtFn retLayout st' fs' s
      go st1 fs1 ss

emitStmtFn :: Maybe TypeLayout -> GenState -> FuncState -> Stmt -> Either CompileError (GenState, FuncState)
emitStmtFn retLayout st fs stmt
  | fsTerminated fs = Right (st, fs)
  | otherwise =
      case stmt of
        SLet name expr -> emitLet name expr st fs
        SVar name expr -> emitVar name expr st fs
        SAssign lv expr -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          case viType ptrInfo of
            TLAtomic _ -> Left (CompileError "use atomicStore for atomic values" Nothing Nothing)
            _ -> pure ()
          (st2, fs2, val) <- emitExpr st1 fs1 expr
          (st3, fs3, val') <- coerceValueToLayout (viType ptrInfo) val st2 fs2
          ensureWritable ptrInfo
          let fs4 = addFuncInstr (Instr opStore [viPtrId ptrInfo, valId val']) fs3
          Right (st3, fs4)
        SAssignOp lv op expr -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          case viType ptrInfo of
            TLAtomic _ -> Left (CompileError "use atomicStore for atomic values" Nothing Nothing)
            _ -> pure ()
          ensureWritable ptrInfo
          (st2, fs2, lhsVal) <- emitLoadFromPtr st1 fs1 ptrInfo
          (st3, fs3, rhsVal) <- emitExpr st2 fs2 expr
          (st4, fs4, rhsVal') <- coerceValueToLayout (viType ptrInfo) rhsVal st3 fs3
          (st5, fs5, resVal) <- emitBinary op (viType ptrInfo) (valId lhsVal) (valId rhsVal') st4 fs4
          let fs6 = addFuncInstr (Instr opStore [viPtrId ptrInfo, valId resVal]) fs5
          Right (st5, fs6)
        SInc lv -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          case viType ptrInfo of
            TLAtomic _ -> Left (CompileError "use atomicStore for atomic values" Nothing Nothing)
            _ -> pure ()
          ensureWritable ptrInfo
          (st2, fs2, lhsVal) <- emitLoadFromPtr st1 fs1 ptrInfo
          (oneId, st3) <- emitConstOne (viType ptrInfo) st2
          (st4, fs3, resVal) <- emitBinary OpAdd (viType ptrInfo) (valId lhsVal) oneId st3 fs2
          let fs4 = addFuncInstr (Instr opStore [viPtrId ptrInfo, valId resVal]) fs3
          Right (st4, fs4)
        SDec lv -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          case viType ptrInfo of
            TLAtomic _ -> Left (CompileError "use atomicStore for atomic values" Nothing Nothing)
            _ -> pure ()
          ensureWritable ptrInfo
          (st2, fs2, lhsVal) <- emitLoadFromPtr st1 fs1 ptrInfo
          (oneId, st3) <- emitConstOne (viType ptrInfo) st2
          (st4, fs3, resVal) <- emitBinary OpSub (viType ptrInfo) (valId lhsVal) oneId st3 fs2
          let fs4 = addFuncInstr (Instr opStore [viPtrId ptrInfo, valId resVal]) fs3
          Right (st4, fs4)
        SExpr expr -> emitExprStmt st fs expr
        SIf cond thenBody elseBody ->
          emitIfFn retLayout cond thenBody elseBody st fs
        SWhile cond body ->
          emitWhileFn retLayout cond body st fs
        SLoop body continuing ->
          emitLoopFn retLayout body continuing st fs
        SFor initStmt condExpr contStmt body ->
          emitForFn retLayout initStmt condExpr contStmt body st fs
        SSwitch expr cases defBody ->
          emitSwitchFn retLayout expr cases defBody st fs
        SBreak ->
          emitBreak st fs
        SBreakIf cond ->
          emitIfFn retLayout cond [SBreak] Nothing st fs
        SContinue ->
          emitContinue st fs
        SDiscard ->
          if gsEntryStage st == StageFragment
            then
              let fs1 = addTerminator (Instr opKill []) fs
              in Right (st, fs1)
            else Left (CompileError "discard is only allowed in fragment entry points" Nothing Nothing)
        SFallthrough ->
          Left (CompileError "fallthrough is only allowed in switch cases" Nothing Nothing)
        SReturn mexpr ->
          case retLayout of
            Nothing ->
              case mexpr of
                Nothing -> Right (st, fs { fsTerminated = True, fsInstrs = fsInstrs fs <> [Instr opReturn []] })
                Just _ -> Left (CompileError "void function cannot return a value" Nothing Nothing)
            Just layout -> do
              expr <- case mexpr of
                Nothing -> Left (CompileError "non-void function must return a value" Nothing Nothing)
                Just e -> Right e
              (st1, fs1, val) <- emitExpr st fs expr
              (st2, fs2, val') <- coerceValueToLayout layout val st1 fs1
              let fs3 = addFuncInstr (Instr opReturnValue [valId val']) fs2
              Right (st2, fs3 { fsTerminated = True })

finalizeFunctionReturn :: Maybe TypeLayout -> FuncState -> Either CompileError FuncState
finalizeFunctionReturn retLayout fs =
  if fsTerminated fs
    then Right fs
    else case retLayout of
      Nothing -> Right (fs { fsInstrs = fsInstrs fs <> [Instr opReturn []], fsTerminated = True })
      Just _ -> Left (CompileError "non-void function must return a value" Nothing Nothing)

emitIfFn :: Maybe TypeLayout -> Expr -> [Stmt] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitIfFn retLayout cond thenBody elseBody st fs = do
  (st1, fs1, condVal) <- emitExpr st fs cond
  ensureBoolScalar (valType condVal)
  let (thenLabel, st2) = freshId st1
  let (elseLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3
  let fs2 = addFuncInstr (Instr opSelectionMerge [mergeLabel, selectionControlNone]) fs1
  let fs3 = addTerminator (Instr opBranchConditional [valId condVal, thenLabel, elseLabel]) fs2

  let fsThen0 = addLabel thenLabel fs3
  (st5, fsThen1) <- emitStmtListFn retLayout st4 fsThen0 thenBody
  let thenTerm = fsTerminated fsThen1
  let fsThen2 = if thenTerm then fsThen1 else addTerminator (Instr opBranch [mergeLabel]) fsThen1

  let fsElse0 = addLabel elseLabel fsThen2
  (st6, fsElse1) <- case elseBody of
    Nothing -> Right (st5, fsElse0)
    Just body -> emitStmtListFn retLayout st5 fsElse0 body
  let elseTerm = fsTerminated fsElse1
  let fsElse2 = if elseTerm then fsElse1 else addTerminator (Instr opBranch [mergeLabel]) fsElse1

  let fsMerge = addLabel mergeLabel fsElse2
  let fsMerge1 =
        if thenTerm && elseTerm
          then addTerminator (Instr opUnreachable []) fsMerge
          else fsMerge
  Right (st6, fsMerge1)

emitWhileFn :: Maybe TypeLayout -> Expr -> [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitWhileFn retLayout cond body st fs = do
  let loopStack = fsLoopStack fs
  let breakStack = fsBreakStack fs
  let (headerLabel, st1) = freshId st
  let (bodyLabel, st2) = freshId st1
  let (continueLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3

  let fs1 = addTerminator (Instr opBranch [headerLabel]) fs
  let fsHeader0 = addLabel headerLabel fs1
  (st5, fsHeader1, condVal) <- emitExpr st4 fsHeader0 cond
  ensureBoolScalar (valType condVal)
  let fsHeader2 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader1
  let fsHeader3 = addTerminator (Instr opBranchConditional [valId condVal, bodyLabel, mergeLabel]) fsHeader2

  let fsBody0 = addLabel bodyLabel fsHeader3
  let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st6, fsBody2) <- emitStmtListFn retLayout st5 fsBody1 body
  let fsBody3 = if fsTerminated fsBody2 then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

  let fsContinue0 = addLabel continueLabel fsBody3
  let fsContinue1 = addTerminator (Instr opBranch [headerLabel]) (fsContinue0 { fsBreakStack = mergeLabel : breakStack })

  let fsMerge = addLabel mergeLabel fsContinue1
  let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
  Right (st6, fsMerge1)

emitLoopFn :: Maybe TypeLayout -> [Stmt] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitLoopFn retLayout body continuing st fs = do
  let loopStack = fsLoopStack fs
  let breakStack = fsBreakStack fs
  let (headerLabel, st1) = freshId st
  let (bodyLabel, st2) = freshId st1
  let (continueLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3

  let fs1 = addTerminator (Instr opBranch [headerLabel]) fs
  let fsHeader0 = addLabel headerLabel fs1
  let fsHeader1 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader0
  let fsHeader2 = addTerminator (Instr opBranch [bodyLabel]) fsHeader1

  let fsBody0 = addLabel bodyLabel fsHeader2
  let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st5, fsBody2) <- emitStmtListFn retLayout st4 fsBody1 body
  let fsBody3 = if fsTerminated fsBody2 then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

  let fsContinue0 = addLabel continueLabel fsBody3
  let fsContinue1 = fsContinue0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st6, fsContinue2) <- case continuing of
    Nothing -> Right (st5, fsContinue1)
    Just contBody -> emitStmtListFn retLayout st5 fsContinue1 contBody
  let fsContinue3 =
        if fsTerminated fsContinue2
          then fsContinue2
          else addTerminator (Instr opBranch [headerLabel]) fsContinue2

  let fsMerge = addLabel mergeLabel fsContinue3
  let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
  Right (st6, fsMerge1)

emitForFn :: Maybe TypeLayout -> Maybe Stmt -> Maybe Expr -> Maybe Stmt -> [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitForFn retLayout initStmt condExpr contStmt body st fs = do
  (st1, fs1) <- case initStmt of
    Nothing -> Right (st, fs)
    Just s -> emitStmtFn retLayout st fs s
  if fsTerminated fs1
    then Right (st1, fs1)
    else do
      let loopStack = fsLoopStack fs1
      let breakStack = fsBreakStack fs1
      let (headerLabel, st2) = freshId st1
      let (bodyLabel, st3) = freshId st2
      let (continueLabel, st4) = freshId st3
      let (mergeLabel, st5) = freshId st4

      let fs2 = addTerminator (Instr opBranch [headerLabel]) fs1
      let fsHeader0 = addLabel headerLabel fs2
      (st6, fsHeader1, condVal) <- case condExpr of
        Nothing -> do
          let (cid, st') = emitConstBool st5 True
          let (a, sz) = scalarLayout Bool
          let layout = TLScalar Bool a sz
          Right (st', fsHeader0, Value layout cid)
        Just expr -> emitExpr st5 fsHeader0 expr
      ensureBoolScalar (valType condVal)
      let fsHeader2 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader1
      let fsHeader3 = addTerminator (Instr opBranchConditional [valId condVal, bodyLabel, mergeLabel]) fsHeader2

      let fsBody0 = addLabel bodyLabel fsHeader3
      let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
      (st7, fsBody2) <- emitStmtListFn retLayout st6 fsBody1 body
      let fsBody3 = if fsTerminated fsBody2 then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

      let fsContinue0 = addLabel continueLabel fsBody3
      let fsContinue1 = fsContinue0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
      (st8, fsContinue2) <- case contStmt of
        Nothing -> Right (st7, fsContinue1)
        Just s -> emitStmtFn retLayout st7 fsContinue1 s
      let fsContinue3 =
            if fsTerminated fsContinue2
              then fsContinue2
              else addTerminator (Instr opBranch [headerLabel]) fsContinue2

      let fsMerge = addLabel mergeLabel fsContinue3
      let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
      Right (st8, fsMerge1)

emitSwitchFn :: Maybe TypeLayout -> Expr -> [SwitchCase] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitSwitchFn retLayout expr cases defBody st fs = do
  (st1, fs1, selVal) <- emitExpr st fs expr
  ensureSwitchType (valType selVal)
  cases' <- expandSwitchCases cases defBody
  let breakStack = fsBreakStack fs1
  let (mergeLabel, st2) = freshId st1
  let fs2 = fs1 { fsBreakStack = mergeLabel : breakStack }
  (st3, fs3) <- emitSwitchChainFn retLayout selVal cases' defBody st2 fs2
  let fs4 = if fsTerminated fs3 then fs3 else addTerminator (Instr opBranch [mergeLabel]) fs3
  let fs5 = addLabel mergeLabel fs4
  let fs6 = fs5 { fsBreakStack = breakStack }
  Right (st3, fs6)

emitSwitchChainFn :: Maybe TypeLayout -> Value -> [SwitchCase] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitSwitchChainFn retLayout selVal cases defBody st fs =
  case cases of
    [] ->
      case defBody of
        Nothing -> Right (st, fs)
        Just body -> emitStmtListFn retLayout st fs body
    (SwitchCase selectors body : rest) -> do
      (st1, fs1, condVal) <- emitSwitchCond selVal selectors st fs
      ensureBoolScalar (valType condVal)
      let (thenLabel, st2) = freshId st1
      let (elseLabel, st3) = freshId st2
      let (mergeLabel, st4) = freshId st3
      let fs2 = addFuncInstr (Instr opSelectionMerge [mergeLabel, selectionControlNone]) fs1
      let fs3 = addTerminator (Instr opBranchConditional [valId condVal, thenLabel, elseLabel]) fs2

      let fsThen0 = addLabel thenLabel fs3
      (st5, fsThen1) <- emitStmtListFn retLayout st4 fsThen0 body
      let fsThen2 = if fsTerminated fsThen1 then fsThen1 else addTerminator (Instr opBranch [mergeLabel]) fsThen1

      let fsElse0 = addLabel elseLabel fsThen2
      (st6, fsElse1) <- emitSwitchChainFn retLayout selVal rest defBody st5 fsElse0
      let fsElse2 = if fsTerminated fsElse1 then fsElse1 else addTerminator (Instr opBranch [mergeLabel]) fsElse1

      let fsMerge = addLabel mergeLabel fsElse2
      let fsMerge1 =
            if fsTerminated fsThen1 && fsTerminated fsElse1
              then addTerminator (Instr opUnreachable []) fsMerge
              else fsMerge
      Right (st6, fsMerge1)

emitSwitchCond :: Value -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSwitchCond selVal selectors st fs =
  case selectors of
    [] -> Left (CompileError "switch case must have at least one selector" Nothing Nothing)
    firstSel : rest -> do
      (st1, fs1, firstCond) <- emitSelector selVal firstSel st fs
      foldM combine (st1, fs1, firstCond) rest
  where
    emitSelector base expr st' fs' = do
      (st1, sel) <- emitConstExpr st' expr
      (st2, sel') <- coerceConstValueToLayout (valType base) sel st1
      emitBinary OpEq (valType base) (valId base) (valId sel') st2 fs'
    combine (st1, fs1, acc) expr = do
      (st2, fs2, nextCond) <- emitSelector selVal expr st1 fs1
      emitBinary OpOr (valType acc) (valId acc) (valId nextCond) st2 fs2

ensureNoResources :: String -> TypeLayout -> Either CompileError ()
ensureNoResources label layout =
  if containsResource layout || containsAtomic layout
    then Left (CompileError (label <> " cannot contain resource or atomic types") Nothing Nothing)
    else Right ()

addFunctions :: [Instr] -> GenState -> GenState
addFunctions instrs st = st { gsFunctions = gsFunctions st <> instrs }

getExtInstSet :: GenState -> String -> (Word32, GenState)
getExtInstSet st name =
  case lookup name (gsExtInstIds st) of
    Just sid -> (sid, st)
    Nothing ->
      let (sid, st1) = freshId st
          instr = Instr opExtInstImport (sid : encodeString name)
          st2 = st1
            { gsExtInstIds = (name, sid) : gsExtInstIds st1
            , gsExtInstImports = gsExtInstImports st1 <> [instr]
            }
      in (sid, st2)

addFuncInstr :: Instr -> FuncState -> FuncState
addFuncInstr instr fs = fs { fsInstrs = fsInstrs fs <> [instr] }

addFuncLocal :: Instr -> FuncState -> FuncState
addFuncLocal instr fs = fs { fsLocals = fsLocals fs <> [instr] }

addTerminator :: Instr -> FuncState -> FuncState
addTerminator instr fs = fs { fsInstrs = fsInstrs fs <> [instr], fsTerminated = True }

addLabel :: Word32 -> FuncState -> FuncState
addLabel lbl fs = fs { fsInstrs = fsInstrs fs <> [Instr opLabel [lbl]], fsTerminated = False }

emitStatements :: EntryPoint -> [OutputTarget] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitStatements entry outTargets st fs = do
  (st1, fs1) <- emitStmtList entry outTargets st fs (epBody entry)
  case epStage entry of
    StageFragment ->
      if fsTerminated fs1
        then Right (st1, fs1)
        else Left (CompileError "fragment entry point must return a value" Nothing Nothing)
    StageVertex ->
      if fsTerminated fs1
        then Right (st1, fs1)
        else Left (CompileError "vertex entry point must return a value" Nothing Nothing)
    StageCompute -> Right (st1, fs1)

storeReturnValue :: [OutputTarget] -> TypeLayout -> Word32 -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
storeReturnValue targets valLayout valId st fs = do
  when (null targets) $
    Left (CompileError "missing output targets for return value" Nothing Nothing)
  case targets of
    [OutputTarget out layout []] -> do
      ensureTypeMatch layout valLayout
      let fs1 = addFuncInstr (Instr opStore [viPtrId out, valId]) fs
      Right (st, fs1)
    _ -> foldM storeOne (st, fs) targets
  where
    storeOne (st', fs') target = do
      let outInfo = otVar target
      let outLayout = otLayout target
      case otPath target of
        [] -> do
          ensureTypeMatch outLayout valLayout
          let fs1 = addFuncInstr (Instr opStore [viPtrId outInfo, valId]) fs'
          Right (st', fs1)
        path -> do
          let (tyId, st1) = emitTypeFromLayout st' outLayout
          let (resId, st2) = freshId st1
          let fs1 = addFuncInstr (Instr opCompositeExtract (tyId : resId : valId : path)) fs'
          let fs2 = addFuncInstr (Instr opStore [viPtrId outInfo, resId]) fs1
          Right (st2, fs2)

emitStmtList :: EntryPoint -> [OutputTarget] -> GenState -> FuncState -> [Stmt] -> Either CompileError (GenState, FuncState)
emitStmtList entry outTargets st fs = go st fs
  where
    go st' fs' [] = Right (st', fs')
    go st' fs' (s:ss) = do
      (st1, fs1) <- emitStmt entry outTargets st' fs' s
      go st1 fs1 ss

emitStmt :: EntryPoint -> [OutputTarget] -> GenState -> FuncState -> Stmt -> Either CompileError (GenState, FuncState)
emitStmt entry outTargets st fs stmt
  | fsTerminated fs = Right (st, fs)
  | otherwise =
      case stmt of
        SLet name expr -> emitLet name expr st fs
        SVar name expr -> emitVar name expr st fs
        SAssign lv expr -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          case viType ptrInfo of
            TLAtomic _ -> Left (CompileError "use atomicStore for atomic values" Nothing Nothing)
            _ -> pure ()
          (st2, fs2, val) <- emitExpr st1 fs1 expr
          (st3, fs3, val') <- coerceValueToLayout (viType ptrInfo) val st2 fs2
          ensureWritable ptrInfo
          let fs4 = addFuncInstr (Instr opStore [viPtrId ptrInfo, valId val']) fs3
          Right (st3, fs4)
        SAssignOp lv op expr -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          case viType ptrInfo of
            TLAtomic _ -> Left (CompileError "use atomicStore for atomic values" Nothing Nothing)
            _ -> pure ()
          ensureWritable ptrInfo
          (st2, fs2, lhsVal) <- emitLoadFromPtr st1 fs1 ptrInfo
          (st3, fs3, rhsVal) <- emitExpr st2 fs2 expr
          (st4, fs4, rhsVal') <- coerceValueToLayout (viType ptrInfo) rhsVal st3 fs3
          (st5, fs5, resVal) <- emitBinary op (viType ptrInfo) (valId lhsVal) (valId rhsVal') st4 fs4
          let fs6 = addFuncInstr (Instr opStore [viPtrId ptrInfo, valId resVal]) fs5
          Right (st5, fs6)
        SInc lv -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          case viType ptrInfo of
            TLAtomic _ -> Left (CompileError "use atomicStore for atomic values" Nothing Nothing)
            _ -> pure ()
          ensureWritable ptrInfo
          (st2, fs2, lhsVal) <- emitLoadFromPtr st1 fs1 ptrInfo
          (oneId, st3) <- emitConstOne (viType ptrInfo) st2
          (st4, fs3, resVal) <- emitBinary OpAdd (viType ptrInfo) (valId lhsVal) oneId st3 fs2
          let fs4 = addFuncInstr (Instr opStore [viPtrId ptrInfo, valId resVal]) fs3
          Right (st4, fs4)
        SDec lv -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          case viType ptrInfo of
            TLAtomic _ -> Left (CompileError "use atomicStore for atomic values" Nothing Nothing)
            _ -> pure ()
          ensureWritable ptrInfo
          (st2, fs2, lhsVal) <- emitLoadFromPtr st1 fs1 ptrInfo
          (oneId, st3) <- emitConstOne (viType ptrInfo) st2
          (st4, fs3, resVal) <- emitBinary OpSub (viType ptrInfo) (valId lhsVal) oneId st3 fs2
          let fs4 = addFuncInstr (Instr opStore [viPtrId ptrInfo, valId resVal]) fs3
          Right (st4, fs4)
        SExpr expr -> emitExprStmt st fs expr
        SIf cond thenBody elseBody ->
          emitIf entry outTargets cond thenBody elseBody st fs
        SWhile cond body ->
          emitWhile entry outTargets cond body st fs
        SLoop body continuing ->
          emitLoop entry outTargets body continuing st fs
        SFor initStmt condExpr contStmt body ->
          emitFor entry outTargets initStmt condExpr contStmt body st fs
        SSwitch expr cases defBody ->
          emitSwitch entry outTargets expr cases defBody st fs
        SBreak ->
          emitBreak st fs
        SBreakIf cond ->
          emitIf entry outTargets cond [SBreak] Nothing st fs
        SContinue ->
          emitContinue st fs
        SDiscard ->
          case epStage entry of
            StageFragment ->
              let fs1 = addTerminator (Instr opKill []) fs
              in Right (st, fs1)
            _ -> Left (CompileError "discard is only allowed in fragment entry points" Nothing Nothing)
        SFallthrough ->
          Left (CompileError "fallthrough is only allowed in switch cases" Nothing Nothing)
        SReturn mexpr ->
          case epStage entry of
            StageCompute ->
              case mexpr of
                Nothing -> Right (st, fs { fsTerminated = True })
                Just _ -> Left (CompileError "compute entry points cannot return a value" Nothing Nothing)
            StageFragment -> do
              expr <- case mexpr of
                Nothing -> Left (CompileError "fragment entry points must return a value" Nothing Nothing)
                Just e -> Right e
              (st1, fs1, val) <- emitExpr st fs expr
              (st2, fs2) <- storeReturnValue outTargets (valType val) (valId val) st1 fs1
              Right (st2, fs2 { fsTerminated = True })
            StageVertex -> do
              expr <- case mexpr of
                Nothing -> Left (CompileError "vertex entry points must return a value" Nothing Nothing)
                Just e -> Right e
              (st1, fs1, val) <- emitExpr st fs expr
              (st2, fs2) <- storeReturnValue outTargets (valType val) (valId val) st1 fs1
              Right (st2, fs2 { fsTerminated = True })

emitExprStmt :: GenState -> FuncState -> Expr -> Either CompileError (GenState, FuncState)
emitExprStmt st fs expr =
  case expr of
    ECall name args ->
      case name of
        "textureStore" -> emitTextureStore args st fs
        "atomicStore" -> emitAtomicStore args st fs
        _ ->
          if any (\fi -> fiName fi == name) (gsFunctionTable st)
            then do
              (st1, fs1, _) <- emitFunctionCallByName name args st fs
              Right (st1, fs1)
            else do
              (st1, fs1, _) <- emitExpr st fs expr
              Right (st1, fs1)
    _ -> do
      (st1, fs1, _) <- emitExpr st fs expr
      Right (st1, fs1)

emitLoadFromPtr :: GenState -> FuncState -> VarInfo -> Either CompileError (GenState, FuncState, Value)
emitLoadFromPtr st fs info = do
  let (tyId, st1) = emitTypeFromLayout st (viType info)
  let (resId, st2) = freshId st1
  let fs1 = addFuncInstr (Instr opLoad [tyId, resId, viPtrId info]) fs
  Right (st2, fs1, Value (viType info) resId)

emitConstOne :: TypeLayout -> GenState -> Either CompileError (Word32, GenState)
emitConstOne layout st =
  case layout of
    TLScalar U32 _ _ -> Right (emitConstU32 st 1)
    TLScalar I32 _ _ -> Right (emitConstI32 st 1)
    TLScalar F32 _ _ -> Right (emitConstF32 st 1.0)
    TLScalar F16 _ _ -> Right (emitConstF16 st 1.0)
    _ -> Left (CompileError "increment/decrement requires an i32, u32, f16, or f32 scalar" Nothing Nothing)

emitLet :: Text -> Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitLet name expr st fs = do
  (st1, fs1, val) <- emitExpr st fs expr
  case valType val of
    TLPointer {} ->
      let fs2 = fs1 { fsValues = (name, val) : fsValues fs1 }
      in Right (st1, fs2)
    _ -> emitLocalValue name val st1 fs1

emitVar :: Text -> Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitVar name expr st fs = do
  (st1, fs1, val) <- emitExpr st fs expr
  case valType val of
    TLPointer {} -> Left (CompileError "var cannot have pointer type" Nothing Nothing)
    _ -> emitLocalValue name val st1 fs1

emitLocalValue :: Text -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitLocalValue name val st fs = do
  let (baseTy, st1) = emitTypeFromLayout st (valType val)
  let (ptrTy, st2) = emitPointerType st1 storageClassFunction baseTy
  let (varId, st3) = freshId st2
  let fs1 = addFuncLocal (Instr opVariable [ptrTy, varId, storageClassFunction]) fs
  let fs2 = addFuncInstr (Instr opStore [varId, valId val]) fs1
  let info = VarInfo (valType val) varId storageClassFunction ReadWrite
  let fs3 = fs2 { fsVars = (name, info) : fsVars fs2 }
  Right (st3, fs3)

emitIf :: EntryPoint -> [OutputTarget] -> Expr -> [Stmt] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitIf entry outTargets cond thenBody elseBody st fs = do
  (st1, fs1, condVal) <- emitExpr st fs cond
  ensureBoolScalar (valType condVal)
  let (thenLabel, st2) = freshId st1
  let (elseLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3
  let fs2 = addFuncInstr (Instr opSelectionMerge [mergeLabel, selectionControlNone]) fs1
  let fs3 = addTerminator (Instr opBranchConditional [valId condVal, thenLabel, elseLabel]) fs2

  let fsThen0 = addLabel thenLabel fs3
  (st5, fsThen1) <- emitStmtList entry outTargets st4 fsThen0 thenBody
  let thenTerm = fsTerminated fsThen1
  let fsThen2 = if thenTerm then fsThen1 else addTerminator (Instr opBranch [mergeLabel]) fsThen1

  let fsElse0 = addLabel elseLabel fsThen2
  (st6, fsElse1) <- case elseBody of
    Nothing -> Right (st5, fsElse0)
    Just body -> emitStmtList entry outTargets st5 fsElse0 body
  let elseTerm = fsTerminated fsElse1
  let fsElse2 = if elseTerm then fsElse1 else addTerminator (Instr opBranch [mergeLabel]) fsElse1

  let fsMerge = addLabel mergeLabel fsElse2
  let fsMerge1 =
        if thenTerm && elseTerm
          then addTerminator (Instr opUnreachable []) fsMerge
          else fsMerge
  Right (st6, fsMerge1)

emitWhile :: EntryPoint -> [OutputTarget] -> Expr -> [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitWhile entry outTargets cond body st fs = do
  let loopStack = fsLoopStack fs
  let breakStack = fsBreakStack fs
  let (headerLabel, st1) = freshId st
  let (bodyLabel, st2) = freshId st1
  let (continueLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3

  let fs1 = addTerminator (Instr opBranch [headerLabel]) fs
  let fsHeader0 = addLabel headerLabel fs1
  (st5, fsHeader1, condVal) <- emitExpr st4 fsHeader0 cond
  ensureBoolScalar (valType condVal)
  let fsHeader2 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader1
  let fsHeader3 = addTerminator (Instr opBranchConditional [valId condVal, bodyLabel, mergeLabel]) fsHeader2

  let fsBody0 = addLabel bodyLabel fsHeader3
  let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st6, fsBody2) <- emitStmtList entry outTargets st5 fsBody1 body
  let fsBody3 = if fsTerminated fsBody2 then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

  let fsContinue0 = addLabel continueLabel fsBody3
  let fsContinue1 = addTerminator (Instr opBranch [headerLabel]) (fsContinue0 { fsBreakStack = mergeLabel : breakStack })

  let fsMerge = addLabel mergeLabel fsContinue1
  let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
  Right (st6, fsMerge1)

emitLoop :: EntryPoint -> [OutputTarget] -> [Stmt] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitLoop entry outTargets body continuing st fs = do
  let loopStack = fsLoopStack fs
  let breakStack = fsBreakStack fs
  let (headerLabel, st1) = freshId st
  let (bodyLabel, st2) = freshId st1
  let (continueLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3

  let fs1 = addTerminator (Instr opBranch [headerLabel]) fs
  let fsHeader0 = addLabel headerLabel fs1
  let fsHeader1 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader0
  let fsHeader2 = addTerminator (Instr opBranch [bodyLabel]) fsHeader1

  let fsBody0 = addLabel bodyLabel fsHeader2
  let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st5, fsBody2) <- emitStmtList entry outTargets st4 fsBody1 body
  let fsBody3 = if fsTerminated fsBody2 then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

  let fsContinue0 = addLabel continueLabel fsBody3
  let fsContinue1 = fsContinue0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st6, fsContinue2) <- case continuing of
    Nothing -> Right (st5, fsContinue1)
    Just contBody -> emitStmtList entry outTargets st5 fsContinue1 contBody
  let fsContinue3 =
        if fsTerminated fsContinue2
          then fsContinue2
          else addTerminator (Instr opBranch [headerLabel]) fsContinue2

  let fsMerge = addLabel mergeLabel fsContinue3
  let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
  Right (st6, fsMerge1)

emitFor :: EntryPoint -> [OutputTarget] -> Maybe Stmt -> Maybe Expr -> Maybe Stmt -> [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitFor entry outTargets initStmt condExpr contStmt body st fs = do
  (st1, fs1) <- case initStmt of
    Nothing -> Right (st, fs)
    Just s -> emitStmt entry outTargets st fs s
  if fsTerminated fs1
    then Right (st1, fs1)
    else do
      let loopStack = fsLoopStack fs1
      let breakStack = fsBreakStack fs1
      let (headerLabel, st2) = freshId st1
      let (bodyLabel, st3) = freshId st2
      let (continueLabel, st4) = freshId st3
      let (mergeLabel, st5) = freshId st4

      let fs2 = addTerminator (Instr opBranch [headerLabel]) fs1
      let fsHeader0 = addLabel headerLabel fs2
      (st6, fsHeader1, condVal) <- case condExpr of
        Nothing -> do
          let (cid, st') = emitConstBool st5 True
          let (a, sz) = scalarLayout Bool
          let layout = TLScalar Bool a sz
          Right (st', fsHeader0, Value layout cid)
        Just expr -> emitExpr st5 fsHeader0 expr
      ensureBoolScalar (valType condVal)
      let fsHeader2 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader1
      let fsHeader3 = addTerminator (Instr opBranchConditional [valId condVal, bodyLabel, mergeLabel]) fsHeader2

      let fsBody0 = addLabel bodyLabel fsHeader3
      let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
      (st7, fsBody2) <- emitStmtList entry outTargets st6 fsBody1 body
      let fsBody3 = if fsTerminated fsBody2 then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

      let fsContinue0 = addLabel continueLabel fsBody3
      let fsContinue1 = fsContinue0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
      (st8, fsContinue2) <- case contStmt of
        Nothing -> Right (st7, fsContinue1)
        Just s -> emitStmt entry outTargets st7 fsContinue1 s
      let fsContinue3 =
            if fsTerminated fsContinue2
              then fsContinue2
              else addTerminator (Instr opBranch [headerLabel]) fsContinue2

      let fsMerge = addLabel mergeLabel fsContinue3
      let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
      Right (st8, fsMerge1)

emitSwitch :: EntryPoint -> [OutputTarget] -> Expr -> [SwitchCase] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitSwitch entry outTargets expr cases defBody st fs = do
  (st1, fs1, selVal) <- emitExpr st fs expr
  ensureSwitchType (valType selVal)
  cases' <- expandSwitchCases cases defBody
  let breakStack = fsBreakStack fs1
  let (mergeLabel, st2) = freshId st1
  let fs2 = fs1 { fsBreakStack = mergeLabel : breakStack }
  (st3, fs3) <- emitSwitchChain entry outTargets selVal cases' defBody st2 fs2
  let fs4 = if fsTerminated fs3 then fs3 else addTerminator (Instr opBranch [mergeLabel]) fs3
  let fs5 = addLabel mergeLabel fs4
  let fs6 = fs5 { fsBreakStack = breakStack }
  Right (st3, fs6)

emitSwitchChain :: EntryPoint -> [OutputTarget] -> Value -> [SwitchCase] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitSwitchChain entry outTargets selVal cases defBody st fs =
  case cases of
    [] ->
      case defBody of
        Nothing -> Right (st, fs)
        Just body -> emitStmtList entry outTargets st fs body
    (SwitchCase selectors body : rest) -> do
      (st1, fs1, condVal) <- emitSwitchCond selVal selectors st fs
      ensureBoolScalar (valType condVal)
      let (thenLabel, st2) = freshId st1
      let (elseLabel, st3) = freshId st2
      let (mergeLabel, st4) = freshId st3
      let fs2 = addFuncInstr (Instr opSelectionMerge [mergeLabel, selectionControlNone]) fs1
      let fs3 = addTerminator (Instr opBranchConditional [valId condVal, thenLabel, elseLabel]) fs2

      let fsThen0 = addLabel thenLabel fs3
      (st5, fsThen1) <- emitStmtList entry outTargets st4 fsThen0 body
      let fsThen2 = if fsTerminated fsThen1 then fsThen1 else addTerminator (Instr opBranch [mergeLabel]) fsThen1

      let fsElse0 = addLabel elseLabel fsThen2
      (st6, fsElse1) <- emitSwitchChain entry outTargets selVal rest defBody st5 fsElse0
      let fsElse2 = if fsTerminated fsElse1 then fsElse1 else addTerminator (Instr opBranch [mergeLabel]) fsElse1

      let fsMerge = addLabel mergeLabel fsElse2
      let fsMerge1 =
            if fsTerminated fsThen1 && fsTerminated fsElse1
              then addTerminator (Instr opUnreachable []) fsMerge
              else fsMerge
      Right (st6, fsMerge1)

emitBreak :: GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitBreak st fs =
  case fsBreakStack fs of
    [] -> Left (CompileError "break used outside of a loop or switch" Nothing Nothing)
    (mergeLabel:_) ->
      let fs1 = addTerminator (Instr opBranch [mergeLabel]) fs
      in Right (st, fs1)

emitContinue :: GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitContinue st fs =
  case fsLoopStack fs of
    [] -> Left (CompileError "continue used outside of a loop" Nothing Nothing)
    ((_, continueLabel):_) ->
      let fs1 = addTerminator (Instr opBranch [continueLabel]) fs
      in Right (st, fs1)

emitExpr :: GenState -> FuncState -> Expr -> Either CompileError (GenState, FuncState, Value)
emitExpr st fs expr =
  case expr of
    EInt n -> do
      (scalar, val) <- selectIntLiteralScalar n
      case scalar of
        I32 -> do
          let (cid, st1) = emitConstI32 st (fromIntegral val)
          let (a, sz) = scalarLayout I32
          let layout = TLScalar I32 a sz
          Right (st1, fs, Value layout cid)
        U32 -> do
          let (cid, st1) = emitConstU32 st (fromIntegral val)
          let (a, sz) = scalarLayout U32
          let layout = TLScalar U32 a sz
          Right (st1, fs, Value layout cid)
        _ -> Left (CompileError "integer literal must be i32 or u32" Nothing Nothing)
    EFloat f -> do
      let (cid, st1) = emitConstF32 st f
      let (a, sz) = scalarLayout F32
      let layout = TLScalar F32 a sz
      Right (st1, fs, Value layout cid)
    EBool b -> do
      let (cid, st1) = emitConstBool st b
      let (a, sz) = scalarLayout Bool
      let layout = TLScalar Bool a sz
      Right (st1, fs, Value layout cid)
    EVar name ->
      case lookup name (fsValues fs) of
        Just val -> Right (st, fs, val)
        Nothing ->
          case lookup name (gsConstValues st) of
            Just val -> Right (st, fs, val)
            Nothing -> emitLoadFromExpr st fs expr
    EField base field -> emitFieldExpr st fs base field
    EIndex _ _ -> emitLoadFromExpr st fs expr
    EUnary OpNeg inner -> do
      (st1, fs1, val) <- emitExpr st fs inner
      case valType val of
        TLScalar U32 _ _ ->
          case lookupConstKeyById st1 (valId val) of
            Just key -> do
              key' <- convertConstKey key I32
              let (cid, st2) = emitConstFromKey st1 key'
              let (a, sz) = scalarLayout I32
              let layout = TLScalar I32 a sz
              let (tyId, st3) = emitTypeFromLayout st2 layout
              let (resId, st4) = freshId st3
              let fs2 = addFuncInstr (Instr opSNegate [tyId, resId, cid]) fs1
              Right (st4, fs2, Value layout resId)
            Nothing -> Left (CompileError "unary minus is not supported for u32" Nothing Nothing)
        _ -> do
          opcode <- case classifyNumeric (valType val) of
            Nothing -> Left (CompileError "unary minus only supports scalar or vector numeric types" Nothing Nothing)
            Just (_, scalar) -> case scalar of
              F32 -> Right opFNegate
              F16 -> Right opFNegate
              I32 -> Right opSNegate
              U32 -> Left (CompileError "unary minus is not supported for u32" Nothing Nothing)
              Bool -> Left (CompileError "unary minus is not supported for bool" Nothing Nothing)
          let (tyId, st2) = emitTypeFromLayout st1 (valType val)
          let (resId, st3) = freshId st2
          let fs2 = addFuncInstr (Instr opcode [tyId, resId, valId val]) fs1
          Right (st3, fs2, Value (valType val) resId)
    EUnary OpNot inner -> do
      (st1, fs1, val) <- emitExpr st fs inner
      ensureBoolScalar (valType val)
      let (tyId, st2) = emitTypeFromLayout st1 (valType val)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opLogicalNot [tyId, resId, valId val]) fs1
      Right (st3, fs2, Value (valType val) resId)
    EUnary OpAddr inner ->
      case exprToLValue inner of
        Nothing -> Left (CompileError "address-of requires an addressable expression" Nothing Nothing)
        Just lv -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          let layout = TLPointer (viStorage ptrInfo) (varAccessToPtrAccess (viAccess ptrInfo)) (viType ptrInfo)
          Right (st1, fs1, Value layout (viPtrId ptrInfo))
    EUnary OpDeref inner -> do
      (st1, fs1, val) <- emitExpr st fs inner
      case valType val of
        TLPointer _ _ elemLayout -> do
          let (tyId, st2) = emitTypeFromLayout st1 elemLayout
          let (resId, st3) = freshId st2
          let fs2 = addFuncInstr (Instr opLoad [tyId, resId, valId val]) fs1
          Right (st3, fs2, Value elemLayout resId)
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)
    EBinary op lhs rhs -> do
      (st1, fs1, lval) <- emitExpr st fs lhs
      (st2, fs2, rval) <- emitExpr st1 fs1 rhs
      (st3, fs3, lval', rval', layout) <- coerceBinaryOperands lval rval st2 fs2
      emitBinary op layout (valId lval') (valId rval') st3 fs3
    ECall name args -> emitCall name args st fs
    EBitcast targetTy inner -> do
      (st1, fs1, val) <- emitExpr st fs inner
      targetLayout <- resolveBitcastLayout targetTy
      (srcN, srcSz, _) <- bitcastLayoutInfo (valType val)
      (dstN, dstSz, _) <- bitcastLayoutInfo targetLayout
      when (srcN /= dstN || srcSz /= dstSz) $
        Left (CompileError "bitcast source and target types must have the same size" Nothing Nothing)
      if valType val == targetLayout
        then Right (st1, fs1, val)
        else emitBitcastValue targetLayout val st1 fs1

emitLoadFromExpr :: GenState -> FuncState -> Expr -> Either CompileError (GenState, FuncState, Value)
emitLoadFromExpr st fs expr =
  case exprToLValue expr of
    Nothing -> Left (CompileError "expected addressable expression" Nothing Nothing)
    Just lv -> do
      (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
      case viType ptrInfo of
        TLAtomic _ -> Left (CompileError "use atomicLoad for atomic values" Nothing Nothing)
        _ -> pure ()
      let (tyId, st2) = emitTypeFromLayout st1 (viType ptrInfo)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opLoad [tyId, resId, viPtrId ptrInfo]) fs1
      Right (st3, fs2, Value (viType ptrInfo) resId)

emitFieldExpr :: GenState -> FuncState -> Expr -> Text -> Either CompileError (GenState, FuncState, Value)
emitFieldExpr st fs base field = do
  (st1, fs1, baseVal) <- emitExpr st fs base
  case valType baseVal of
    TLVector n scalar _ _ -> do
      idxs <- vectorFieldIndices field n
      case idxs of
        [ix] -> do
          let (a, sz) = scalarLayout scalar
          let layout = TLScalar scalar a sz
          let (tyId, st2) = emitTypeFromLayout st1 layout
          let (resId, st3) = freshId st2
          let fs2 = addFuncInstr (Instr opCompositeExtract [tyId, resId, valId baseVal, ix]) fs1
          Right (st3, fs2, Value layout resId)
        _ -> do
          let len = length idxs
          let (a, sz) = vectorLayout scalar len
          let layout = TLVector len scalar a sz
          let (tyId, st2) = emitTypeFromLayout st1 layout
          let (resId, st3) = freshId st2
          let shuffleOps = [tyId, resId, valId baseVal, valId baseVal] <> idxs
          let fs2 = addFuncInstr (Instr opVectorShuffle shuffleOps) fs1
          Right (st3, fs2, Value layout resId)
    TLStruct _ fields _ _ -> do
      (ix, fieldLayout) <- findField (textToString field) fields
      let (tyId, st2) = emitTypeFromLayout st1 fieldLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opCompositeExtract [tyId, resId, valId baseVal, fromIntegral ix]) fs1
      Right (st3, fs2, Value fieldLayout resId)
    _ -> Left (CompileError "field access requires struct or vector type" Nothing Nothing)

emitBinary :: BinOp -> TypeLayout -> Word32 -> Word32 -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitBinary op layout lhs rhs st fs =
  case op of
    OpAdd -> emitArith
    OpSub -> emitArith
    OpMul -> emitArith
    OpDiv -> emitArith
    OpMod -> emitMod
    OpAnd -> emitLogical
    OpOr -> emitLogical
    OpBitAnd -> emitBitwise
    OpBitOr -> emitBitwise
    OpBitXor -> emitBitwise
    OpShl -> emitShift
    OpShr -> emitShift
    OpEq -> emitCompare
    OpNe -> emitCompare
    OpLt -> emitCompare
    OpLe -> emitCompare
    OpGt -> emitCompare
    OpGe -> emitCompare
  where
    emitArith = do
      case classifyNumeric layout of
        Nothing -> Left (CompileError "binary operators only support scalar or vector numeric types" Nothing Nothing)
        Just (_, scalar) -> case scalar of
          Bool -> Left (CompileError "arithmetic operators do not support bool" Nothing Nothing)
          _ -> do
            let (tyId, st1) = emitTypeFromLayout st layout
            let (resId, st2) = freshId st1
            let opcode = case (scalar, op) of
                  (F32, OpAdd) -> opFAdd
                  (F16, OpAdd) -> opFAdd
                  (F32, OpSub) -> opFSub
                  (F16, OpSub) -> opFSub
                  (F32, OpMul) -> opFMul
                  (F16, OpMul) -> opFMul
                  (F32, OpDiv) -> opFDiv
                  (F16, OpDiv) -> opFDiv
                  (I32, OpAdd) -> opIAdd
                  (U32, OpAdd) -> opIAdd
                  (I32, OpSub) -> opISub
                  (U32, OpSub) -> opISub
                  (I32, OpMul) -> opIMul
                  (U32, OpMul) -> opIMul
                  (I32, OpDiv) -> opSDiv
                  (U32, OpDiv) -> opUDiv
                  _ -> opIAdd
            let fs1 = addFuncInstr (Instr opcode [tyId, resId, lhs, rhs]) fs
            Right (st2, fs1, Value layout resId)
    emitMod =
      case classifyNumeric layout of
        Nothing -> Left (CompileError "modulo only supports scalar or vector integer types" Nothing Nothing)
        Just (_, scalar) -> case scalar of
          I32 -> emitIntOp opSRem
          U32 -> emitIntOp opUMod
          _ -> Left (CompileError "modulo only supports i32 or u32 types" Nothing Nothing)
    emitLogical =
      case layout of
        TLScalar Bool _ _ -> do
          let (tyId, st1) = emitTypeFromLayout st layout
          let (resId, st2) = freshId st1
          let opcode = case op of
                OpAnd -> opLogicalAnd
                OpOr -> opLogicalOr
                _ -> opLogicalAnd
          let fs1 = addFuncInstr (Instr opcode [tyId, resId, lhs, rhs]) fs
          Right (st2, fs1, Value layout resId)
        _ -> Left (CompileError "logical operators require bool scalars" Nothing Nothing)
    emitBitwise =
      case classifyNumeric layout of
        Nothing -> Left (CompileError "bitwise operators require i32 or u32 scalar or vector types" Nothing Nothing)
        Just (_, scalar) -> case scalar of
          I32 -> emitIntOp opcode
          U32 -> emitIntOp opcode
          _ -> Left (CompileError "bitwise operators require i32 or u32 types" Nothing Nothing)
      where
        opcode = case op of
          OpBitAnd -> opBitwiseAnd
          OpBitOr -> opBitwiseOr
          OpBitXor -> opBitwiseXor
          _ -> opBitwiseAnd
    emitShift =
      case classifyNumeric layout of
        Nothing -> Left (CompileError "shift operators require i32 or u32 scalar or vector types" Nothing Nothing)
        Just (_, scalar) -> case scalar of
          I32 -> emitIntOp shiftOp
          U32 -> emitIntOp shiftOp
          _ -> Left (CompileError "shift operators require i32 or u32 types" Nothing Nothing)
      where
        shiftOp = case op of
          OpShl -> opShiftLeftLogical
          OpShr ->
            case classifyNumeric layout of
              Just (_, I32) -> opShiftRightArithmetic
              Just (_, U32) -> opShiftRightLogical
              _ -> opShiftRightLogical
          _ -> opShiftLeftLogical
    emitIntOp opcode = do
      let (tyId, st1) = emitTypeFromLayout st layout
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opcode [tyId, resId, lhs, rhs]) fs
      Right (st2, fs1, Value layout resId)
    emitCompare =
      case classifyNumeric layout of
        Nothing -> Left (CompileError "comparison operators only support scalar or vector numeric types" Nothing Nothing)
        Just (n, scalar) -> do
          let resultLayout = boolResultLayout n
          let (tyId, st1) = emitTypeFromLayout st resultLayout
          let (resId, st2) = freshId st1
          opcode <- case scalar of
            Bool ->
              if n /= 1
                then Left (CompileError "bool vector comparisons are not supported" Nothing Nothing)
                else case op of
                  OpEq -> Right opLogicalEqual
                  OpNe -> Right opLogicalNotEqual
                  _ -> Left (CompileError "ordered comparisons are not supported for bool" Nothing Nothing)
            F32 ->
              case op of
                OpEq -> Right opFOrdEqual
                OpNe -> Right opFOrdNotEqual
                OpLt -> Right opFOrdLessThan
                OpLe -> Right opFOrdLessThanEqual
                OpGt -> Right opFOrdGreaterThan
                OpGe -> Right opFOrdGreaterThanEqual
                _ -> Left (CompileError "unsupported float comparison" Nothing Nothing)
            F16 ->
              case op of
                OpEq -> Right opFOrdEqual
                OpNe -> Right opFOrdNotEqual
                OpLt -> Right opFOrdLessThan
                OpLe -> Right opFOrdLessThanEqual
                OpGt -> Right opFOrdGreaterThan
                OpGe -> Right opFOrdGreaterThanEqual
                _ -> Left (CompileError "unsupported float comparison" Nothing Nothing)
            I32 ->
              case op of
                OpEq -> Right opIEqual
                OpNe -> Right opINotEqual
                OpLt -> Right opSLessThan
                OpLe -> Right opSLessThanEqual
                OpGt -> Right opSGreaterThan
                OpGe -> Right opSGreaterThanEqual
                _ -> Left (CompileError "unsupported int comparison" Nothing Nothing)
            U32 ->
              case op of
                OpEq -> Right opIEqual
                OpNe -> Right opINotEqual
                OpLt -> Right opULessThan
                OpLe -> Right opULessThanEqual
                OpGt -> Right opUGreaterThan
                OpGe -> Right opUGreaterThanEqual
                _ -> Left (CompileError "unsupported uint comparison" Nothing Nothing)
          let fs1 = addFuncInstr (Instr opcode [tyId, resId, lhs, rhs]) fs
          Right (st2, fs1, Value resultLayout resId)

emitCall :: Text -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitCall name args st fs =
  case name of
    "vec2" -> emitVectorCtor 2 args st fs
    "vec3" -> emitVectorCtor 3 args st fs
    "vec4" -> emitVectorCtor 4 args st fs
    "array" -> emitArrayCtor args st fs
    "f16" -> emitScalarCtor F16 args st fs
    "f32" -> emitScalarCtor F32 args st fs
    "u32" -> emitScalarCtor U32 args st fs
    "i32" -> emitScalarCtor I32 args st fs
    "abs" -> emitAbsBuiltin args st fs
    "min" -> emitMinMaxBuiltin True args st fs
    "max" -> emitMinMaxBuiltin False args st fs
    "clamp" -> emitClampBuiltin args st fs
    "mix" -> emitGLSLTrinary glslStd450FMix args st fs
    "select" -> emitSelectBuiltin args st fs
    "any" -> emitAnyAll opAny args st fs
    "all" -> emitAnyAll opAll args st fs
    "round" -> emitGLSLUnary glslStd450Round args st fs
    "roundEven" -> emitGLSLUnary glslStd450RoundEven args st fs
    "trunc" -> emitGLSLUnary glslStd450Trunc args st fs
    "step" -> emitGLSLBinary glslStd450Step args st fs
    "smoothstep" -> emitGLSLTrinary glslStd450SmoothStep args st fs
    "floor" -> emitGLSLUnary glslStd450Floor args st fs
    "ceil" -> emitGLSLUnary glslStd450Ceil args st fs
    "fract" -> emitGLSLUnary glslStd450Fract args st fs
    "radians" -> emitGLSLUnary glslStd450Radians args st fs
    "degrees" -> emitGLSLUnary glslStd450Degrees args st fs
    "exp" -> emitGLSLUnary glslStd450Exp args st fs
    "log" -> emitGLSLUnary glslStd450Log args st fs
    "exp2" -> emitGLSLUnary glslStd450Exp2 args st fs
    "log2" -> emitGLSLUnary glslStd450Log2 args st fs
    "sin" -> emitGLSLUnary glslStd450Sin args st fs
    "cos" -> emitGLSLUnary glslStd450Cos args st fs
    "tan" -> emitGLSLUnary glslStd450Tan args st fs
    "asin" -> emitGLSLUnary glslStd450Asin args st fs
    "acos" -> emitGLSLUnary glslStd450Acos args st fs
    "atan" -> emitGLSLUnary glslStd450Atan args st fs
    "atan2" -> emitGLSLBinary glslStd450Atan2 args st fs
    "sinh" -> emitGLSLUnary glslStd450Sinh args st fs
    "cosh" -> emitGLSLUnary glslStd450Cosh args st fs
    "tanh" -> emitGLSLUnary glslStd450Tanh args st fs
    "asinh" -> emitGLSLUnary glslStd450Asinh args st fs
    "acosh" -> emitGLSLUnary glslStd450Acosh args st fs
    "atanh" -> emitGLSLUnary glslStd450Atanh args st fs
    "pow" -> emitGLSLBinary glslStd450Pow args st fs
    "sqrt" -> emitGLSLUnary glslStd450Sqrt args st fs
    "inverseSqrt" -> emitGLSLUnary glslStd450InverseSqrt args st fs
    "fma" -> emitGLSLTrinary glslStd450Fma args st fs
    "sign" -> emitSignBuiltin args st fs
    "length" -> emitGLSLLength args st fs
    "normalize" -> emitGLSLUnary glslStd450Normalize args st fs
    "dot" -> emitDot args st fs
    "cross" -> emitCrossBuiltin args st fs
    "distance" -> emitDistance args st fs
    "faceForward" -> emitFaceForwardBuiltin args st fs
    "reflect" -> emitReflect args st fs
    "refract" -> emitRefractBuiltin args st fs
    "transpose" -> emitTransposeBuiltin args st fs
    "determinant" -> emitDeterminantBuiltin args st fs
    "inverse" -> emitMatrixInverseBuiltin args st fs
    "modf" -> emitModfBuiltin args st fs
    "frexp" -> emitFrexpBuiltin args st fs
    "ldexp" -> emitLdexpBuiltin args st fs
    "pack4x8snorm" -> emitPackBuiltin glslStd450PackSnorm4x8 4 args st fs
    "pack4x8unorm" -> emitPackBuiltin glslStd450PackUnorm4x8 4 args st fs
    "pack2x16snorm" -> emitPackBuiltin glslStd450PackSnorm2x16 2 args st fs
    "pack2x16unorm" -> emitPackBuiltin glslStd450PackUnorm2x16 2 args st fs
    "pack2x16float" -> emitPackBuiltin glslStd450PackHalf2x16 2 args st fs
    "unpack4x8snorm" -> emitUnpackBuiltin glslStd450UnpackSnorm4x8 4 args st fs
    "unpack4x8unorm" -> emitUnpackBuiltin glslStd450UnpackUnorm4x8 4 args st fs
    "unpack2x16snorm" -> emitUnpackBuiltin glslStd450UnpackSnorm2x16 2 args st fs
    "unpack2x16unorm" -> emitUnpackBuiltin glslStd450UnpackUnorm2x16 2 args st fs
    "unpack2x16float" -> emitUnpackBuiltin glslStd450UnpackHalf2x16 2 args st fs
    "firstLeadingBit" -> emitFirstLeadingBitBuiltin args st fs
    "firstTrailingBit" -> emitFirstTrailingBitBuiltin args st fs
    "saturate" -> emitSaturateBuiltin args st fs
    "quantizeToF16" -> emitQuantizeToF16 args st fs
    "countOneBits" -> emitBitUnary opBitCount args st fs
    "countLeadingZeros" -> emitCountLeadingZeros args st fs
    "countTrailingZeros" -> emitCountTrailingZeros args st fs
    "reverseBits" -> emitBitUnary opBitReverse args st fs
    "extractBits" -> emitExtractBitsBuiltin args st fs
    "insertBits" -> emitInsertBitsBuiltin args st fs
    "dot4U8Packed" -> emitDot4Packed False args st fs
    "dot4I8Packed" -> emitDot4Packed True args st fs
    "arrayLength" -> emitArrayLengthBuiltin args st fs
    "textureSample" -> emitTextureSample args st fs
    "textureSampleCompare" -> emitTextureSampleCompare args st fs
    "textureSampleLevel" -> emitTextureSampleLevel args st fs
    "textureSampleBias" -> emitTextureSampleBias args st fs
    "textureSampleGrad" -> emitTextureSampleGrad args st fs
    "textureSampleCompareLevel" -> emitTextureSampleCompareLevel args st fs
    "textureGather" -> emitTextureGather args st fs
    "textureGatherCompare" -> emitTextureGatherCompare args st fs
    "textureDimensions" -> emitTextureDimensions args st fs
    "textureNumLevels" -> emitTextureNumLevels args st fs
    "textureNumLayers" -> emitTextureNumLayers args st fs
    "textureNumSamples" -> emitTextureNumSamples args st fs
    "dpdx" -> emitDerivative opDPdx args st fs
    "dpdy" -> emitDerivative opDPdy args st fs
    "fwidth" -> emitDerivative opFwidth args st fs
    "textureLoad" -> emitTextureLoad args st fs
    "atomicLoad" -> emitAtomicLoad args st fs
    "atomicAdd" -> emitAtomicBinary (\s -> case s of {I32 -> Right opAtomicIAdd; U32 -> Right opAtomicIAdd; _ -> Left (CompileError "atomicAdd requires i32 or u32" Nothing Nothing)}) args st fs
    "atomicSub" -> emitAtomicBinary (\s -> case s of {I32 -> Right opAtomicISub; U32 -> Right opAtomicISub; _ -> Left (CompileError "atomicSub requires i32 or u32" Nothing Nothing)}) args st fs
    "atomicMin" -> emitAtomicBinary (\s -> case s of {I32 -> Right opAtomicSMin; U32 -> Right opAtomicUMin; _ -> Left (CompileError "atomicMin requires i32 or u32" Nothing Nothing)}) args st fs
    "atomicMax" -> emitAtomicBinary (\s -> case s of {I32 -> Right opAtomicSMax; U32 -> Right opAtomicUMax; _ -> Left (CompileError "atomicMax requires i32 or u32" Nothing Nothing)}) args st fs
    "atomicAnd" -> emitAtomicBinary (\s -> case s of {I32 -> Right opAtomicAnd; U32 -> Right opAtomicAnd; _ -> Left (CompileError "atomicAnd requires i32 or u32" Nothing Nothing)}) args st fs
    "atomicOr" -> emitAtomicBinary (\s -> case s of {I32 -> Right opAtomicOr; U32 -> Right opAtomicOr; _ -> Left (CompileError "atomicOr requires i32 or u32" Nothing Nothing)}) args st fs
    "atomicXor" -> emitAtomicBinary (\s -> case s of {I32 -> Right opAtomicXor; U32 -> Right opAtomicXor; _ -> Left (CompileError "atomicXor requires i32 or u32" Nothing Nothing)}) args st fs
    "atomicExchange" -> emitAtomicBinary (\s -> case s of {I32 -> Right opAtomicExchange; U32 -> Right opAtomicExchange; _ -> Left (CompileError "atomicExchange requires i32 or u32" Nothing Nothing)}) args st fs
    _ ->
      case parseMatrixName name of
        Just (cols, rows) -> emitMatrixCtor cols rows args st fs
        Nothing ->
          case lookup name (gsStructLayouts st) of
            Just layout ->
              emitStructCtor name layout args st fs
            Nothing -> do
              (st1, fs1, mval) <- emitFunctionCallByName name args st fs
              case mval of
                Nothing -> Left (CompileError "void function call cannot be used as a value" Nothing Nothing)
                Just val -> Right (st1, fs1, val)

emitGLSLUnary :: Word32 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitGLSLUnary inst args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (st2, fs2, val') <- case valType val of
        TLScalar F32 _ _ -> Right (st1, fs1, val)
        TLScalar F16 _ _ -> Right (st1, fs1, val)
        TLScalar _ _ _ -> do
          let (a, sz) = scalarLayout F32
          let layout = TLScalar F32 a sz
          coerceValueToLayout layout val st1 fs1
        _ -> Right (st1, fs1, val)
      ensureFloatNumeric (valType val')
      emitExtInst (valType val') inst [valId val'] st2 fs2
    _ -> Left (CompileError "builtin expects one argument" Nothing Nothing)

emitGLSLBinary :: Word32 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitGLSLBinary inst args st fs =
  case args of
    [a, b] -> do
      (st1, fs1, v1) <- emitExpr st fs a
      (st2, fs2, v2) <- emitExpr st1 fs1 b
      (st3, fs3, v1', v2', layout) <-
        if valType v1 == valType v2
          then Right (st2, fs2, v1, v2, valType v1)
          else coerceBinaryOperands v1 v2 st2 fs2
      ensureFloatNumeric layout
      emitExtInst layout inst [valId v1', valId v2'] st3 fs3
    _ -> Left (CompileError "builtin expects two arguments" Nothing Nothing)

emitGLSLTrinary :: Word32 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitGLSLTrinary inst args st fs =
  case args of
    [a, b, c] -> do
      (st1, fs1, v1) <- emitExpr st fs a
      (st2, fs2, v2) <- emitExpr st1 fs1 b
      (st3, fs3, v3) <- emitExpr st2 fs2 c
      let targetLayout = pickBaseLayout st3 [v1, v2, v3]
      (st4, fs4, v1') <- coerceValueToLayout targetLayout v1 st3 fs3
      (st5, fs5, v2') <- coerceValueToLayout targetLayout v2 st4 fs4
      (st6, fs6, v3') <- coerceValueToLayout targetLayout v3 st5 fs5
      ensureFloatNumeric targetLayout
      emitExtInst targetLayout inst [valId v1', valId v2', valId v3'] st6 fs6
    _ -> Left (CompileError "builtin expects three arguments" Nothing Nothing)

emitGLSLLength :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitGLSLLength args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatVector (valType val)
      scalar <- case valType val of
        TLVector _ s _ _ -> Right s
        _ -> Left (CompileError "length expects a float vector" Nothing Nothing)
      let (align, size) = scalarLayout scalar
      let layout = TLScalar scalar align size
      emitExtInst layout glslStd450Length [valId val] st1 fs1
    _ -> Left (CompileError "length expects one argument" Nothing Nothing)

emitDot :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDot args st fs =
  case args of
    [a, b] -> do
      (st1, fs1, v1) <- emitExpr st fs a
      (st2, fs2, v2) <- emitExpr st1 fs1 b
      ensureTypeMatch (valType v1) (valType v2)
      ensureFloatVector (valType v1)
      scalar <- case valType v1 of
        TLVector _ s _ _ -> Right s
        _ -> Left (CompileError "dot expects float vectors" Nothing Nothing)
      let (align, size) = scalarLayout scalar
      let layout = TLScalar scalar align size
      let (tyId, st3) = emitTypeFromLayout st2 layout
      let (resId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opDot [tyId, resId, valId v1, valId v2]) fs2
      Right (st4, fs3, Value layout resId)
    _ -> Left (CompileError "dot expects two arguments" Nothing Nothing)

emitDistance :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDistance args st fs =
  case args of
    [a, b] -> do
      (st1, fs1, v1) <- emitExpr st fs a
      (st2, fs2, v2) <- emitExpr st1 fs1 b
      (st3, fs3, v1', v2', layout) <-
        if valType v1 == valType v2
          then Right (st2, fs2, v1, v2, valType v1)
          else coerceBinaryOperands v1 v2 st2 fs2
      ensureFloatNumeric layout
      case layout of
        TLScalar _ _ _ -> do
          (st4, fs4, diff) <- emitBinary OpSub layout (valId v1') (valId v2') st3 fs3
          emitExtInst (valType diff) glslStd450FAbs [valId diff] st4 fs4
        TLVector _ scalar _ _ -> do
          (st4, fs4, diff) <- emitBinary OpSub layout (valId v1') (valId v2') st3 fs3
          let (a', sz') = scalarLayout scalar
          let distLayout = TLScalar scalar a' sz'
          emitExtInst distLayout glslStd450Length [valId diff] st4 fs4
        _ -> Left (CompileError "distance expects float scalar or vector arguments" Nothing Nothing)
    _ -> Left (CompileError "distance expects two arguments" Nothing Nothing)

emitReflect :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitReflect args st fs =
  case args of
    [iExpr, nExpr] -> do
      (st1, fs1, iVal) <- emitExpr st fs iExpr
      (st2, fs2, nVal) <- emitExpr st1 fs1 nExpr
      ensureTypeMatch (valType iVal) (valType nVal)
      ensureFloatVector (valType iVal)
      scalar <- case valType iVal of
        TLVector _ s _ _ -> Right s
        _ -> Left (CompileError "reflect expects float vector arguments" Nothing Nothing)
      let (a', sz') = scalarLayout scalar
      let dotLayout = TLScalar scalar a' sz'
      let (dotTy, st3) = emitTypeFromLayout st2 dotLayout
      let (dotId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opDot [dotTy, dotId, valId nVal, valId iVal]) fs2
      (twoId, st5) <- emitConstFloatScalar scalar 2.0 st4
      (st6, fs4, scaleVal) <- emitBinary OpMul dotLayout dotId twoId st5 fs3
      (st7, fs5, scaleVec) <- emitSplatVector (valType iVal) (valId scaleVal) st6 fs4
      (st8, fs6, scaledN) <- emitBinary OpMul (valType nVal) (valId nVal) (valId scaleVec) st7 fs5
      emitBinary OpSub (valType iVal) (valId iVal) (valId scaledN) st8 fs6
    _ -> Left (CompileError "reflect expects two arguments" Nothing Nothing)

emitSelectValue :: TypeLayout -> Value -> Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSelectValue layout trueVal falseVal condVal st fs = do
  let (tyId, st1) = emitTypeFromLayout st layout
  let (resId, st2) = freshId st1
  let fs1 = addFuncInstr (Instr opSelect [tyId, resId, valId condVal, valId trueVal, valId falseVal]) fs
  Right (st2, fs1, Value layout resId)

emitSelectBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSelectBuiltin args st fs =
  case args of
    [aExpr, bExpr, condExpr] -> do
      (st1, fs1, aVal) <- emitExpr st fs aExpr
      (st2, fs2, bVal) <- emitExpr st1 fs1 bExpr
      (st3, fs3, condVal) <- emitExpr st2 fs2 condExpr
      (st4, fs4, aVal', bVal', layout) <- coerceBinaryOperands aVal bVal st3 fs3
      case layout of
        TLScalar _ _ _ -> ensureBoolScalar (valType condVal)
        TLVector n _ _ _ -> ensureBoolVectorSize n (valType condVal)
        _ -> Left (CompileError "select expects scalar or vector types" Nothing Nothing)
      emitSelectValue layout bVal' aVal' condVal st4 fs4
    _ -> Left (CompileError "select expects (a, b, cond)" Nothing Nothing)

emitAnyAll :: Word16 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitAnyAll opcode args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case valType val of
        TLScalar Bool _ _ -> Right (st1, fs1, val)
        TLVector _ Bool _ _ -> do
          let (a, sz) = scalarLayout Bool
          let layout = TLScalar Bool a sz
          let (tyId, st2) = emitTypeFromLayout st1 layout
          let (resId, st3) = freshId st2
          let fs2 = addFuncInstr (Instr opcode [tyId, resId, valId val]) fs1
          Right (st3, fs2, Value layout resId)
        _ -> Left (CompileError "any/all expect a bool vector" Nothing Nothing)
    _ -> Left (CompileError "any/all expect one argument" Nothing Nothing)

layoutSuffix :: TypeLayout -> Either CompileError String
layoutSuffix layout =
  case layout of
    TLScalar s _ _ -> Right (scalarSuffix s)
    TLVector n s _ _ -> Right ("v" <> show n <> "_" <> scalarSuffix s)
    TLMatrix c r s _ _ _ -> Right ("m" <> show c <> "x" <> show r <> "_" <> scalarSuffix s)
    _ -> Left (CompileError "unsupported layout for builtin struct" Nothing Nothing)
  where
    scalarSuffix s = case s of
      I32 -> "i32"
      U32 -> "u32"
      F16 -> "f16"
      F32 -> "f32"
      Bool -> "bool"

makeBuiltinStructLayout :: String -> [(String, TypeLayout)] -> TypeLayout
makeBuiltinStructLayout name fields =
  let (fields', align) = buildFields 0 [] 1 fields
      size = structSize fields' align
  in TLStruct name fields' align size
  where
    buildFields _ acc alignAcc [] = (reverse acc, alignAcc)
    buildFields offset acc alignAcc ((fname, fLayout):rest) =
      let fAlign = layoutAlign fLayout
          fSize = layoutSize fLayout
          aligned = roundUp offset fAlign
          entry = FieldLayout fname aligned fLayout fAlign fSize
          alignAcc' = max alignAcc fAlign
      in buildFields (aligned + fSize) (entry:acc) alignAcc' rest

modfStructLayout :: TypeLayout -> Either CompileError TypeLayout
modfStructLayout baseLayout = do
  suffix <- layoutSuffix baseLayout
  let name = "__modf_result_" <> suffix
  pure (makeBuiltinStructLayout name [("fract", baseLayout), ("whole", baseLayout)])

frexpStructLayout :: TypeLayout -> Either CompileError TypeLayout
frexpStructLayout baseLayout = do
  suffix <- layoutSuffix baseLayout
  expLayout <- case baseLayout of
    TLScalar _ _ _ ->
      let (a, sz) = scalarLayout I32
      in Right (TLScalar I32 a sz)
    TLVector n _ _ _ ->
      let (a, sz) = vectorLayout I32 n
      in Right (TLVector n I32 a sz)
    _ -> Left (CompileError "frexp expects float scalar or vector" Nothing Nothing)
  let name = "__frexp_result_" <> suffix
  pure (makeBuiltinStructLayout name [("fract", baseLayout), ("exp", expLayout)])

emitFloatConvert :: Scalar -> Scalar -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFloatConvert from to val st fs =
  case valType val of
    TLScalar s _ _ | s == from -> emitScalarConvert from to val st fs
    TLVector n s _ _ | s == from -> do
      let (a, sz) = vectorLayout to n
      let layout = TLVector n to a sz
      let (tyId, st1) = emitTypeFromLayout st layout
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opFConvert [tyId, resId, valId val]) fs
      Right (st2, fs1, Value layout resId)
    _ -> Left (CompileError "expected float scalar or vector for conversion" Nothing Nothing)

emitQuantizeToF16 :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitQuantizeToF16 args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case valType val of
        TLScalar F32 _ _ -> do
          (st2, fs2, halfVal) <- emitFloatConvert F32 F16 val st1 fs1
          emitFloatConvert F16 F32 halfVal st2 fs2
        TLVector _ F32 _ _ -> do
          (st2, fs2, halfVal) <- emitFloatConvert F32 F16 val st1 fs1
          emitFloatConvert F16 F32 halfVal st2 fs2
        _ -> Left (CompileError "quantizeToF16 expects f32 scalar or vector" Nothing Nothing)
    _ -> Left (CompileError "quantizeToF16 expects one argument" Nothing Nothing)

emitModfBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitModfBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatNumeric (valType val)
      layout <- modfStructLayout (valType val)
      emitExtInst layout glslStd450ModfStruct [valId val] st1 fs1
    _ -> Left (CompileError "modf expects one argument" Nothing Nothing)

emitFrexpBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFrexpBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatNumeric (valType val)
      layout <- frexpStructLayout (valType val)
      emitExtInst layout glslStd450FrexpStruct [valId val] st1 fs1
    _ -> Left (CompileError "frexp expects one argument" Nothing Nothing)

emitLdexpBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitLdexpBuiltin args st fs =
  case args of
    [xExpr, expExpr] -> do
      (st1, fs1, xVal) <- emitExpr st fs xExpr
      ensureFloatNumeric (valType xVal)
      (st2, fs2, expVal) <- emitExpr st1 fs1 expExpr
      (st3, fs3, expVal') <- coerceExpToI32 (valType xVal) expVal st2 fs2
      emitExtInst (valType xVal) glslStd450Ldexp [valId xVal, valId expVal'] st3 fs3
    _ -> Left (CompileError "ldexp expects (x, exp)" Nothing Nothing)

emitPackBuiltin :: Word32 -> Int -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitPackBuiltin inst n args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatVecN n (valType val)
      let (a, sz) = scalarLayout U32
      let layout = TLScalar U32 a sz
      emitExtInst layout inst [valId val] st1 fs1
    _ -> Left (CompileError "pack builtin expects one argument" Nothing Nothing)

emitUnpackBuiltin :: Word32 -> Int -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitUnpackBuiltin inst n args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      let (a, sz) = scalarLayout U32
      let layoutU32 = TLScalar U32 a sz
      (st2, fs2, val') <- coerceValueToLayout layoutU32 val st1 fs1
      let (va, vsz) = vectorLayout F32 n
      let layout = TLVector n F32 va vsz
      emitExtInst layout inst [valId val'] st2 fs2
    _ -> Left (CompileError "unpack builtin expects one argument" Nothing Nothing)

emitTransposeBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTransposeBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (cols, rows, scalar) <- ensureFloatMatrix (valType val)
      let layout = matrixLayout rows cols scalar
      let (tyId, st2) = emitTypeFromLayout st1 layout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opTranspose [tyId, resId, valId val]) fs1
      Right (st3, fs2, Value layout resId)
    _ -> Left (CompileError "transpose expects one argument" Nothing Nothing)

emitDeterminantBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDeterminantBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (cols, rows, scalar) <- ensureFloatMatrix (valType val)
      when (cols /= rows) $
        Left (CompileError "determinant expects a square matrix" Nothing Nothing)
      let (a, sz) = scalarLayout scalar
      let layout = TLScalar scalar a sz
      emitExtInst layout glslStd450Determinant [valId val] st1 fs1
    _ -> Left (CompileError "determinant expects one argument" Nothing Nothing)

emitMatrixInverseBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitMatrixInverseBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (cols, rows, _) <- ensureFloatMatrix (valType val)
      when (cols /= rows) $
        Left (CompileError "inverse expects a square matrix" Nothing Nothing)
      emitExtInst (valType val) glslStd450MatrixInverse [valId val] st1 fs1
    _ -> Left (CompileError "inverse expects one argument" Nothing Nothing)

emitCrossBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitCrossBuiltin args st fs =
  case args of
    [aExpr, bExpr] -> do
      (st1, fs1, v1) <- emitExpr st fs aExpr
      (st2, fs2, v2) <- emitExpr st1 fs1 bExpr
      (st3, fs3, v1', v2', layout) <-
        if valType v1 == valType v2
          then Right (st2, fs2, v1, v2, valType v1)
          else coerceBinaryOperands v1 v2 st2 fs2
      ensureFloatVec3 layout
      emitExtInst layout glslStd450Cross [valId v1', valId v2'] st3 fs3
    _ -> Left (CompileError "cross expects two arguments" Nothing Nothing)

emitFaceForwardBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFaceForwardBuiltin args st fs =
  case args of
    [nExpr, iExpr, nrefExpr] -> do
      (st1, fs1, nVal) <- emitExpr st fs nExpr
      (st2, fs2, iVal) <- emitExpr st1 fs1 iExpr
      (st3, fs3, nrefVal) <- emitExpr st2 fs2 nrefExpr
      (st4, fs4, nVal', iVal', layout) <-
        if valType nVal == valType iVal
          then Right (st3, fs3, nVal, iVal, valType nVal)
          else coerceBinaryOperands nVal iVal st3 fs3
      (st5, fs5, nrefVal') <- coerceValueToLayout layout nrefVal st4 fs4
      ensureFloatVector layout
      emitExtInst layout glslStd450FaceForward [valId nVal', valId iVal', valId nrefVal'] st5 fs5
    _ -> Left (CompileError "faceForward expects three arguments" Nothing Nothing)

emitRefractBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitRefractBuiltin args st fs =
  case args of
    [iExpr, nExpr, etaExpr] -> do
      (st1, fs1, iVal) <- emitExpr st fs iExpr
      (st2, fs2, nVal) <- emitExpr st1 fs1 nExpr
      (st3, fs3, etaVal) <- emitExpr st2 fs2 etaExpr
      (st4, fs4, iVal', nVal', layout) <-
        if valType iVal == valType nVal
          then Right (st3, fs3, iVal, nVal, valType iVal)
          else coerceBinaryOperands iVal nVal st3 fs3
      ensureFloatVector layout
      (st5, fs5, etaScalar) <- case valType etaVal of
        TLScalar s _ _ ->
          if s == scalarFromLayout layout
            then Right (st4, fs4, etaVal)
            else do
              (st5, fs5, etaConverted) <- emitScalarConvert s (scalarFromLayout layout) etaVal st4 fs4
              Right (st5, fs5, etaConverted)
        _ -> Left (CompileError "refract expects a float scalar for eta" Nothing Nothing)
      emitExtInst layout glslStd450Refract [valId iVal', valId nVal', valId etaScalar] st5 fs5
    _ -> Left (CompileError "refract expects three arguments" Nothing Nothing)
  where
    scalarFromLayout layout =
      case layout of
        TLVector _ s _ _ -> s
        _ -> F32

emitFirstTrailingBitBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFirstTrailingBitBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureIntNumeric (valType val)
      emitExtInst (valType val) glslStd450FindILsb [valId val] st1 fs1
    _ -> Left (CompileError "firstTrailingBit expects one argument" Nothing Nothing)

emitFirstLeadingBitBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFirstLeadingBitBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureIntNumeric (valType val)
      let inst = case classifyNumeric (valType val) of
            Just (_, I32) -> glslStd450FindSMsb
            Just (_, U32) -> glslStd450FindUMsb
            _ -> glslStd450FindSMsb
      emitExtInst (valType val) inst [valId val] st1 fs1
    _ -> Left (CompileError "firstLeadingBit expects one argument" Nothing Nothing)

emitSaturateBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSaturateBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatNumeric (valType val)
      (st2, zeroVal) <- emitConstFloatSplat (valType val) 0.0 st1
      (st3, oneVal) <- emitConstFloatSplat (valType val) 1.0 st2
      emitExtInst (valType val) glslStd450FClamp [valId val, valId zeroVal, valId oneVal] st3 fs1
    _ -> Left (CompileError "saturate expects one argument" Nothing Nothing)

emitIntNegate :: Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitIntNegate val st fs =
  case valType val of
    TLScalar I32 _ _ -> do
      let (tyId, st1) = emitTypeFromLayout st (valType val)
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opSNegate [tyId, resId, valId val]) fs
      Right (st2, fs1, Value (valType val) resId)
    TLVector _ I32 _ _ -> do
      let (tyId, st1) = emitTypeFromLayout st (valType val)
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opSNegate [tyId, resId, valId val]) fs
      Right (st2, fs1, Value (valType val) resId)
    _ -> Left (CompileError "expected i32 scalar or vector" Nothing Nothing)

emitAbsBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitAbsBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case classifyNumeric (valType val) of
        Just (_, F32) -> emitExtInst (valType val) glslStd450FAbs [valId val] st1 fs1
        Just (_, F16) -> emitExtInst (valType val) glslStd450FAbs [valId val] st1 fs1
        Just (_, I32) -> do
          (st2, zeroVal) <- emitConstIntSplat (valType val) 0 st1
          (st3, fs3, condVal) <- emitBinary OpLt (valType val) (valId val) (valId zeroVal) st2 fs1
          (st4, fs4, negVal) <- emitIntNegate val st3 fs3
          emitSelectValue (valType val) negVal val condVal st4 fs4
        Just (_, U32) -> Right (st1, fs1, val)
        _ -> Left (CompileError "abs expects numeric scalar or vector types" Nothing Nothing)
    _ -> Left (CompileError "abs expects one argument" Nothing Nothing)

emitMinMaxInt :: BinOp -> TypeLayout -> Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitMinMaxInt cmpOp layout v1 v2 st fs = do
  (st1, fs1, condVal) <- emitBinary cmpOp layout (valId v1) (valId v2) st fs
  emitSelectValue layout v1 v2 condVal st1 fs1

emitMinMaxBuiltin :: Bool -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitMinMaxBuiltin isMin args st fs =
  case args of
    [aExpr, bExpr] -> do
      (st1, fs1, v1) <- emitExpr st fs aExpr
      (st2, fs2, v2) <- emitExpr st1 fs1 bExpr
      (st3, fs3, v1', v2', layout) <-
        if valType v1 == valType v2
          then Right (st2, fs2, v1, v2, valType v1)
          else coerceBinaryOperands v1 v2 st2 fs2
      case classifyNumeric layout of
        Just (_, F32) ->
          let inst = if isMin then glslStd450FMin else glslStd450FMax
          in emitExtInst layout inst [valId v1', valId v2'] st3 fs3
        Just (_, F16) ->
          let inst = if isMin then glslStd450FMin else glslStd450FMax
          in emitExtInst layout inst [valId v1', valId v2'] st3 fs3
        Just (_, I32) -> do
          let cmpOp = if isMin then OpLt else OpGt
          emitMinMaxInt cmpOp layout v1' v2' st3 fs3
        Just (_, U32) -> do
          let cmpOp = if isMin then OpLt else OpGt
          emitMinMaxInt cmpOp layout v1' v2' st3 fs3
        _ -> Left (CompileError "min/max expect numeric scalar or vector types" Nothing Nothing)
    _ -> Left (CompileError "min/max expect two arguments" Nothing Nothing)

emitClampBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitClampBuiltin args st fs =
  case args of
    [xExpr, lowExpr, highExpr] -> do
      (st1, fs1, vx) <- emitExpr st fs xExpr
      (st2, fs2, vlo) <- emitExpr st1 fs1 lowExpr
      (st3, fs3, vhi) <- emitExpr st2 fs2 highExpr
      let baseLayout = pickBaseLayout st3 [vx, vlo, vhi]
      case classifyNumeric baseLayout of
        Just (_, F32) -> do
          (st4, fs4, vx') <- coerceValueToLayout baseLayout vx st3 fs3
          (st5, fs5, vlo') <- coerceValueToLayout baseLayout vlo st4 fs4
          (st6, fs6, vhi') <- coerceValueToLayout baseLayout vhi st5 fs5
          emitExtInst baseLayout glslStd450FClamp [valId vx', valId vlo', valId vhi'] st6 fs6
        Just (_, F16) -> do
          (st4, fs4, vx') <- coerceValueToLayout baseLayout vx st3 fs3
          (st5, fs5, vlo') <- coerceValueToLayout baseLayout vlo st4 fs4
          (st6, fs6, vhi') <- coerceValueToLayout baseLayout vhi st5 fs5
          emitExtInst baseLayout glslStd450FClamp [valId vx', valId vlo', valId vhi'] st6 fs6
        Just (_, I32) -> do
          (st4, fs4, vx') <- coerceValueToLayout baseLayout vx st3 fs3
          (st5, fs5, vlo') <- coerceValueToLayout baseLayout vlo st4 fs4
          (st6, fs6, vhi') <- coerceValueToLayout baseLayout vhi st5 fs5
          (st7, fs7, maxVal) <- emitMinMaxInt OpGt baseLayout vx' vlo' st6 fs6
          emitMinMaxInt OpLt baseLayout maxVal vhi' st7 fs7
        Just (_, U32) -> do
          (st4, fs4, vx') <- coerceValueToLayout baseLayout vx st3 fs3
          (st5, fs5, vlo') <- coerceValueToLayout baseLayout vlo st4 fs4
          (st6, fs6, vhi') <- coerceValueToLayout baseLayout vhi st5 fs5
          (st7, fs7, maxVal) <- emitMinMaxInt OpGt baseLayout vx' vlo' st6 fs6
          emitMinMaxInt OpLt baseLayout maxVal vhi' st7 fs7
        _ -> Left (CompileError "clamp expects numeric scalar or vector types" Nothing Nothing)
    _ -> Left (CompileError "clamp expects three arguments" Nothing Nothing)

emitSignBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSignBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case classifyNumeric (valType val) of
        Just (_, F32) -> emitSignFloat (valType val) val st1 fs1
        Just (_, F16) -> emitSignFloat (valType val) val st1 fs1
        Just (_, I32) -> emitSignInt (valType val) val st1 fs1
        _ -> Left (CompileError "sign expects i32 or float scalar/vector types" Nothing Nothing)
    _ -> Left (CompileError "sign expects one argument" Nothing Nothing)
  where
    emitSignFloat layout val st1 fs1 = do
      (st2, zeroVal) <- emitConstFloatSplat layout 0.0 st1
      (st3, oneVal) <- emitConstFloatSplat layout 1.0 st2
      (st4, negOneVal) <- emitConstFloatSplat layout (-1.0) st3
      (st5, fs2, condPos) <- emitBinary OpGt layout (valId val) (valId zeroVal) st4 fs1
      (st6, fs3, condNeg) <- emitBinary OpLt layout (valId val) (valId zeroVal) st5 fs2
      (st7, fs4, posVal) <- emitSelectValue layout oneVal zeroVal condPos st6 fs3
      emitSelectValue layout negOneVal posVal condNeg st7 fs4
    emitSignInt layout val st1 fs1 = do
      (st2, zeroVal) <- emitConstIntSplat layout 0 st1
      (st3, oneVal) <- emitConstIntSplat layout 1 st2
      (st4, negOneVal) <- emitConstIntSplat layout (-1) st3
      (st5, fs2, condPos) <- emitBinary OpGt layout (valId val) (valId zeroVal) st4 fs1
      (st6, fs3, condNeg) <- emitBinary OpLt layout (valId val) (valId zeroVal) st5 fs2
      (st7, fs4, posVal) <- emitSelectValue layout oneVal zeroVal condPos st6 fs3
      emitSelectValue layout negOneVal posVal condNeg st7 fs4

emitBitUnary :: Word16 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitBitUnary opcode args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureIntNumeric (valType val)
      let (tyId, st2) = emitTypeFromLayout st1 (valType val)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opcode [tyId, resId, valId val]) fs1
      Right (st3, fs2, Value (valType val) resId)
    _ -> Left (CompileError "bitwise builtin expects one argument" Nothing Nothing)

emitCountLeadingZeros :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitCountLeadingZeros args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (st2, fs2, valU, layoutU, origLayout) <- toUnsignedValue val st1 fs1
      let bitWidth = intBitWidth layoutU
      (st3, zeroVal) <- emitConstIntSplat layoutU 0 st2
      (st4, fs3, condZero) <- emitBinary OpEq layoutU (valId valU) (valId zeroVal) st3 fs2
      (st5, bitWidthVal) <- emitConstIntSplat layoutU bitWidth st4
      (st6, oneLessVal) <- emitConstIntSplat layoutU (bitWidth - 1) st5
      (st7, fs4, msbVal) <- emitExtInst layoutU glslStd450FindUMsb [valId valU] st6 fs3
      (st8, fs5, clzVal) <- emitBinary OpSub layoutU (valId oneLessVal) (valId msbVal) st7 fs4
      (st9, fs6, resU) <- emitSelectValue layoutU bitWidthVal clzVal condZero st8 fs5
      fromUnsignedValue origLayout resU st9 fs6
    _ -> Left (CompileError "countLeadingZeros expects one argument" Nothing Nothing)

emitCountTrailingZeros :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitCountTrailingZeros args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (st2, fs2, valU, layoutU, origLayout) <- toUnsignedValue val st1 fs1
      let bitWidth = intBitWidth layoutU
      (st3, zeroVal) <- emitConstIntSplat layoutU 0 st2
      (st4, fs3, condZero) <- emitBinary OpEq layoutU (valId valU) (valId zeroVal) st3 fs2
      (st5, bitWidthVal) <- emitConstIntSplat layoutU bitWidth st4
      (st6, fs4, lsbVal) <- emitExtInst layoutU glslStd450FindILsb [valId valU] st5 fs3
      (st7, fs5, resU) <- emitSelectValue layoutU bitWidthVal lsbVal condZero st6 fs4
      fromUnsignedValue origLayout resU st7 fs5
    _ -> Left (CompileError "countTrailingZeros expects one argument" Nothing Nothing)

emitDot4Packed :: Bool -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDot4Packed signed args st fs =
  case args of
    [aExpr, bExpr] -> do
      (st1, fs1, aVal) <- emitExpr st fs aExpr
      (st2, fs2, bVal) <- emitExpr st1 fs1 bExpr
      let targetScalar = if signed then I32 else U32
          (a, sz) = scalarLayout targetScalar
          layout = TLScalar targetScalar a sz
      (st3, fs3, aPacked) <- coercePackedScalar "dot4 packed value" layout aVal st2 fs2
      (st4, fs4, bPacked) <- coercePackedScalar "dot4 packed value" layout bVal st3 fs3
      (st5, sum0) <- emitConstIntSplat layout 0 st4
      let offsets = [0, 8, 16, 24] :: [Integer]
          opcode = if signed then opBitFieldSExtract else opBitFieldUExtract
          go st' fs' acc [] = Right (st', fs', acc)
          go st' fs' acc (off:rest) = do
            (stA, fsA, aLane) <- emitBitFieldExtractConst opcode layout aPacked off 8 st' fs'
            (stB, fsB, bLane) <- emitBitFieldExtractConst opcode layout bPacked off 8 stA fsA
            (stC, fsC, prod) <- emitBinary OpMul layout (valId aLane) (valId bLane) stB fsB
            (stD, fsD, sum') <- emitBinary OpAdd layout (valId acc) (valId prod) stC fsC
            go stD fsD sum' rest
      (st6, fs6, sumVal) <- go st5 fs4 sum0 offsets
      Right (st6, fs6, sumVal)
    _ -> Left (CompileError "dot4 packed expects two arguments" Nothing Nothing)
  where
    coercePackedScalar label layout val st1 fs1 =
      case valType val of
        TLScalar I32 _ _ ->
          if layout == valType val
            then Right (st1, fs1, val)
            else emitBitcastValue layout val st1 fs1
        TLScalar U32 _ _ ->
          if layout == valType val
            then Right (st1, fs1, val)
            else emitBitcastValue layout val st1 fs1
        _ -> Left (CompileError (label <> " must be i32 or u32 scalar") Nothing Nothing)

emitBitFieldExtractConst :: Word16 -> TypeLayout -> Value -> Integer -> Integer -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitBitFieldExtractConst opcode layout baseVal offset count st fs = do
  (st1, offsetVal) <- emitConstIntSplat layout offset st
  (st2, countVal) <- emitConstIntSplat layout count st1
  let (tyId, st3) = emitTypeFromLayout st2 layout
  let (resId, st4) = freshId st3
  let fs1 = addFuncInstr (Instr opcode [tyId, resId, valId baseVal, valId offsetVal, valId countVal]) fs
  Right (st4, fs1, Value layout resId)

toUnsignedValue :: Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value, TypeLayout, TypeLayout)
toUnsignedValue val st fs =
  case valType val of
    TLScalar U32 _ _ -> Right (st, fs, val, valType val, valType val)
    TLScalar I32 _ _ -> do
      let (a, sz) = scalarLayout U32
          layoutU = TLScalar U32 a sz
      (st1, fs1, valU) <- emitBitcastValue layoutU val st fs
      Right (st1, fs1, valU, layoutU, valType val)
    TLVector _ U32 _ _ -> Right (st, fs, val, valType val, valType val)
    TLVector n I32 _ _ -> do
      let (a, sz) = vectorLayout U32 n
          layoutU = TLVector n U32 a sz
      (st1, fs1, valU) <- emitBitcastValue layoutU val st fs
      Right (st1, fs1, valU, layoutU, valType val)
    _ -> Left (CompileError "countLeadingZeros/countTrailingZeros expect i32 or u32 scalar or vector types" Nothing Nothing)

fromUnsignedValue :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
fromUnsignedValue origLayout valU st fs =
  case origLayout of
    TLScalar I32 _ _ -> emitBitcastValue origLayout valU st fs
    TLVector _ I32 _ _ -> emitBitcastValue origLayout valU st fs
    _ -> Right (st, fs, valU)

intBitWidth :: TypeLayout -> Integer
intBitWidth layout =
  case layout of
    TLScalar U32 _ _ -> 32
    TLVector _ U32 _ _ -> 32
    _ -> 32

resolveBitcastLayout :: Type -> Either CompileError TypeLayout
resolveBitcastLayout ty =
  case ty of
    TyScalar scalar ->
      case scalar of
        Bool -> Left (CompileError "bitcast does not support bool types" Nothing Nothing)
        _ ->
          let (a, sz) = scalarLayout scalar
          in Right (TLScalar scalar a sz)
    TyVector n scalar ->
      case scalar of
        Bool -> Left (CompileError "bitcast does not support bool types" Nothing Nothing)
        _ ->
          let (a, sz) = vectorLayout scalar n
          in Right (TLVector n scalar a sz)
    _ -> Left (CompileError "bitcast expects a scalar or vector type" Nothing Nothing)

bitcastLayoutInfo :: TypeLayout -> Either CompileError (Int, Word32, Scalar)
bitcastLayoutInfo layout =
  case layout of
    TLScalar scalar _ _ ->
      if scalar == Bool
        then Left (CompileError "bitcast does not support bool types" Nothing Nothing)
        else
          let (_, sz) = scalarLayout scalar
          in Right (1, sz, scalar)
    TLVector n scalar _ _ ->
      if scalar == Bool
        then Left (CompileError "bitcast does not support bool types" Nothing Nothing)
        else
          let (_, sz) = scalarLayout scalar
          in Right (n, sz, scalar)
    _ -> Left (CompileError "bitcast expects a scalar or vector type" Nothing Nothing)

normalizeBitFieldIndex :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
normalizeBitFieldIndex baseLayout idxVal st fs =
  case baseLayout of
    TLScalar baseScalar _ _ -> do
      ensureIndexType (valType idxVal)
      case valType idxVal of
        TLScalar s _ _ | s == baseScalar -> Right (st, fs, idxVal)
        TLScalar s _ _ -> emitScalarConvert s baseScalar idxVal st fs
        _ -> Left (CompileError "bitfield indices must be scalar" Nothing Nothing)
    TLVector n baseScalar _ _ ->
      case valType idxVal of
        TLScalar s _ _ -> do
          ensureIndexType (valType idxVal)
          (st1, fs1, scalarVal) <-
            if s == baseScalar
              then Right (st, fs, idxVal)
              else emitScalarConvert s baseScalar idxVal st fs
          let (a, sz) = vectorLayout baseScalar n
          let layout = TLVector n baseScalar a sz
          emitSplatVector layout (valId scalarVal) st1 fs1
        TLVector m s _ _ | m == n && s == baseScalar -> Right (st, fs, idxVal)
        TLVector _ _ _ _ -> Left (CompileError "bitfield indices must match vector size and scalar type" Nothing Nothing)
        _ -> Left (CompileError "bitfield indices must be scalar or matching vector" Nothing Nothing)
    _ -> Left (CompileError "bitfield indices require integer scalars or vectors" Nothing Nothing)

emitBitcastValue :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitBitcastValue target val st fs = do
  let (tyId, st1) = emitTypeFromLayout st target
  let (resId, st2) = freshId st1
  let fs1 = addFuncInstr (Instr opBitcast [tyId, resId, valId val]) fs
  Right (st2, fs1, Value target resId)

coerceExpToI32 :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
coerceExpToI32 baseLayout expVal st fs =
  case baseLayout of
    TLScalar _ _ _ ->
      case valType expVal of
        TLScalar I32 _ _ -> Right (st, fs, expVal)
        TLScalar U32 _ _ -> emitScalarConvert U32 I32 expVal st fs
        _ -> Left (CompileError "ldexp exponent must be i32 or u32 scalar" Nothing Nothing)
    TLVector n _ _ _ ->
      case valType expVal of
        TLScalar s _ _ -> do
          (st1, fs1, scalarVal) <- case s of
            I32 -> Right (st, fs, expVal)
            U32 -> emitScalarConvert U32 I32 expVal st fs
            _ -> Left (CompileError "ldexp exponent must be i32 or u32" Nothing Nothing)
          let (a, sz) = vectorLayout I32 n
          let layout = TLVector n I32 a sz
          emitSplatVector layout (valId scalarVal) st1 fs1
        TLVector m s _ _ | m == n ->
          case s of
            I32 -> Right (st, fs, expVal)
            U32 ->
              let (a, sz) = vectorLayout I32 n
                  layout = TLVector n I32 a sz
              in emitBitcastValue layout expVal st fs
            _ -> Left (CompileError "ldexp exponent must be i32 or u32" Nothing Nothing)
        _ -> Left (CompileError "ldexp exponent must be i32 or u32" Nothing Nothing)
    _ -> Left (CompileError "ldexp expects a float scalar or vector" Nothing Nothing)

emitExtractBitsBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitExtractBitsBuiltin args st fs =
  case args of
    [baseExpr, offsetExpr, countExpr] -> do
      (st1, fs1, baseVal) <- emitExpr st fs baseExpr
      ensureIntNumeric (valType baseVal)
      (st2, fs2, offsetVal) <- emitExpr st1 fs1 offsetExpr
      (st3, fs3, countVal) <- emitExpr st2 fs2 countExpr
      (st4, fs4, offsetVal') <- normalizeBitFieldIndex (valType baseVal) offsetVal st3 fs3
      (st5, fs5, countVal') <- normalizeBitFieldIndex (valType baseVal) countVal st4 fs4
      scalar <- case classifyNumeric (valType baseVal) of
        Just (_, s) -> Right s
        Nothing -> Left (CompileError "extractBits expects integer types" Nothing Nothing)
      opcode <- case scalar of
        I32 -> Right opBitFieldSExtract
        U32 -> Right opBitFieldUExtract
        _ -> Left (CompileError "extractBits expects i32 or u32 types" Nothing Nothing)
      let (tyId, st6) = emitTypeFromLayout st5 (valType baseVal)
      let (resId, st7) = freshId st6
      let fs6 = addFuncInstr (Instr opcode [tyId, resId, valId baseVal, valId offsetVal', valId countVal']) fs5
      Right (st7, fs6, Value (valType baseVal) resId)
    _ -> Left (CompileError "extractBits expects (e, offset, count)" Nothing Nothing)

emitInsertBitsBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitInsertBitsBuiltin args st fs =
  case args of
    [baseExpr, insertExpr, offsetExpr, countExpr] -> do
      (st1, fs1, baseVal) <- emitExpr st fs baseExpr
      (st2, fs2, insertVal) <- emitExpr st1 fs1 insertExpr
      (st3, fs3, offsetVal) <- emitExpr st2 fs2 offsetExpr
      (st4, fs4, countVal) <- emitExpr st3 fs3 countExpr
      (st5, fs5, baseVal', insertVal', layout) <-
        if valType baseVal == valType insertVal
          then Right (st4, fs4, baseVal, insertVal, valType baseVal)
          else coerceBinaryOperands baseVal insertVal st4 fs4
      ensureIntNumeric layout
      (st6, fs6, offsetVal') <- normalizeBitFieldIndex layout offsetVal st5 fs5
      (st7, fs7, countVal') <- normalizeBitFieldIndex layout countVal st6 fs6
      let (tyId, st8) = emitTypeFromLayout st7 layout
      let (resId, st9) = freshId st8
      let fs8 = addFuncInstr (Instr opBitFieldInsert [tyId, resId, valId baseVal', valId insertVal', valId offsetVal', valId countVal']) fs7
      Right (st9, fs8, Value layout resId)
    _ -> Left (CompileError "insertBits expects (base, insert, offset, count)" Nothing Nothing)

emitArrayLengthBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitArrayLengthBuiltin args st fs =
  case args of
    [expr] ->
      case exprToLValue expr of
        Just lv -> emitArrayLengthLValue lv st fs
        Nothing -> Left (CompileError "arrayLength expects an addressable runtime array" Nothing Nothing)
    _ -> Left (CompileError "arrayLength expects one argument" Nothing Nothing)

emitArrayLengthLValue :: LValue -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitArrayLengthLValue lv st fs =
  case lv of
    LVField base field -> do
      (st1, fs1, baseInfo) <- emitLValuePtr st fs base
      when (viStorage baseInfo /= storageClassStorageBuffer) $
        Left (CompileError "arrayLength requires a storage buffer struct" Nothing Nothing)
      case viType baseInfo of
        TLStruct _ fields _ _ -> do
          (ix, fieldLayout) <- findField (textToString field) fields
          when (ix /= length fields - 1) $
            Left (CompileError "runtime arrays must be the last struct member" Nothing Nothing)
          case fieldLayout of
            TLArray Nothing _ _ _ _ -> do
              let (a, sz) = scalarLayout U32
              let layout = TLScalar U32 a sz
              let (tyId, st2) = emitTypeFromLayout st1 layout
              let (resId, st3) = freshId st2
              let fs2 = addFuncInstr (Instr opArrayLength [tyId, resId, viPtrId baseInfo, fromIntegral ix]) fs1
              Right (st3, fs2, Value layout resId)
            _ -> Left (CompileError "arrayLength expects a runtime array field" Nothing Nothing)
        _ -> Left (CompileError "arrayLength expects a struct field" Nothing Nothing)
    _ -> Left (CompileError "arrayLength expects a struct runtime array field" Nothing Nothing)

emitFunctionCallByName :: Text -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Maybe Value)
emitFunctionCallByName name args st fs = do
  (st1, fs1, vals) <- emitExprList st fs args
  let candidates = [fi | fi <- gsFunctionTable st, fiName fi == name, length (fiParams fi) == length vals]
  let exactMatches = filter (\fi -> and (zipWith (==) (fiParams fi) (map valType vals))) candidates
  let coercibleMatches =
        filter
          (\fi -> and (zipWith (argCoercible st1) vals (fiParams fi)))
          candidates
  case exactMatches of
    [fi] -> emitFunctionCall fi vals st1 fs1
    [] ->
      case coercibleMatches of
        [fi] -> do
          (st2, fs2, vals') <- coerceArgsToLayouts vals (fiParams fi) st1 fs1
          emitFunctionCall fi vals' st2 fs2
        [] -> Left (CompileError ("unsupported call: " <> textToString name) Nothing Nothing)
        _ -> Left (CompileError ("ambiguous overload for " <> textToString name) Nothing Nothing)
    _ -> Left (CompileError ("ambiguous overload for " <> textToString name) Nothing Nothing)

argCoercible :: GenState -> Value -> TypeLayout -> Bool
argCoercible st actual expected =
  case ensureTypeMatch expected (valType actual) of
    Right _ -> True
    Left _ ->
      case (valType actual, expected) of
        (TLScalar _ _ _, TLScalar _ _ _) -> isConstLiteral st actual
        _ -> False

emitFunctionCall :: FunctionInfo -> [Value] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Maybe Value)
emitFunctionCall fnInfo vals st fs = do
  let expected = length (fiParams fnInfo)
  if length vals /= expected
    then Left (CompileError "function arity mismatch" Nothing Nothing)
    else pure ()
  zipWithM_ (\val ty -> ensureTypeMatch (valType val) ty) vals (fiParams fnInfo)
  let (retTyId, st1) = case fiReturn fnInfo of
        Nothing -> emitVoidType st
        Just layout -> emitTypeFromLayout st layout
  let (resId, st2) = freshId st1
  let argIds = map valId vals
  let instr = Instr opFunctionCall (retTyId : resId : fiId fnInfo : argIds)
  let fs1 = addFuncInstr instr fs
  case fiReturn fnInfo of
    Nothing -> Right (st2, fs1, Nothing)
    Just layout -> Right (st2, fs1, Just (Value layout resId))

emitVectorCtor :: Int -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVectorCtor n args st fs =
  if length args /= n
    then Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
    else do
      (st1, fs1, vals) <- emitExprList st fs args
      case vals of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        _ -> do
          let baseLayout = pickBaseLayout st1 vals
          (st2, fs2, vals') <- coerceValuesToLayout baseLayout vals st1 fs1
          scalar <- case baseLayout of
            TLScalar s _ _ -> Right s
            _ -> Left (CompileError "vector constructor arguments must be scalars" Nothing Nothing)
          let (align, size) = vectorLayout scalar n
          let layout = TLVector n scalar align size
          let (tyId, st3) = emitTypeFromLayout st2 layout
          let (resId, st4) = freshId st3
          let fs3 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map valId vals')) fs2
          Right (st4, fs3, Value layout resId)

emitMatrixCtor :: Int -> Int -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitMatrixCtor cols rows args st fs =
  if null args
    then Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
    else do
      (st1, fs1, vals) <- emitExprList st fs args
      case vals of
        [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        _ -> do
          let baseLayout = pickBaseLayout st1 vals
          (st2, fs2, vals') <- coerceValuesToLayout baseLayout vals st1 fs1
          let scalarCount = cols * rows
          case baseLayout of
            TLScalar scalar _ _ | length vals' == scalarCount -> do
              let (a, sz) = vectorLayout scalar rows
              let vecLayout = TLVector rows scalar a sz
              (st3, fs3, colsVals) <- buildColumns vecLayout vals' st2 fs2
              let layout = matrixLayout cols rows scalar
              let (tyId, st4) = emitTypeFromLayout st3 layout
              let (resId, st5) = freshId st4
              let fs4 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map valId colsVals)) fs3
              Right (st5, fs4, Value layout resId)
            TLVector n scalar _ _ | n == rows && length vals' == cols -> do
              let layout = matrixLayout cols rows scalar
              let (tyId, st3) = emitTypeFromLayout st2 layout
              let (resId, st4) = freshId st3
              let fs3 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map valId vals')) fs2
              Right (st4, fs3, Value layout resId)
            _ -> Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
  where
    buildColumns vecLayout vals st' fs' =
      case splitAt rows vals of
        ([], _) -> Right (st', fs', [])
        (col, rest) -> do
          let (tyId, st1) = emitTypeFromLayout st' vecLayout
          let (resId, st2) = freshId st1
          let fs1 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map valId col)) fs'
          (st3, fs2, colsVals) <- buildColumns vecLayout rest st2 fs1
          Right (st3, fs2, Value vecLayout resId : colsVals)

emitStructCtor :: Text -> TypeLayout -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitStructCtor name layout args st fs =
  case layout of
    TLStruct _ fields _ _ -> do
      when (length args /= length fields) $
        Left (CompileError ("struct constructor arity mismatch for " <> textToString name) Nothing Nothing)
      (st1, fs1, vals) <- emitExprList st fs args
      (st2, fs2, vals') <- coerceArgsToLayouts vals (map flType fields) st1 fs1
      let (tyId, st3) = emitTypeFromLayout st2 layout
      let (resId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map valId vals')) fs2
      Right (st4, fs3, Value layout resId)
    _ -> Left (CompileError ("unsupported constructor: " <> textToString name) Nothing Nothing)

emitArrayCtor :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitArrayCtor args st fs =
  case args of
    [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
    _ -> do
      (st1, fs1, vals) <- emitExprList st fs args
      case vals of
        [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
        _ -> do
          let firstLayout = pickBaseLayout st1 vals
          (st2, fs2, vals') <- coerceValuesToLayout firstLayout vals st1 fs1
          when (containsResource firstLayout) $
            Left (CompileError "arrays of resources are not supported" Nothing Nothing)
          when (containsAtomic firstLayout) $
            Left (CompileError "arrays of atomic types are not supported" Nothing Nothing)
          let elemAlign = layoutAlign firstLayout
          let elemSize = layoutSize firstLayout
          let stride = roundUp elemSize elemAlign
          let total = stride * fromIntegral (length vals')
          let layout = TLArray (Just (length vals)) stride firstLayout elemAlign total
          let (tyId, st3) = emitTypeFromLayout st2 layout
          let (resId, st4) = freshId st3
          let fs3 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map valId vals')) fs2
          Right (st4, fs3, Value layout resId)

emitScalarCtor :: Scalar -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitScalarCtor scalar args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case valType val of
        TLScalar s _ _ | s == scalar -> Right (st1, fs1, val)
        TLScalar s _ _ -> emitScalarConvert s scalar val st1 fs1
        _ -> Left (CompileError "scalar cast requires a scalar argument" Nothing Nothing)
    _ -> Left (CompileError "scalar cast requires a single argument" Nothing Nothing)

emitScalarConvert :: Scalar -> Scalar -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitScalarConvert from to val st fs = do
  opcode <- case (from, to) of
    (U32, F32) -> Right opConvertUToF
    (I32, F32) -> Right opConvertSToF
    (U32, F16) -> Right opConvertUToF
    (I32, F16) -> Right opConvertSToF
    (F32, U32) -> Right opConvertFToU
    (F32, I32) -> Right opConvertFToS
    (F16, U32) -> Right opConvertFToU
    (F16, I32) -> Right opConvertFToS
    (F16, F32) -> Right opFConvert
    (F32, F16) -> Right opFConvert
    (U32, I32) -> Right opBitcast
    (I32, U32) -> Right opBitcast
    _ -> Left (CompileError "unsupported scalar conversion" Nothing Nothing)
  let (a, sz) = scalarLayout to
  let layout = TLScalar to a sz
  let (tyId, st1) = emitTypeFromLayout st layout
  let (resId, st2) = freshId st1
  let fs1 = addFuncInstr (Instr opcode [tyId, resId, valId val]) fs
  Right (st2, fs1, Value layout resId)

isConstLiteral :: GenState -> Value -> Bool
isConstLiteral st val = isJust (lookupConstKeyById st (valId val))

pickBaseLayout :: GenState -> [Value] -> TypeLayout
pickBaseLayout st vals =
  case dropWhile (isConstLiteral st) vals of
    (v:_) -> valType v
    [] ->
      case vals of
        (v:_) -> valType v
        [] ->
          let (a, sz) = scalarLayout F32
          in TLScalar F32 a sz

coerceValuesToLayout :: TypeLayout -> [Value] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, [Value])
coerceValuesToLayout target vals st fs = go st fs [] vals
  where
    go st' fs' acc [] = Right (st', fs', reverse acc)
    go st' fs' acc (v:vs) = do
      (st1, fs1, v') <- coerceValueToLayout target v st' fs'
      go st1 fs1 (v':acc) vs

coerceArgsToLayouts :: [Value] -> [TypeLayout] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, [Value])
coerceArgsToLayouts vals tys st fs = go st fs [] vals tys
  where
    go st' fs' acc [] [] = Right (st', fs', reverse acc)
    go st' fs' acc (v:vs) (t:ts) = do
      (st1, fs1, v') <- coerceValueToLayout t v st' fs'
      go st1 fs1 (v':acc) vs ts
    go _ _ _ _ _ = Left (CompileError "function arity mismatch" Nothing Nothing)

coerceValueToLayout :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
coerceValueToLayout target val st fs
  | valType val == target = Right (st, fs, val)
  | otherwise =
      case (target, valType val) of
        (TLPointer _ _ _, TLPointer _ _ _) ->
          case ensureTypeMatch target (valType val) of
            Right _ -> Right (st, fs, val)
            Left _ -> Left (CompileError "type mismatch" Nothing Nothing)
        (TLScalar targetScalar _ _, TLScalar _ _ _) ->
          case lookupConstKeyById st (valId val) of
            Nothing -> Left (CompileError "implicit conversion requires a constant literal" Nothing Nothing)
            Just key -> do
              key' <- convertConstKey key targetScalar
              let (cid, st1) = emitConstFromKey st key'
              let (a, sz) = scalarLayout targetScalar
              let layout = TLScalar targetScalar a sz
              Right (st1, fs, Value layout cid)
        _ -> Left (CompileError "type mismatch" Nothing Nothing)

coerceBinaryOperands :: Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value, Value, TypeLayout)
coerceBinaryOperands lval rval st fs
  | valType lval == valType rval = Right (st, fs, lval, rval, valType lval)
  | otherwise =
      case (valType lval, valType rval) of
        (TLScalar _ _ _, TLScalar _ _ _) ->
          let lConst = lookupConstKeyById st (valId lval)
              rConst = lookupConstKeyById st (valId rval)
          in case (lConst, rConst) of
            (_, Just _) -> do
              (st1, fs1, rval') <- coerceValueToLayout (valType lval) rval st fs
              Right (st1, fs1, lval, rval', valType lval)
            (Just _, _) -> do
              (st1, fs1, lval') <- coerceValueToLayout (valType rval) lval st fs
              Right (st1, fs1, lval', rval, valType rval)
            _ -> Left (CompileError "type mismatch" Nothing Nothing)
        _ -> Left (CompileError "type mismatch" Nothing Nothing)

coerceConstValueToLayout :: TypeLayout -> Value -> GenState -> Either CompileError (GenState, Value)
coerceConstValueToLayout target val st
  | valType val == target = Right (st, val)
  | otherwise =
      case (target, valType val) of
        (TLScalar targetScalar _ _, TLScalar _ _ _) ->
          case lookupConstKeyById st (valId val) of
            Nothing -> Left (CompileError "implicit conversion requires a constant literal" Nothing Nothing)
            Just key -> do
              key' <- convertConstKey key targetScalar
              let (cid, st1) = emitConstFromKey st key'
              let (a, sz) = scalarLayout targetScalar
              let layout = TLScalar targetScalar a sz
              Right (st1, Value layout cid)
        _ -> Left (CompileError "type mismatch" Nothing Nothing)

coerceConstValuesToLayout :: TypeLayout -> [Value] -> GenState -> Either CompileError (GenState, [Value])
coerceConstValuesToLayout target vals st = go st [] vals
  where
    go st' acc [] = Right (st', reverse acc)
    go st' acc (v:vs) = do
      (st1, v') <- coerceConstValueToLayout target v st'
      go st1 (v':acc) vs

coerceConstArgsToLayouts :: [Value] -> [TypeLayout] -> GenState -> Either CompileError (GenState, [Value])
coerceConstArgsToLayouts vals tys st = go st [] vals tys
  where
    go st' acc [] [] = Right (st', reverse acc)
    go st' acc (v:vs) (t:ts) = do
      (st1, v') <- coerceConstValueToLayout t v st'
      go st1 (v':acc) vs ts
    go _ _ _ _ = Left (CompileError "constructor arity mismatch" Nothing Nothing)

emitVectorComponent :: Value -> Word32 -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVectorComponent vecVal idx st fs =
  case valType vecVal of
    TLVector n scalar _ _ -> do
      when (idx >= fromIntegral n) $
        Left (CompileError "vector component out of range" Nothing Nothing)
      let (a, sz) = scalarLayout scalar
      let layout = TLScalar scalar a sz
      let (tyId, st1) = emitTypeFromLayout st layout
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opCompositeExtract [tyId, resId, valId vecVal, idx]) fs
      Right (st2, fs1, Value layout resId)
    _ -> Left (CompileError "expected vector value" Nothing Nothing)

emitVec3FromVec2Scalar :: Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVec3FromVec2Scalar vecVal scalarVal st fs =
  case valType vecVal of
    TLVector 2 scalar _ _ -> do
      (st1, fs1, xVal) <- emitVectorComponent vecVal 0 st fs
      (st2, fs2, yVal) <- emitVectorComponent vecVal 1 st1 fs1
      (st3, fs3, scalarVal') <- case valType scalarVal of
        TLScalar s _ _ | s == scalar -> Right (st2, fs2, scalarVal)
        TLScalar s _ _ -> emitScalarConvert s scalar scalarVal st2 fs2
        _ -> Left (CompileError "expected scalar value" Nothing Nothing)
      let (a, sz) = vectorLayout scalar 3
      let layout = TLVector 3 scalar a sz
      let (tyId, st4) = emitTypeFromLayout st3 layout
      let (resId, st5) = freshId st4
      let fs4 = addFuncInstr (Instr opCompositeConstruct [tyId, resId, valId xVal, valId yVal, valId scalarVal']) fs3
      Right (st5, fs4, Value layout resId)
    _ -> Left (CompileError "expected vec2 value" Nothing Nothing)

emitVec2FromScalarScalar :: Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVec2FromScalarScalar scalarVal arrayVal st fs =
  case valType scalarVal of
    TLScalar scalar _ _ -> do
      (st1, fs1, arrayVal') <- case valType arrayVal of
        TLScalar s _ _ | s == scalar -> Right (st, fs, arrayVal)
        TLScalar s _ _ -> emitScalarConvert s scalar arrayVal st fs
        _ -> Left (CompileError "expected scalar value" Nothing Nothing)
      let (a, sz) = vectorLayout scalar 2
      let layout = TLVector 2 scalar a sz
      let (tyId, st2) = emitTypeFromLayout st1 layout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opCompositeConstruct [tyId, resId, valId scalarVal, valId arrayVal']) fs1
      Right (st3, fs2, Value layout resId)
    _ -> Left (CompileError "expected scalar value" Nothing Nothing)

emitVec4FromVec3Scalar :: Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVec4FromVec3Scalar vecVal scalarVal st fs =
  case valType vecVal of
    TLVector 3 scalar _ _ -> do
      (st1, fs1, xVal) <- emitVectorComponent vecVal 0 st fs
      (st2, fs2, yVal) <- emitVectorComponent vecVal 1 st1 fs1
      (st3, fs3, zVal) <- emitVectorComponent vecVal 2 st2 fs2
      (st4, fs4, scalarVal') <- case valType scalarVal of
        TLScalar s _ _ | s == scalar -> Right (st3, fs3, scalarVal)
        TLScalar s _ _ -> emitScalarConvert s scalar scalarVal st3 fs3
        _ -> Left (CompileError "expected scalar value" Nothing Nothing)
      let (a, sz) = vectorLayout scalar 4
      let layout = TLVector 4 scalar a sz
      let (tyId, st5) = emitTypeFromLayout st4 layout
      let (resId, st6) = freshId st5
      let fs5 = addFuncInstr (Instr opCompositeConstruct [tyId, resId, valId xVal, valId yVal, valId zVal, valId scalarVal']) fs4
      Right (st6, fs5, Value layout resId)
    _ -> Left (CompileError "expected vec3 value" Nothing Nothing)

emitTextureSample :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSample args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr] ->
      emitSample texExpr samplerExpr coordExpr Nothing st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr] ->
      emitSample texExpr samplerExpr coordExpr (Just arrayExpr) st fs
    _ -> Left (CompileError "textureSample expects (texture, sampler, coords[, array_index])" Nothing Nothing)
  where
    emitSample texExpr samplerExpr coordExpr mArray st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitExpr st1 fs1 samplerExpr
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      case valType sampVal of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureSample expects a sampler binding" Nothing Nothing)
      case (valType texVal, mArray) of
        (TLTexture1D s, Nothing) -> do
          ensureFloatScalar (valType coordVal)
          sampleColorCoord s coordVal texVal sampVal st3 fs3
        (TLTexture1DArray s, Just arrayExpr) ->
          sampleColor1DArray s coordVal arrayExpr texVal sampVal st3 fs3
        (TLTexture2D s, Nothing) -> sampleColor s coordVal texVal sampVal st3 fs3
        (TLTexture2DArray s, Just arrayExpr) -> sampleColorArray s coordVal arrayExpr texVal sampVal st3 fs3
        (TLTexture3D s, Nothing) -> sampleColor3D s coordVal texVal sampVal st3 fs3
        (TLTextureCube s, Nothing) -> sampleColor3D s coordVal texVal sampVal st3 fs3
        (TLTextureCubeArray s, Just arrayExpr) ->
          sampleColorCubeArray s coordVal arrayExpr texVal sampVal st3 fs3
        (TLTextureDepth2D, Nothing) -> sampleDepth2D coordVal texVal sampVal st3 fs3
        (TLTextureDepth2DArray, Just arrayExpr) -> sampleDepthArray2D coordVal arrayExpr texVal sampVal st3 fs3
        (TLTextureDepthCube, Nothing) -> sampleDepthCube coordVal texVal sampVal st3 fs3
        (TLTextureDepthCubeArray, Just arrayExpr) -> sampleDepthCubeArray coordVal arrayExpr texVal sampVal st3 fs3
        (TLTexture1DArray _, Nothing) ->
          Left (CompileError "textureSample for texture_1d_array requires an array index" Nothing Nothing)
        (TLTexture2DArray _, Nothing) ->
          Left (CompileError "textureSample for texture_2d_array requires an array index" Nothing Nothing)
        (TLTextureCubeArray _, Nothing) ->
          Left (CompileError "textureSample for texture_cube_array requires an array index" Nothing Nothing)
        (TLTextureDepth2DArray, Nothing) ->
          Left (CompileError "textureSample for texture_depth_2d_array requires an array index" Nothing Nothing)
        (TLTextureDepthCubeArray, Nothing) ->
          Left (CompileError "textureSample for texture_depth_cube_array requires an array index" Nothing Nothing)
        (TLTextureMultisampled2D _, _) ->
          Left (CompileError "textureSample is not supported for multisampled textures" Nothing Nothing)
        (TLTextureDepthMultisampled2D, _) ->
          Left (CompileError "textureSample is not supported for multisampled textures" Nothing Nothing)
        _ -> Left (CompileError "textureSample expects a sampled texture binding" Nothing Nothing)

    buildSampledImage texVal sampVal st0 fs0 = do
      let (imageTy, st1) = emitTypeFromLayout st0 (valType texVal)
      let (sampledTy, st2) = emitSampledImageType imageTy st1
      let (sampledId, st3) = freshId st2
      let fs1 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, valId texVal, valId sampVal]) fs0
      Right (st3, fs1, sampledId)

    sampleColorCoord scalar coordVal texVal sampVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleImplicitLod [outTy, resId, sampledId, valId coordVal]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleColor3D scalar coordVal texVal sampVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      sampleColorCoord scalar coordVal texVal sampVal st0 fs0

    sampleColor scalar coordVal texVal sampVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      sampleColorCoord scalar coordVal texVal sampVal st0 fs0

    sampleColor1DArray scalar coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatScalar (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal st1 fs1
      sampleColorCoord scalar coordVal' texVal sampVal st2 fs2

    sampleColorCubeArray scalar coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleColorCoord scalar coordVal' texVal sampVal st2 fs2

    sampleColorArray scalar coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleColorCoord scalar coordVal' texVal sampVal st2 fs2

    sampleDepthCoord coordVal texVal sampVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleImplicitLod [outTy, resId, sampledId, valId coordVal]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleDepth2D coordVal texVal sampVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      sampleDepthCoord coordVal texVal sampVal st0 fs0

    sampleDepthCube coordVal texVal sampVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      sampleDepthCoord coordVal texVal sampVal st0 fs0

    sampleDepthArray2D coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleDepthCoord coordVal' texVal sampVal st2 fs2

    sampleDepthCubeArray coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleDepthCoord coordVal' texVal sampVal st2 fs2

emitTextureSampleCompare :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleCompare args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, depthExpr] ->
      emitCompareSample texExpr samplerExpr coordExpr Nothing depthExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, depthExpr] ->
      emitCompareSample texExpr samplerExpr coordExpr (Just arrayExpr) depthExpr st fs
    _ -> Left (CompileError "textureSampleCompare expects (texture, sampler_comparison, coords, depth_ref) or (texture, sampler_comparison, coords, array_index, depth_ref)" Nothing Nothing)
  where
    emitCompareSample texExpr samplerExpr coordExpr mArray depthExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitExpr st1 fs1 samplerExpr
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, depthVal) <- emitExpr st3 fs3 depthExpr
      case valType sampVal of
        TLSamplerComparison -> Right ()
        _ -> Left (CompileError "textureSampleCompare expects a sampler_comparison binding" Nothing Nothing)
      ensureFloatScalar (valType depthVal)
      (st5, fs5, coordVal') <-
        case (valType texVal, mArray) of
          (TLTextureDepth2D, Nothing) -> do
            ensureFloatVec2 (valType coordVal)
            Right (st4, fs4, coordVal)
          (TLTextureDepth2DArray, Just arrayExpr) -> do
            ensureFloatVec2 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st4 fs4 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepthCube, Nothing) -> do
            ensureFloatVec3 (valType coordVal)
            Right (st4, fs4, coordVal)
          (TLTextureDepthCubeArray, Just arrayExpr) -> do
            ensureFloatVec3 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st4 fs4 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepth2DArray, Nothing) ->
            Left (CompileError "textureSampleCompare for texture_depth_2d_array requires an array index" Nothing Nothing)
          (TLTextureDepthCubeArray, Nothing) ->
            Left (CompileError "textureSampleCompare for texture_depth_cube_array requires an array index" Nothing Nothing)
          _ -> Left (CompileError "textureSampleCompare expects a depth texture binding" Nothing Nothing)
      (st6, fs6, depthVal') <-
        case valType depthVal of
          TLScalar F32 _ _ -> Right (st5, fs5, depthVal)
          TLScalar F16 _ _ -> emitScalarConvert F16 F32 depthVal st5 fs5
          _ -> Left (CompileError "depth reference must be f32 or f16" Nothing Nothing)
      let (imageTy, st7) = emitTypeFromLayout st6 (valType texVal)
      let (sampledTy, st8) = emitSampledImageType imageTy st7
      let (sampledId, st9) = freshId st8
      let fs7 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, valId texVal, valId sampVal]) fs6
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st10) = emitTypeFromLayout st9 outLayout
      let (resId, st11) = freshId st10
      let fs8 = addFuncInstr (Instr opImageSampleDrefImplicitLod [outTy, resId, sampledId, valId coordVal', valId depthVal']) fs7
      Right (st11, fs8, Value outLayout resId)

emitTextureSampleLevel :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleLevel args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, levelExpr] ->
      emitSample texExpr samplerExpr coordExpr Nothing levelExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, levelExpr] ->
      emitSample texExpr samplerExpr coordExpr (Just arrayExpr) levelExpr st fs
    _ -> Left (CompileError "textureSampleLevel expects (texture, sampler, coords[, array_index], level)" Nothing Nothing)
  where
    emitSample texExpr samplerExpr coordExpr mArray levelExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitExpr st1 fs1 samplerExpr
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, levelVal) <- emitExpr st3 fs3 levelExpr
      case valType sampVal of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureSampleLevel expects a sampler binding" Nothing Nothing)
      (st5, fs5, lodVal) <- case valType levelVal of
        TLScalar F32 _ _ -> Right (st4, fs4, levelVal)
        TLScalar F16 _ _ -> emitScalarConvert F16 F32 levelVal st4 fs4
        _ -> Left (CompileError "textureSampleLevel requires a float level" Nothing Nothing)
      case (valType texVal, mArray) of
        (TLTexture1D s, Nothing) -> do
          ensureFloatScalar (valType coordVal)
          sampleColorCoordLod s coordVal texVal sampVal lodVal st5 fs5
        (TLTexture1DArray s, Just arrayExpr) ->
          sampleColor1DArray s coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTexture2D s, Nothing) -> sampleColor2D s coordVal texVal sampVal lodVal st5 fs5
        (TLTexture2DArray s, Just arrayExpr) -> sampleColorArray s coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTexture3D s, Nothing) -> sampleColor3D s coordVal texVal sampVal lodVal st5 fs5
        (TLTextureCube s, Nothing) -> sampleColor3D s coordVal texVal sampVal lodVal st5 fs5
        (TLTextureCubeArray s, Just arrayExpr) ->
          sampleColorCubeArray s coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTextureDepth2D, Nothing) -> sampleDepth2D coordVal texVal sampVal lodVal st5 fs5
        (TLTextureDepth2DArray, Just arrayExpr) -> sampleDepthArray2D coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTextureDepthCube, Nothing) -> sampleDepthCube coordVal texVal sampVal lodVal st5 fs5
        (TLTextureDepthCubeArray, Just arrayExpr) -> sampleDepthCubeArray coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTexture1DArray _, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_1d_array requires an array index" Nothing Nothing)
        (TLTexture2DArray _, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_2d_array requires an array index" Nothing Nothing)
        (TLTextureCubeArray _, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_cube_array requires an array index" Nothing Nothing)
        (TLTextureDepth2DArray, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_depth_2d_array requires an array index" Nothing Nothing)
        (TLTextureDepthCubeArray, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_depth_cube_array requires an array index" Nothing Nothing)
        (TLTextureMultisampled2D _, _) ->
          Left (CompileError "textureSampleLevel is not supported for multisampled textures" Nothing Nothing)
        (TLTextureDepthMultisampled2D, _) ->
          Left (CompileError "textureSampleLevel is not supported for multisampled textures" Nothing Nothing)
        _ -> Left (CompileError "textureSampleLevel expects a sampled texture binding" Nothing Nothing)

    buildSampledImage texVal sampVal st0 fs0 = do
      let (imageTy, st1) = emitTypeFromLayout st0 (valType texVal)
      let (sampledTy, st2) = emitSampledImageType imageTy st1
      let (sampledId, st3) = freshId st2
      let fs1 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, valId texVal, valId sampVal]) fs0
      Right (st3, fs1, sampledId)

    sampleColorCoordLod scalar coordVal texVal sampVal lodVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleExplicitLod [outTy, resId, sampledId, valId coordVal, imageOperandsLod, valId lodVal]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleColor2D scalar coordVal texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      sampleColorCoordLod scalar coordVal texVal sampVal lodVal st0 fs0

    sampleColor3D scalar coordVal texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      sampleColorCoordLod scalar coordVal texVal sampVal lodVal st0 fs0

    sampleColor1DArray scalar coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatScalar (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal st1 fs1
      sampleColorCoordLod scalar coordVal' texVal sampVal lodVal st2 fs2

    sampleColorCubeArray scalar coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleColorCoordLod scalar coordVal' texVal sampVal lodVal st2 fs2

    sampleColorArray scalar coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleColorCoordLod scalar coordVal' texVal sampVal lodVal st2 fs2

    sampleDepthCoordLod coordVal texVal sampVal lodVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleExplicitLod [outTy, resId, sampledId, valId coordVal, imageOperandsLod, valId lodVal]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleDepth2D coordVal texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      sampleDepthCoordLod coordVal texVal sampVal lodVal st0 fs0

    sampleDepthCube coordVal texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      sampleDepthCoordLod coordVal texVal sampVal lodVal st0 fs0

    sampleDepthArray2D coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordLod coordVal' texVal sampVal lodVal st2 fs2

    sampleDepthCubeArray coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordLod coordVal' texVal sampVal lodVal st2 fs2

emitTextureSampleBias :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleBias args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, biasExpr] ->
      emitSample texExpr samplerExpr coordExpr Nothing biasExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, biasExpr] ->
      emitSample texExpr samplerExpr coordExpr (Just arrayExpr) biasExpr st fs
    _ -> Left (CompileError "textureSampleBias expects (texture, sampler, coords[, array_index], bias)" Nothing Nothing)
  where
    emitSample texExpr samplerExpr coordExpr mArray biasExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitExpr st1 fs1 samplerExpr
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, biasVal) <- emitExpr st3 fs3 biasExpr
      case valType sampVal of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureSampleBias expects a sampler binding" Nothing Nothing)
      (st5, fs5, biasVal') <- case valType biasVal of
        TLScalar F32 _ _ -> Right (st4, fs4, biasVal)
        TLScalar F16 _ _ -> emitScalarConvert F16 F32 biasVal st4 fs4
        _ -> Left (CompileError "textureSampleBias requires a float bias" Nothing Nothing)
      case (valType texVal, mArray) of
        (TLTexture1D s, Nothing) -> do
          ensureFloatScalar (valType coordVal)
          sampleColorCoordBias s coordVal texVal sampVal biasVal' st5 fs5
        (TLTexture1DArray s, Just arrayExpr) ->
          sampleColor1DArray s coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTexture2D s, Nothing) -> sampleColor2D s coordVal texVal sampVal biasVal' st5 fs5
        (TLTexture2DArray s, Just arrayExpr) -> sampleColorArray s coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTexture3D s, Nothing) -> sampleColor3D s coordVal texVal sampVal biasVal' st5 fs5
        (TLTextureCube s, Nothing) -> sampleColor3D s coordVal texVal sampVal biasVal' st5 fs5
        (TLTextureCubeArray s, Just arrayExpr) ->
          sampleColorCubeArray s coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTextureDepth2D, Nothing) -> sampleDepth2D coordVal texVal sampVal biasVal' st5 fs5
        (TLTextureDepth2DArray, Just arrayExpr) -> sampleDepthArray2D coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTextureDepthCube, Nothing) -> sampleDepthCube coordVal texVal sampVal biasVal' st5 fs5
        (TLTextureDepthCubeArray, Just arrayExpr) -> sampleDepthCubeArray coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTexture1DArray _, Nothing) ->
          Left (CompileError "textureSampleBias for texture_1d_array requires an array index" Nothing Nothing)
        (TLTexture2DArray _, Nothing) ->
          Left (CompileError "textureSampleBias for texture_2d_array requires an array index" Nothing Nothing)
        (TLTextureCubeArray _, Nothing) ->
          Left (CompileError "textureSampleBias for texture_cube_array requires an array index" Nothing Nothing)
        (TLTextureDepth2DArray, Nothing) ->
          Left (CompileError "textureSampleBias for texture_depth_2d_array requires an array index" Nothing Nothing)
        (TLTextureDepthCubeArray, Nothing) ->
          Left (CompileError "textureSampleBias for texture_depth_cube_array requires an array index" Nothing Nothing)
        (TLTextureMultisampled2D _, _) ->
          Left (CompileError "textureSampleBias is not supported for multisampled textures" Nothing Nothing)
        (TLTextureDepthMultisampled2D, _) ->
          Left (CompileError "textureSampleBias is not supported for multisampled textures" Nothing Nothing)
        _ -> Left (CompileError "textureSampleBias expects a sampled texture binding" Nothing Nothing)

    buildSampledImage texVal sampVal st0 fs0 = do
      let (imageTy, st1) = emitTypeFromLayout st0 (valType texVal)
      let (sampledTy, st2) = emitSampledImageType imageTy st1
      let (sampledId, st3) = freshId st2
      let fs1 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, valId texVal, valId sampVal]) fs0
      Right (st3, fs1, sampledId)

    sampleColorCoordBias scalar coordVal texVal sampVal biasVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleImplicitLod [outTy, resId, sampledId, valId coordVal, imageOperandsBias, valId biasVal]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleColor2D scalar coordVal texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      sampleColorCoordBias scalar coordVal texVal sampVal biasVal st0 fs0

    sampleColor3D scalar coordVal texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      sampleColorCoordBias scalar coordVal texVal sampVal biasVal st0 fs0

    sampleColor1DArray scalar coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatScalar (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal st1 fs1
      sampleColorCoordBias scalar coordVal' texVal sampVal biasVal st2 fs2

    sampleColorCubeArray scalar coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleColorCoordBias scalar coordVal' texVal sampVal biasVal st2 fs2

    sampleColorArray scalar coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleColorCoordBias scalar coordVal' texVal sampVal biasVal st2 fs2

    sampleDepthCoordBias coordVal texVal sampVal biasVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleImplicitLod [outTy, resId, sampledId, valId coordVal, imageOperandsBias, valId biasVal]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleDepth2D coordVal texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      sampleDepthCoordBias coordVal texVal sampVal biasVal st0 fs0

    sampleDepthCube coordVal texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      sampleDepthCoordBias coordVal texVal sampVal biasVal st0 fs0

    sampleDepthArray2D coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordBias coordVal' texVal sampVal biasVal st2 fs2

    sampleDepthCubeArray coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordBias coordVal' texVal sampVal biasVal st2 fs2

emitTextureSampleGrad :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleGrad args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, ddxExpr, ddyExpr] ->
      emitSample texExpr samplerExpr coordExpr Nothing ddxExpr ddyExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, ddxExpr, ddyExpr] ->
      emitSample texExpr samplerExpr coordExpr (Just arrayExpr) ddxExpr ddyExpr st fs
    _ -> Left (CompileError "textureSampleGrad expects (texture, sampler, coords[, array_index], ddx, ddy)" Nothing Nothing)
  where
    emitSample texExpr samplerExpr coordExpr mArray ddxExpr ddyExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitExpr st1 fs1 samplerExpr
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, ddxVal) <- emitExpr st3 fs3 ddxExpr
      (st5, fs5, ddyVal) <- emitExpr st4 fs4 ddyExpr
      case valType sampVal of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureSampleGrad expects a sampler binding" Nothing Nothing)
      ensureFloatNumeric (valType coordVal)
      ensureTypeMatch (valType coordVal) (valType ddxVal)
      ensureTypeMatch (valType coordVal) (valType ddyVal)
      case (valType texVal, mArray) of
        (TLTexture1D s, Nothing) ->
          sampleColorCoordGrad s coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture1DArray s, Just arrayExpr) ->
          sampleColor1DArray s coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture2D s, Nothing) -> sampleColor2D s coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture2DArray s, Just arrayExpr) -> sampleColorArray s coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture3D s, Nothing) -> sampleColor3D s coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureCube s, Nothing) -> sampleColor3D s coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureCubeArray s, Just arrayExpr) ->
          sampleColorCubeArray s coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureDepth2D, Nothing) -> sampleDepth2D coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureDepth2DArray, Just arrayExpr) -> sampleDepthArray2D coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureDepthCube, Nothing) -> sampleDepthCube coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureDepthCubeArray, Just arrayExpr) -> sampleDepthCubeArray coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture1DArray _, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_1d_array requires an array index" Nothing Nothing)
        (TLTexture2DArray _, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_2d_array requires an array index" Nothing Nothing)
        (TLTextureCubeArray _, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_cube_array requires an array index" Nothing Nothing)
        (TLTextureDepth2DArray, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_depth_2d_array requires an array index" Nothing Nothing)
        (TLTextureDepthCubeArray, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_depth_cube_array requires an array index" Nothing Nothing)
        (TLTextureMultisampled2D _, _) ->
          Left (CompileError "textureSampleGrad is not supported for multisampled textures" Nothing Nothing)
        (TLTextureDepthMultisampled2D, _) ->
          Left (CompileError "textureSampleGrad is not supported for multisampled textures" Nothing Nothing)
        _ -> Left (CompileError "textureSampleGrad expects a sampled texture binding" Nothing Nothing)

    buildSampledImage texVal sampVal st0 fs0 = do
      let (imageTy, st1) = emitTypeFromLayout st0 (valType texVal)
      let (sampledTy, st2) = emitSampledImageType imageTy st1
      let (sampledId, st3) = freshId st2
      let fs1 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, valId texVal, valId sampVal]) fs0
      Right (st3, fs1, sampledId)

    sampleColorCoordGrad scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleExplicitLod [outTy, resId, sampledId, valId coordVal, imageOperandsGrad, valId ddxVal, valId ddyVal]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleColor2D scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      sampleColorCoordGrad scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0

    sampleColor3D scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      sampleColorCoordGrad scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0

    sampleColor1DArray scalar coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatScalar (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal st1 fs1
      sampleColorCoordGrad scalar coordVal' texVal sampVal ddxVal ddyVal st2 fs2

    sampleColorCubeArray scalar coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleColorCoordGrad scalar coordVal' texVal sampVal ddxVal ddyVal st2 fs2

    sampleColorArray scalar coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleColorCoordGrad scalar coordVal' texVal sampVal ddxVal ddyVal st2 fs2

    sampleDepthCoordGrad coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleExplicitLod [outTy, resId, sampledId, valId coordVal, imageOperandsGrad, valId ddxVal, valId ddyVal]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleDepth2D coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      sampleDepthCoordGrad coordVal texVal sampVal ddxVal ddyVal st0 fs0

    sampleDepthCube coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      sampleDepthCoordGrad coordVal texVal sampVal ddxVal ddyVal st0 fs0

    sampleDepthArray2D coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec2 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordGrad coordVal' texVal sampVal ddxVal ddyVal st2 fs2

    sampleDepthCubeArray coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec3 (valType coordVal)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (valType arrayVal)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordGrad coordVal' texVal sampVal ddxVal ddyVal st2 fs2

emitTextureSampleCompareLevel :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleCompareLevel args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, depthExpr, levelExpr] ->
      emitCompareSample texExpr samplerExpr coordExpr Nothing depthExpr levelExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, depthExpr, levelExpr] ->
      emitCompareSample texExpr samplerExpr coordExpr (Just arrayExpr) depthExpr levelExpr st fs
    _ -> Left (CompileError "textureSampleCompareLevel expects (texture, sampler_comparison, coords[, array_index], depth_ref, level)" Nothing Nothing)
  where
    emitCompareSample texExpr samplerExpr coordExpr mArray depthExpr levelExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitExpr st1 fs1 samplerExpr
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, depthVal) <- emitExpr st3 fs3 depthExpr
      (st5, fs5, levelVal) <- emitExpr st4 fs4 levelExpr
      case valType sampVal of
        TLSamplerComparison -> Right ()
        _ -> Left (CompileError "textureSampleCompareLevel expects a sampler_comparison binding" Nothing Nothing)
      ensureFloatScalar (valType depthVal)
      (st6, fs6, lodVal) <- case valType levelVal of
        TLScalar F32 _ _ -> Right (st5, fs5, levelVal)
        TLScalar F16 _ _ -> emitScalarConvert F16 F32 levelVal st5 fs5
        _ -> Left (CompileError "textureSampleCompareLevel requires a float level" Nothing Nothing)
      (st7, fs7, coordVal') <-
        case (valType texVal, mArray) of
          (TLTextureDepth2D, Nothing) -> do
            ensureFloatVec2 (valType coordVal)
            Right (st6, fs6, coordVal)
          (TLTextureDepth2DArray, Just arrayExpr) -> do
            ensureFloatVec2 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st6 fs6 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepthCube, Nothing) -> do
            ensureFloatVec3 (valType coordVal)
            Right (st6, fs6, coordVal)
          (TLTextureDepthCubeArray, Just arrayExpr) -> do
            ensureFloatVec3 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st6 fs6 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepth2DArray, Nothing) ->
            Left (CompileError "textureSampleCompareLevel for texture_depth_2d_array requires an array index" Nothing Nothing)
          (TLTextureDepthCubeArray, Nothing) ->
            Left (CompileError "textureSampleCompareLevel for texture_depth_cube_array requires an array index" Nothing Nothing)
          _ -> Left (CompileError "textureSampleCompareLevel expects a depth texture binding" Nothing Nothing)
      (st8, fs8, depthVal') <-
        case valType depthVal of
          TLScalar F32 _ _ -> Right (st7, fs7, depthVal)
          TLScalar F16 _ _ -> emitScalarConvert F16 F32 depthVal st7 fs7
          _ -> Left (CompileError "depth reference must be f32 or f16" Nothing Nothing)
      let (imageTy, st9) = emitTypeFromLayout st8 (valType texVal)
      let (sampledTy, st10) = emitSampledImageType imageTy st9
      let (sampledId, st11) = freshId st10
      let fs9 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, valId texVal, valId sampVal]) fs8
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st12) = emitTypeFromLayout st11 outLayout
      let (resId, st13) = freshId st12
      let fs10 = addFuncInstr (Instr opImageSampleDrefExplicitLod [outTy, resId, sampledId, valId coordVal', valId depthVal', imageOperandsLod, valId lodVal]) fs9
      Right (st13, fs10, Value outLayout resId)

emitTextureGather :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureGather args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr] ->
      emitGather texExpr samplerExpr coordExpr Nothing Nothing st fs
    [texExpr, samplerExpr, coordExpr, arg4] ->
      emitGather texExpr samplerExpr coordExpr (Just arg4) Nothing st fs
    [texExpr, samplerExpr, coordExpr, arg4, arg5] ->
      emitGather texExpr samplerExpr coordExpr (Just arg4) (Just arg5) st fs
    _ -> Left (CompileError "textureGather expects (texture, sampler, coords[, array_index|component][, component])" Nothing Nothing)
  where
    emitGather texExpr samplerExpr coordExpr mArrayOrComp mComp st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitExpr st1 fs1 samplerExpr
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      case valType sampVal of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureGather expects a sampler binding" Nothing Nothing)
      let isArray = case valType texVal of
            TLTexture2DArray _ -> True
            TLTextureCubeArray _ -> True
            _ -> False
      (arrayExpr', compExpr') <-
        case (isArray, mArrayOrComp, mComp) of
          (False, Nothing, Nothing) -> Right (Nothing, Nothing)
          (False, Just compExpr, Nothing) -> Right (Nothing, Just compExpr)
          (False, Nothing, Just _) ->
            Left (CompileError "textureGather component must be provided as the fourth argument" Nothing Nothing)
          (False, Just _, Just _) ->
            Left (CompileError "textureGather only accepts one component argument" Nothing Nothing)
          (True, Just arrayExpr, Nothing) -> Right (Just arrayExpr, Nothing)
          (True, Just arrayExpr, Just compExpr) -> Right (Just arrayExpr, Just compExpr)
          (True, Nothing, _) ->
            Left (CompileError "textureGather for array textures requires an array index" Nothing Nothing)
      (st4, fs4, coordValFinal, scalar) <-
        case (valType texVal, arrayExpr') of
          (TLTexture2D s, Nothing) -> do
            ensureFloatVec2 (valType coordVal)
            Right (st3, fs3, coordVal, s)
          (TLTexture2DArray s, Just arrayExpr) -> do
            ensureFloatVec2 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st3 fs3 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal', s)
          (TLTextureCube s, Nothing) -> do
            ensureFloatVec3 (valType coordVal)
            Right (st3, fs3, coordVal, s)
          (TLTextureCubeArray s, Just arrayExpr) -> do
            ensureFloatVec3 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st3 fs3 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal', s)
          _ -> Left (CompileError "textureGather expects a 2d or cube sampled texture" Nothing Nothing)
      (st6, fs6, compVal) <- case compExpr' of
        Nothing ->
          let (cid, st') = emitConstU32 st4 0
              layout = TLScalar U32 (fst (scalarLayout U32)) (snd (scalarLayout U32))
          in Right (st', fs4, Value layout cid)
        Just compExpr -> do
          (stA, fsA, compVal0) <- emitExpr st4 fs4 compExpr
          ensureIndexType (valType compVal0)
          case valType compVal0 of
            TLScalar U32 _ _ -> Right (stA, fsA, compVal0)
            TLScalar I32 _ _ -> emitScalarConvert I32 U32 compVal0 stA fsA
            _ -> Left (CompileError "textureGather component must be i32 or u32" Nothing Nothing)
      let (imageTy, st7) = emitTypeFromLayout st6 (valType texVal)
      let (sampledTy, st8) = emitSampledImageType imageTy st7
      let (sampledId, st9) = freshId st8
      let fs7 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, valId texVal, valId sampVal]) fs6
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st10) = emitTypeFromLayout st9 outLayout
      let (resId, st11) = freshId st10
      let fs8 = addFuncInstr (Instr opImageGather [outTy, resId, sampledId, valId coordValFinal, valId compVal]) fs7
      Right (st11, fs8, Value outLayout resId)

emitTextureGatherCompare :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureGatherCompare args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, depthExpr] ->
      emitGather texExpr samplerExpr coordExpr Nothing depthExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, depthExpr] ->
      emitGather texExpr samplerExpr coordExpr (Just arrayExpr) depthExpr st fs
    _ -> Left (CompileError "textureGatherCompare expects (texture, sampler_comparison, coords[, array_index], depth_ref)" Nothing Nothing)
  where
    emitGather texExpr samplerExpr coordExpr mArray depthExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitExpr st1 fs1 samplerExpr
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, depthVal) <- emitExpr st3 fs3 depthExpr
      case valType sampVal of
        TLSamplerComparison -> Right ()
        _ -> Left (CompileError "textureGatherCompare expects a sampler_comparison binding" Nothing Nothing)
      ensureFloatScalar (valType depthVal)
      (st5, fs5, coordVal') <-
        case (valType texVal, mArray) of
          (TLTextureDepth2D, Nothing) -> do
            ensureFloatVec2 (valType coordVal)
            Right (st4, fs4, coordVal)
          (TLTextureDepth2DArray, Just arrayExpr) -> do
            ensureFloatVec2 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st4 fs4 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepthCube, Nothing) -> do
            ensureFloatVec3 (valType coordVal)
            Right (st4, fs4, coordVal)
          (TLTextureDepthCubeArray, Just arrayExpr) -> do
            ensureFloatVec3 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st4 fs4 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepth2DArray, Nothing) ->
            Left (CompileError "textureGatherCompare for texture_depth_2d_array requires an array index" Nothing Nothing)
          (TLTextureDepthCubeArray, Nothing) ->
            Left (CompileError "textureGatherCompare for texture_depth_cube_array requires an array index" Nothing Nothing)
          _ -> Left (CompileError "textureGatherCompare expects a depth texture binding" Nothing Nothing)
      (st6, fs6, depthVal') <-
        case valType depthVal of
          TLScalar F32 _ _ -> Right (st5, fs5, depthVal)
          TLScalar F16 _ _ -> emitScalarConvert F16 F32 depthVal st5 fs5
          _ -> Left (CompileError "depth reference must be f32 or f16" Nothing Nothing)
      let (imageTy, st7) = emitTypeFromLayout st6 (valType texVal)
      let (sampledTy, st8) = emitSampledImageType imageTy st7
      let (sampledId, st9) = freshId st8
      let fs7 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, valId texVal, valId sampVal]) fs6
      let (align, size) = vectorLayout F32 4
      let outLayout = TLVector 4 F32 align size
      let (outTy, st10) = emitTypeFromLayout st9 outLayout
      let (resId, st11) = freshId st10
      let fs8 = addFuncInstr (Instr opImageDrefGather [outTy, resId, sampledId, valId coordVal', valId depthVal']) fs7
      Right (st11, fs8, Value outLayout resId)

emitTextureDimensions :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureDimensions args st fs =
  case args of
    [texExpr] -> emitDim texExpr Nothing st fs
    [texExpr, levelExpr] -> emitDim texExpr (Just levelExpr) st fs
    _ -> Left (CompileError "textureDimensions expects (texture[, level])" Nothing Nothing)
  where
    emitDim texExpr mLevel st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, mLevelVal) <- case mLevel of
        Nothing -> Right (st1, fs1, Nothing)
        Just levelExpr -> do
          (stA, fsA, levelVal) <- emitExpr st1 fs1 levelExpr
          ensureIndexType (valType levelVal)
          Right (stA, fsA, Just levelVal)
      let texLayout = valType texVal
      let isStorage = case texLayout of
            TLStorageTexture1D _ _ -> True
            TLStorageTexture2D _ _ -> True
            TLStorageTexture2DArray _ _ -> True
            TLStorageTexture3D _ _ -> True
            _ -> False
      let isMultisampled = case texLayout of
            TLTextureMultisampled2D _ -> True
            TLTextureDepthMultisampled2D -> True
            _ -> False
      when ((isStorage || isMultisampled) && isJust mLevelVal) $
        Left (CompileError "textureDimensions level is only valid for sampled textures" Nothing Nothing)
      let queryLayout = querySizeLayout texLayout
      let (queryTy, st3) = emitTypeFromLayout st2 queryLayout
      let (resId, st4) = freshId st3
      let st4' = addCapability capabilityImageQuery st4
      (st5, fs5, lodVal) <-
        if isStorage || isMultisampled
          then Right (st4', fs2, Nothing)
          else do
            case mLevelVal of
              Just levelVal -> Right (st4', fs2, Just levelVal)
              Nothing -> do
                let (cid, st') = emitConstU32 st4' 0
                let (a, sz) = scalarLayout U32
                let layout = TLScalar U32 a sz
                Right (st', fs2, Just (Value layout cid))
      let fs6 = case lodVal of
            Nothing -> addFuncInstr (Instr opImageQuerySize [queryTy, resId, valId texVal]) fs5
            Just lod ->
              addFuncInstr (Instr opImageQuerySizeLod [queryTy, resId, valId texVal, valId lod]) fs5
      let queryVal = Value queryLayout resId
      adjustQueryValue texLayout queryVal st5 fs6

    querySizeLayout texLayout =
      let (a, sz) = scalarLayout U32
          vec n = let (va, vsz) = vectorLayout U32 n in TLVector n U32 va vsz
      in case texLayout of
          TLTexture1D _ -> TLScalar U32 a sz
          TLTexture1DArray _ -> vec 2
          TLTexture2D _ -> vec 2
          TLTexture2DArray _ -> vec 3
          TLTexture3D _ -> vec 3
          TLTextureCube _ -> vec 2
          TLTextureCubeArray _ -> vec 3
          TLTextureMultisampled2D _ -> vec 2
          TLTextureDepth2D -> vec 2
          TLTextureDepth2DArray -> vec 3
          TLTextureDepthCube -> vec 2
          TLTextureDepthCubeArray -> vec 3
          TLTextureDepthMultisampled2D -> vec 2
          TLStorageTexture1D _ _ -> TLScalar U32 a sz
          TLStorageTexture2D _ _ -> vec 2
          TLStorageTexture2DArray _ _ -> vec 3
          TLStorageTexture3D _ _ -> vec 3
          _ -> TLVector 2 U32 a (snd (vectorLayout U32 2))

    adjustQueryValue texLayout queryVal st0 fs0 =
      case texLayout of
        TLTexture1DArray _ -> emitVectorComponent queryVal 0 st0 fs0
        TLTexture2DArray _ -> dropToVec2 queryVal st0 fs0
        TLTextureCubeArray _ -> dropToVec2 queryVal st0 fs0
        TLTextureDepth2DArray -> dropToVec2 queryVal st0 fs0
        TLTextureDepthCubeArray -> dropToVec2 queryVal st0 fs0
        TLStorageTexture2DArray _ _ -> dropToVec2 queryVal st0 fs0
        _ -> Right (st0, fs0, queryVal)

    dropToVec2 vecVal st0 fs0 = do
      (st1, fs1, xVal) <- emitVectorComponent vecVal 0 st0 fs0
      (st2, fs2, yVal) <- emitVectorComponent vecVal 1 st1 fs1
      (st3, fs3, vec2Val) <- emitVec2FromScalarScalar xVal yVal st2 fs2
      Right (st3, fs3, vec2Val)

emitTextureNumLevels :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureNumLevels args st fs =
  case args of
    [texExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      case valType texVal of
        TLTextureMultisampled2D _ -> Left (CompileError "textureNumLevels is not valid for multisampled textures" Nothing Nothing)
        TLTextureDepthMultisampled2D -> Left (CompileError "textureNumLevels is not valid for multisampled textures" Nothing Nothing)
        TLStorageTexture1D _ _ -> Left (CompileError "textureNumLevels is not valid for storage textures" Nothing Nothing)
        TLStorageTexture2D _ _ -> Left (CompileError "textureNumLevels is not valid for storage textures" Nothing Nothing)
        TLStorageTexture2DArray _ _ -> Left (CompileError "textureNumLevels is not valid for storage textures" Nothing Nothing)
        TLStorageTexture3D _ _ -> Left (CompileError "textureNumLevels is not valid for storage textures" Nothing Nothing)
        _ -> do
          let (a, sz) = scalarLayout U32
          let outLayout = TLScalar U32 a sz
          let (outTy, st2) = emitTypeFromLayout st1 outLayout
          let (resId, st3) = freshId st2
          let st3' = addCapability capabilityImageQuery st3
          let fs2 = addFuncInstr (Instr opImageQueryLevels [outTy, resId, valId texVal]) fs1
          Right (st3', fs2, Value outLayout resId)
    _ -> Left (CompileError "textureNumLevels expects (texture)" Nothing Nothing)

emitTextureNumLayers :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureNumLayers args st fs =
  case args of
    [texExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      let texLayout = valType texVal
      let isArray = case texLayout of
            TLTexture1DArray _ -> True
            TLTexture2DArray _ -> True
            TLTextureCubeArray _ -> True
            TLTextureDepth2DArray -> True
            TLTextureDepthCubeArray -> True
            TLStorageTexture2DArray _ _ -> True
            _ -> False
      when (not isArray) $
        Left (CompileError "textureNumLayers expects an array texture" Nothing Nothing)
      let queryLayout = case texLayout of
            TLTexture1DArray _ -> let (va, vsz) = vectorLayout U32 2 in TLVector 2 U32 va vsz
            _ -> let (va, vsz) = vectorLayout U32 3 in TLVector 3 U32 va vsz
      let (outTy, st2) = emitTypeFromLayout st1 queryLayout
      let (resId, st3) = freshId st2
      let st3' = addCapability capabilityImageQuery st3
      let isStorage = case texLayout of
            TLStorageTexture2DArray _ _ -> True
            _ -> False
      (st4, fs2) <-
        if isStorage
          then Right (st3', addFuncInstr (Instr opImageQuerySize [outTy, resId, valId texVal]) fs1)
          else do
            let (cid, st') = emitConstU32 st3' 0
                (a, sz) = scalarLayout U32
                lodVal = Value (TLScalar U32 a sz) cid
            Right (st', addFuncInstr (Instr opImageQuerySizeLod [outTy, resId, valId texVal, valId lodVal]) fs1)
      let queryVal = Value queryLayout resId
      let layerIx = case texLayout of
            TLTexture1DArray _ -> 1
            _ -> 2
      emitVectorComponent queryVal layerIx st4 fs2
    _ -> Left (CompileError "textureNumLayers expects (texture)" Nothing Nothing)

emitTextureNumSamples :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureNumSamples args st fs =
  case args of
    [texExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      case valType texVal of
        TLTextureMultisampled2D _ -> querySamples texVal st1 fs1
        TLTextureDepthMultisampled2D -> querySamples texVal st1 fs1
        _ -> Left (CompileError "textureNumSamples expects a multisampled texture" Nothing Nothing)
    _ -> Left (CompileError "textureNumSamples expects (texture)" Nothing Nothing)
  where
    querySamples texVal st0 fs0 = do
      let (a, sz) = scalarLayout U32
      let outLayout = TLScalar U32 a sz
      let (outTy, st1) = emitTypeFromLayout st0 outLayout
      let (resId, st2) = freshId st1
      let st2' = addCapability capabilityImageQuery st2
      let fs1 = addFuncInstr (Instr opImageQuerySamples [outTy, resId, valId texVal]) fs0
      Right (st2', fs1, Value outLayout resId)

emitTextureLoad :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureLoad args st fs =
  case args of
    [texExpr, coordExpr] ->
      emitStorageLoad texExpr coordExpr st fs
    [texExpr, coordExpr, thirdExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      case valType texVal of
        TLTextureMultisampled2D _ ->
          emitMultisampledLoad texVal coordExpr thirdExpr st1 fs1
        TLTextureDepthMultisampled2D ->
          emitMultisampledLoad texVal coordExpr thirdExpr st1 fs1
        _ ->
          emitSampledLoad texVal coordExpr Nothing thirdExpr st1 fs1
    [texExpr, coordExpr, arrayExpr, levelExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      emitSampledLoad texVal coordExpr (Just arrayExpr) levelExpr st1 fs1
    _ -> Left (CompileError "textureLoad expects (texture, coords[, array_index], level) or (texture_multisampled_2d, coords, sample_index)" Nothing Nothing)
  where
    emitStorageLoad texExpr coordExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, coordVal) <- emitExpr st1 fs1 coordExpr
      (scalar, access) <- case valType texVal of
        TLStorageTexture1D fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture2D fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture2DArray fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture3D fmt acc -> Right (storageFormatScalar fmt, acc)
        _ -> Left (CompileError "textureLoad expects a storage texture" Nothing Nothing)
      when (access == StorageWrite) $
        Left (CompileError "textureLoad is not allowed on write-only storage textures" Nothing Nothing)
      case valType texVal of
        TLStorageTexture1D _ _ -> ensureIndexType (valType coordVal)
        TLStorageTexture2D _ _ -> ensureIntVec2 (valType coordVal)
        TLStorageTexture2DArray _ _ -> ensureIntVec3 (valType coordVal)
        TLStorageTexture3D _ _ -> ensureIntVec3 (valType coordVal)
        _ -> Left (CompileError "invalid storage texture coordinates" Nothing Nothing)
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st3) = emitTypeFromLayout st2 outLayout
      let (resId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opImageRead [outTy, resId, valId texVal, valId coordVal]) fs2
      Right (st4, fs3, Value outLayout resId)

    emitSampledLoad texVal coordExpr mArray levelExpr st0 fs0 = do
      (st1, fs1, coordVal) <- emitExpr st0 fs0 coordExpr
      (st2, fs2, levelVal) <- emitExpr st1 fs1 levelExpr
      ensureIndexType (valType levelVal)
      (st3, fs3, coordVal') <-
        case (valType texVal, mArray) of
          (TLTexture1D _, Nothing) -> do
            ensureIndexType (valType coordVal)
            Right (st2, fs2, coordVal)
          (TLTexture1DArray _, Just arrayExpr) -> do
            ensureIndexType (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTexture2D _, Nothing) -> do
            ensureIntVec2 (valType coordVal)
            Right (st2, fs2, coordVal)
          (TLTexture3D _, Nothing) -> do
            ensureIntVec3 (valType coordVal)
            Right (st2, fs2, coordVal)
          (TLTextureCube _, Nothing) -> do
            ensureIntVec3 (valType coordVal)
            Right (st2, fs2, coordVal)
          (TLTextureDepth2D, Nothing) -> do
            ensureIntVec2 (valType coordVal)
            Right (st2, fs2, coordVal)
          (TLTextureDepthCube, Nothing) -> do
            ensureIntVec3 (valType coordVal)
            Right (st2, fs2, coordVal)
          (TLTexture2DArray _, Just arrayExpr) -> do
            ensureIntVec2 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepth2DArray, Just arrayExpr) -> do
            ensureIntVec2 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureCubeArray _, Just arrayExpr) -> do
            ensureIntVec3 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepthCubeArray, Just arrayExpr) -> do
            ensureIntVec3 (valType coordVal)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (valType arrayVal)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTexture1DArray _, Nothing) ->
            Left (CompileError "textureLoad for texture_1d_array requires an array index" Nothing Nothing)
          (TLTexture2DArray _, Nothing) ->
            Left (CompileError "textureLoad for texture_2d_array requires an array index" Nothing Nothing)
          (TLTextureCubeArray _, Nothing) ->
            Left (CompileError "textureLoad for texture_cube_array requires an array index" Nothing Nothing)
          (TLTextureDepth2DArray, Nothing) ->
            Left (CompileError "textureLoad for texture_depth_2d_array requires an array index" Nothing Nothing)
          (TLTextureDepthCubeArray, Nothing) ->
            Left (CompileError "textureLoad for texture_depth_cube_array requires an array index" Nothing Nothing)
          _ -> Left (CompileError "textureLoad expects a sampled texture binding" Nothing Nothing)
      let (fetchLayout, resultLayout) =
            case valType texVal of
              TLTextureDepth2D ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, TLScalar F32 (fst (scalarLayout F32)) (snd (scalarLayout F32)))
              TLTextureDepth2DArray ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, TLScalar F32 (fst (scalarLayout F32)) (snd (scalarLayout F32)))
              TLTextureDepthCube ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, TLScalar F32 (fst (scalarLayout F32)) (snd (scalarLayout F32)))
              TLTextureDepthCubeArray ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, TLScalar F32 (fst (scalarLayout F32)) (snd (scalarLayout F32)))
              TLTexture2D s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTexture2DArray s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTexture3D s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTextureCube s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTextureCubeArray s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTexture1D s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTexture1DArray s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              _ -> let (a, sz) = vectorLayout F32 4 in (TLVector 4 F32 a sz, TLVector 4 F32 a sz)
      let (outTy, st4) = emitTypeFromLayout st3 fetchLayout
      let (resId, st5) = freshId st4
      let fs4 = addFuncInstr (Instr opImageFetch [outTy, resId, valId texVal, valId coordVal', imageOperandsLod, valId levelVal]) fs3
      let fetchVal = Value fetchLayout resId
      if fetchLayout == resultLayout
        then Right (st5, fs4, fetchVal)
        else emitVectorComponent fetchVal 0 st5 fs4

    emitMultisampledLoad texVal coordExpr sampleExpr st0 fs0 = do
      (st1, fs1, coordVal) <- emitExpr st0 fs0 coordExpr
      (st2, fs2, sampleVal) <- emitExpr st1 fs1 sampleExpr
      ensureIndexType (valType sampleVal)
      case valType texVal of
        TLTextureMultisampled2D _ -> ensureIntVec2 (valType coordVal)
        TLTextureDepthMultisampled2D -> ensureIntVec2 (valType coordVal)
        _ -> Left (CompileError "textureLoad expects a multisampled texture" Nothing Nothing)
      let (fetchLayout, resultLayout) =
            case valType texVal of
              TLTextureDepthMultisampled2D ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, TLScalar F32 (fst (scalarLayout F32)) (snd (scalarLayout F32)))
              TLTextureMultisampled2D s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              _ -> let (a, sz) = vectorLayout F32 4 in (TLVector 4 F32 a sz, TLVector 4 F32 a sz)
      let (outTy, st3) = emitTypeFromLayout st2 fetchLayout
      let (resId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opImageFetch [outTy, resId, valId texVal, valId coordVal, imageOperandsSample, valId sampleVal]) fs2
      let fetchVal = Value fetchLayout resId
      if fetchLayout == resultLayout
        then Right (st4, fs3, fetchVal)
        else emitVectorComponent fetchVal 0 st4 fs3

emitTextureStore :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitTextureStore args st fs =
  case args of
    [texExpr, coordExpr, valueExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      (st2, fs2, coordVal) <- emitExpr st1 fs1 coordExpr
      (st3, fs3, valueVal) <- emitExpr st2 fs2 valueExpr
      (scalar, access) <- case valType texVal of
        TLStorageTexture1D fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture2D fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture2DArray fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture3D fmt acc -> Right (storageFormatScalar fmt, acc)
        _ -> Left (CompileError "textureStore expects a storage texture" Nothing Nothing)
      when (access == StorageRead) $
        Left (CompileError "textureStore is not allowed on read-only storage textures" Nothing Nothing)
      case valType texVal of
        TLStorageTexture1D _ _ -> ensureIndexType (valType coordVal)
        TLStorageTexture2D _ _ -> ensureIntVec2 (valType coordVal)
        TLStorageTexture2DArray _ _ -> ensureIntVec3 (valType coordVal)
        TLStorageTexture3D _ _ -> ensureIntVec3 (valType coordVal)
        _ -> Left (CompileError "textureStore expects a storage texture" Nothing Nothing)
      let (align, size) = vectorLayout scalar 4
      let expectedLayout = TLVector 4 scalar align size
      ensureTypeMatch expectedLayout (valType valueVal)
      let fs4 = addFuncInstr (Instr opImageWrite [valId texVal, valId coordVal, valId valueVal]) fs3
      Right (st3, fs4)
    _ -> Left (CompileError "textureStore expects (texture, coords, value)" Nothing Nothing)

emitDerivative :: Word16 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDerivative opcode args st fs =
  case args of
    [arg] -> do
      when (gsEntryStage st /= StageFragment) $
        Left (CompileError "derivatives are only available in fragment shaders" Nothing Nothing)
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatNumeric (valType val)
      let (tyId, st2) = emitTypeFromLayout st1 (valType val)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opcode [tyId, resId, valId val]) fs1
      Right (st3, fs2, Value (valType val) resId)
    _ -> Left (CompileError "derivative builtins expect one argument" Nothing Nothing)

emitAtomicPtr :: Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState, VarInfo)
emitAtomicPtr expr st fs =
  case exprToLValue expr of
    Nothing -> Left (CompileError "atomic operations require an addressable expression" Nothing Nothing)
    Just lv -> do
      (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
      case viType ptrInfo of
        TLAtomic _ -> pure ()
        _ -> Left (CompileError "atomic operations require atomic types" Nothing Nothing)
      when (viStorage ptrInfo /= storageClassStorageBuffer) $
        Left (CompileError "atomic operations are only supported on storage buffers" Nothing Nothing)
      when (viAccess ptrInfo == ReadOnly) $
        Left (CompileError "atomic operations require read_write storage buffers" Nothing Nothing)
      Right (st1, fs1, ptrInfo)

emitAtomicLoad :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitAtomicLoad args st fs =
  case args of
    [ptrExpr] -> do
      (st1, fs1, ptrInfo) <- emitAtomicPtr ptrExpr st fs
      scalar <- case viType ptrInfo of
        TLAtomic s -> Right s
        _ -> Left (CompileError "atomicLoad expects an atomic value" Nothing Nothing)
      let (a, sz) = scalarLayout scalar
      let layout = TLScalar scalar a sz
      let (tyId, st2) = emitTypeFromLayout st1 layout
      let (resId, st3) = freshId st2
      let (scopeId, st4) = emitConstU32 st3 memoryScopeDevice
      let (semId, st5) = emitConstU32 st4 memorySemanticsRelaxed
      let fs2 = addFuncInstr (Instr opAtomicLoad [tyId, resId, viPtrId ptrInfo, scopeId, semId]) fs1
      Right (st5, fs2, Value layout resId)
    _ -> Left (CompileError "atomicLoad expects (ptr)" Nothing Nothing)

emitAtomicStore :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitAtomicStore args st fs =
  case args of
    [ptrExpr, valueExpr] -> do
      (st1, fs1, ptrInfo) <- emitAtomicPtr ptrExpr st fs
      scalar <- case viType ptrInfo of
        TLAtomic s -> Right s
        _ -> Left (CompileError "atomicStore expects an atomic value" Nothing Nothing)
      (st2, fs2, val) <- emitExpr st1 fs1 valueExpr
      let (a, sz) = scalarLayout scalar
      (st3, fs3, val') <- coerceValueToLayout (TLScalar scalar a sz) val st2 fs2
      let (scopeId, st4) = emitConstU32 st3 memoryScopeDevice
      let (semId, st5) = emitConstU32 st4 memorySemanticsRelaxed
      let fs4 = addFuncInstr (Instr opAtomicStore [viPtrId ptrInfo, scopeId, semId, valId val']) fs3
      Right (st5, fs4)
    _ -> Left (CompileError "atomicStore expects (ptr, value)" Nothing Nothing)

emitAtomicBinary :: (Scalar -> Either CompileError Word16) -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitAtomicBinary opcodeFor args st fs =
  case args of
    [ptrExpr, valueExpr] -> do
      (st1, fs1, ptrInfo) <- emitAtomicPtr ptrExpr st fs
      scalar <- case viType ptrInfo of
        TLAtomic s -> Right s
        _ -> Left (CompileError "atomic operation expects an atomic value" Nothing Nothing)
      opcode <- opcodeFor scalar
      (st2, fs2, val) <- emitExpr st1 fs1 valueExpr
      let (a, sz) = scalarLayout scalar
      (st3, fs3, val') <- coerceValueToLayout (TLScalar scalar a sz) val st2 fs2
      let layout = TLScalar scalar a sz
      let (tyId, st4) = emitTypeFromLayout st3 layout
      let (resId, st5) = freshId st4
      let (scopeId, st6) = emitConstU32 st5 memoryScopeDevice
      let (semId, st7) = emitConstU32 st6 memorySemanticsRelaxed
      let fs4 = addFuncInstr (Instr opcode [tyId, resId, viPtrId ptrInfo, scopeId, semId, valId val']) fs3
      Right (st7, fs4, Value layout resId)
    _ -> Left (CompileError "atomic operation expects (ptr, value)" Nothing Nothing)

emitExprList :: GenState -> FuncState -> [Expr] -> Either CompileError (GenState, FuncState, [Value])
emitExprList st fs [] = Right (st, fs, [])
emitExprList st fs (x:xs) = do
  (st1, fs1, v) <- emitExpr st fs x
  (st2, fs2, vs) <- emitExprList st1 fs1 xs
  Right (st2, fs2, v:vs)

emitLValuePtr :: GenState -> FuncState -> LValue -> Either CompileError (GenState, FuncState, VarInfo)
emitLValuePtr st fs lv =
  case lv of
    LVVar name ->
      case lookup name (fsVars fs) of
        Just v -> Right (st, fs, v)
        Nothing ->
          case lookup name (fsValues fs) of
            Just _ -> Left (CompileError "cannot take the address of an immutable let binding" Nothing Nothing)
            Nothing -> Left (CompileError ("unknown variable: " <> textToString name) Nothing Nothing)
    LVField base field -> do
      (st1, fs1, baseInfo) <- emitLValuePtr st fs base
      case viType baseInfo of
        TLStruct _ fields _ _ -> do
          (ix, fieldLayout) <- findField (textToString field) fields
          let (ixId, st2) = emitConstU32 st1 (fromIntegral ix)
          emitAccessChain st2 fs1 baseInfo [ixId] fieldLayout
        TLVector n scalar _ _ -> do
          if T.length field /= 1
            then Left (CompileError "cannot assign to vector swizzle" Nothing Nothing)
            else do
              ix <- vectorFieldIndex field n
              let (ixId, st2) = emitConstU32 st1 (fromIntegral ix)
              let (a, sz) = scalarLayout scalar
              let fieldLayout = TLScalar scalar a sz
              emitAccessChain st2 fs1 baseInfo [ixId] fieldLayout
        _ -> Left (CompileError "field access requires struct or vector type" Nothing Nothing)
    LVIndex base idxExpr -> do
      (st1, fs1, baseInfo) <- emitLValuePtr st fs base
      (st2, fs2, idxVal) <- emitExpr st1 fs1 idxExpr
      ensureIndexType (valType idxVal)
      case viType baseInfo of
        TLArray _ _ elemLayout _ _ -> emitAccessChain st2 fs2 baseInfo [valId idxVal] elemLayout
        TLVector _ scalar _ _ ->
          let (a, sz) = scalarLayout scalar
              elemLayout = TLScalar scalar a sz
          in emitAccessChain st2 fs2 baseInfo [valId idxVal] elemLayout
        TLMatrix _ rows scalar _ _ _ ->
          let elemLayout = TLVector rows scalar (fst (vectorLayout scalar rows)) (snd (vectorLayout scalar rows))
          in emitAccessChain st2 fs2 baseInfo [valId idxVal] elemLayout
        _ -> Left (CompileError "indexing requires array or vector type" Nothing Nothing)
    LVDeref expr -> do
      (st1, fs1, val) <- emitExpr st fs expr
      case valType val of
        TLPointer storageClass access elemLayout ->
          Right (st1, fs1, VarInfo elemLayout (valId val) storageClass (ptrAccessToVarAccess access))
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)

emitAccessChain :: GenState -> FuncState -> VarInfo -> [Word32] -> TypeLayout -> Either CompileError (GenState, FuncState, VarInfo)
emitAccessChain st fs baseInfo indices elemLayout = do
  let storageClass = viStorage baseInfo
  let (elemTy, st1) = emitTypeFromLayout st elemLayout
  let (ptrTy, st2) = emitPointerType st1 storageClass elemTy
  let (resId, st3) = freshId st2
  let instr = Instr opAccessChain (ptrTy : resId : viPtrId baseInfo : indices)
  let fs1 = addFuncInstr instr fs
  Right (st3, fs1, VarInfo elemLayout resId storageClass (viAccess baseInfo))

exprToLValue :: Expr -> Maybe LValue
exprToLValue expr =
  case expr of
    EVar name -> Just (LVVar name)
    EField base field -> LVField <$> exprToLValue base <*> pure field
    EIndex base idx -> LVIndex <$> exprToLValue base <*> pure idx
    EUnary OpDeref inner -> Just (LVDeref inner)
    _ -> Nothing

ensureTypeMatch :: TypeLayout -> TypeLayout -> Either CompileError ()
ensureTypeMatch a b =
  case (a, b) of
    (TLPointer scA accA elemA, TLPointer scB accB elemB) ->
      if scA == scB && elemA == elemB && ptrAccessCompatible accA accB
        then Right ()
        else Left (CompileError "type mismatch" Nothing Nothing)
    _ ->
      if a == b
        then Right ()
        else Left (CompileError "type mismatch" Nothing Nothing)

ptrAccessCompatible :: Maybe StorageAccess -> Maybe StorageAccess -> Bool
ptrAccessCompatible expected actual =
  if ptrAccessAllowsWrite expected
    then ptrAccessAllowsWrite actual
    else True

ensureWritable :: VarInfo -> Either CompileError ()
ensureWritable info =
  case viAccess info of
    ReadWrite -> Right ()
    ReadOnly -> Left (CompileError "assignment target is read-only" Nothing Nothing)

ensureBoolScalar :: TypeLayout -> Either CompileError ()
ensureBoolScalar layout =
  case layout of
    TLScalar Bool _ _ -> Right ()
    _ -> Left (CompileError "expected bool scalar" Nothing Nothing)

ensureBoolVectorSize :: Int -> TypeLayout -> Either CompileError ()
ensureBoolVectorSize n layout =
  case layout of
    TLVector m Bool _ _ | m == n -> Right ()
    _ -> Left (CompileError "expected bool vector with matching size" Nothing Nothing)

ensureSwitchType :: TypeLayout -> Either CompileError ()
ensureSwitchType layout =
  case layout of
    TLScalar I32 _ _ -> Right ()
    TLScalar U32 _ _ -> Right ()
    _ -> Left (CompileError "switch selector must be an i32 or u32 scalar" Nothing Nothing)

ensureIntNumeric :: TypeLayout -> Either CompileError ()
ensureIntNumeric layout =
  case layout of
    TLScalar I32 _ _ -> Right ()
    TLScalar U32 _ _ -> Right ()
    TLVector _ I32 _ _ -> Right ()
    TLVector _ U32 _ _ -> Right ()
    _ -> Left (CompileError "expected i32 or u32 scalar or vector" Nothing Nothing)

ensureFloatNumeric :: TypeLayout -> Either CompileError ()
ensureFloatNumeric layout =
  case layout of
    TLScalar F32 _ _ -> Right ()
    TLScalar F16 _ _ -> Right ()
    TLVector _ F32 _ _ -> Right ()
    TLVector _ F16 _ _ -> Right ()
    _ -> Left (CompileError "expected f32 or f16 scalar or vector" Nothing Nothing)

ensureFloatVector :: TypeLayout -> Either CompileError ()
ensureFloatVector layout =
  case layout of
    TLVector _ F32 _ _ -> Right ()
    TLVector _ F16 _ _ -> Right ()
    _ -> Left (CompileError "expected f32 or f16 vector" Nothing Nothing)

ensureIndexType :: TypeLayout -> Either CompileError ()
ensureIndexType layout =
  case layout of
    TLScalar U32 _ _ -> Right ()
    TLScalar I32 _ _ -> Right ()
    _ -> Left (CompileError "index must be u32 or i32" Nothing Nothing)

ensureIntVec2 :: TypeLayout -> Either CompileError ()
ensureIntVec2 layout =
  case layout of
    TLVector 2 U32 _ _ -> Right ()
    TLVector 2 I32 _ _ -> Right ()
    _ -> Left (CompileError "expected vec2<i32> or vec2<u32>" Nothing Nothing)

ensureFloatVec2 :: TypeLayout -> Either CompileError ()
ensureFloatVec2 layout =
  case layout of
    TLVector 2 F32 _ _ -> Right ()
    TLVector 2 F16 _ _ -> Right ()
    _ -> Left (CompileError "expected vec2<f32> or vec2<f16>" Nothing Nothing)

ensureFloatVec3 :: TypeLayout -> Either CompileError ()
ensureFloatVec3 layout =
  case layout of
    TLVector 3 F32 _ _ -> Right ()
    TLVector 3 F16 _ _ -> Right ()
    _ -> Left (CompileError "expected vec3<f32> or vec3<f16>" Nothing Nothing)

ensureFloatVecN :: Int -> TypeLayout -> Either CompileError ()
ensureFloatVecN n layout =
  case layout of
    TLVector m F32 _ _ | m == n -> Right ()
    TLVector m F16 _ _ | m == n -> Right ()
    _ -> Left (CompileError ("expected vec" <> show n <> "<f32> or vec" <> show n <> "<f16>") Nothing Nothing)

ensureIntVec3 :: TypeLayout -> Either CompileError ()
ensureIntVec3 layout =
  case layout of
    TLVector 3 U32 _ _ -> Right ()
    TLVector 3 I32 _ _ -> Right ()
    _ -> Left (CompileError "expected vec3<i32> or vec3<u32>" Nothing Nothing)

ensureFloatMatrix :: TypeLayout -> Either CompileError (Int, Int, Scalar)
ensureFloatMatrix layout =
  case layout of
    TLMatrix cols rows scalar _ _ _ ->
      case scalar of
        F32 -> Right (cols, rows, scalar)
        F16 -> Right (cols, rows, scalar)
        _ -> Left (CompileError "expected float matrix" Nothing Nothing)
    _ -> Left (CompileError "expected matrix type" Nothing Nothing)

ensureFloatScalar :: TypeLayout -> Either CompileError ()
ensureFloatScalar layout =
  case layout of
    TLScalar F32 _ _ -> Right ()
    TLScalar F16 _ _ -> Right ()
    _ -> Left (CompileError "expected f32 or f16 scalar" Nothing Nothing)

findField :: String -> [FieldLayout] -> Either CompileError (Int, TypeLayout)
findField name fields = go 0 fields
  where
    go _ [] = Left (CompileError ("unknown field: " <> name) Nothing Nothing)
    go ix (f:fs)
      | flName f == name = Right (ix, flType f)
      | otherwise = go (ix + 1) fs

vectorFieldIndex :: Text -> Int -> Either CompileError Int
vectorFieldIndex field n =
  case vectorFieldIndices field n of
    Right [ix] -> Right (fromIntegral ix)
    Right _ -> Left (CompileError "expected single-component vector field" Nothing Nothing)
    Left err -> Left err

vectorFieldIndices :: Text -> Int -> Either CompileError [Word32]
vectorFieldIndices field n = do
  indices <- mapM charIndex (T.unpack field)
  mapM_ (checkRange field) indices
  Right indices
  where
    charIndex ch = case ch of
      'x' -> Right 0
      'y' -> Right 1
      'z' -> Right 2
      'w' -> Right 3
      'r' -> Right 0
      'g' -> Right 1
      'b' -> Right 2
      'a' -> Right 3
      's' -> Right 0
      't' -> Right 1
      'p' -> Right 2
      'q' -> Right 3
      _ -> Left (CompileError ("unknown vector field: " <> [ch]) Nothing Nothing)
    checkRange name ix =
      if fromIntegral ix < n
        then Right ()
        else Left (CompileError ("vector field out of range: " <> textToString name) Nothing Nothing)

classifyNumeric :: TypeLayout -> Maybe (Int, Scalar)
classifyNumeric layout =
  case layout of
    TLScalar s _ _ -> Just (1, s)
    TLVector n s _ _ -> Just (n, s)
    TLAtomic _ -> Nothing
    _ -> Nothing

boolResultLayout :: Int -> TypeLayout
boolResultLayout n =
  if n <= 1
    then
      let (a, sz) = scalarLayout Bool
      in TLScalar Bool a sz
    else
      let (a, sz) = vectorLayout Bool n
      in TLVector n Bool a sz

buildSpirvWords :: CompileOptions -> EntryPoint -> GenState -> [Word32]
buildSpirvWords opts entry st =
  let header =
        [ 0x07230203
        , spirvVersion opts
        , 0
        , gsNextId st
        , 0
        ]
      entryPointInstr = case gsEntryPoint st of
        Nothing -> []
        Just epId ->
          let nameWords = encodeString (textToString (epName entry))
              model = case epStage entry of
                StageCompute -> executionModelGLCompute
                StageFragment -> executionModelFragment
                StageVertex -> executionModelVertex
              operands = model : epId : nameWords <> gsInterfaceIds st
          in encodeInstr (Instr opEntryPoint operands)
      execModeInstr = case gsEntryPoint st of
        Nothing -> []
        Just epId ->
          case epStage entry of
            StageCompute ->
              case epWorkgroupSize entry of
                Nothing -> []
                Just (x, y, z) ->
                  let ops = [epId, executionModeLocalSize, x, y, z]
                  in encodeInstr (Instr opExecutionMode ops)
            StageFragment ->
              encodeInstr (Instr opExecutionMode [epId, executionModeOriginUpperLeft])
            StageVertex -> []
      capInstrs =
        concatMap (\cap -> encodeInstr (Instr opCapability [cap])) (capabilityShader : gsCapabilities st)
      body =
        concat
          [ capInstrs
          , concatMap encodeInstr (gsExtInstImports st)
          , encodeInstr (Instr opMemoryModel [addressingLogical, memoryModelGLSL450])
          , entryPointInstr
          , execModeInstr
          , concatMap encodeInstr (gsNames st)
          , concatMap encodeInstr (gsDecorations st)
          , concatMap encodeInstr (gsTypes st)
          , concatMap encodeInstr (gsConstants st)
          , concatMap encodeInstr (gsGlobals st)
          , concatMap encodeInstr (gsFunctions st)
          ]
  in header <> body

emitVoidType :: GenState -> (Word32, GenState)
emitVoidType st = emitTypeCached st TKVoid (Instr opTypeVoid [])

emitBoolType :: GenState -> (Word32, GenState)
emitBoolType st = emitTypeCached st TKBool (Instr opTypeBool [])

emitIntType :: Bool -> GenState -> (Word32, GenState)
emitIntType signed st =
  let key = TKInt signed 32
      instr = Instr opTypeInt [32, if signed then 1 else 0]
  in emitTypeCached st key instr

emitFloatTypeWidth :: Word32 -> GenState -> (Word32, GenState)
emitFloatTypeWidth bits st =
  let st1 =
        if bits == 16
          then addCapability capabilityFloat16 st
          else st
  in emitTypeCached st1 (TKFloat bits) (Instr opTypeFloat [bits])

emitFloatType :: GenState -> (Word32, GenState)
emitFloatType st = emitFloatTypeWidth 32 st

emitVecType :: Word32 -> Word32 -> GenState -> (Word32, GenState)
emitVecType elemId n st =
  emitTypeCached st (TKVector elemId n) (Instr opTypeVector [elemId, n])

emitSamplerType :: GenState -> (Word32, GenState)
emitSamplerType st =
  emitTypeCached st TKSampler (Instr opTypeSampler [])

emitImageType :: Scalar -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> TypeKey -> GenState -> (Word32, GenState)
emitImageType scalar dim depth arrayed ms sampled fmt key st =
  let (a, sz) = scalarLayout scalar
      (sampledId, st1) = emitTypeFromLayout st (TLScalar scalar a sz)
      instr = Instr opTypeImage [sampledId, dim, depth, arrayed, ms, sampled, fmt]
  in emitTypeCached st1 key instr

emitImage1DType :: Scalar -> GenState -> (Word32, GenState)
emitImage1DType scalar st =
  emitImageType scalar dim1D 0 0 0 imageSampled imageFormatUnknown (TKImage1D scalar) (addCapability capabilitySampled1D st)

emitImage1DArrayType :: Scalar -> GenState -> (Word32, GenState)
emitImage1DArrayType scalar st =
  emitImageType scalar dim1D 0 1 0 imageSampled imageFormatUnknown (TKImage1DArray scalar) (addCapability capabilitySampled1D st)

emitImage2DType :: Scalar -> GenState -> (Word32, GenState)
emitImage2DType scalar st =
  emitImageType scalar dim2D 0 0 0 imageSampled imageFormatUnknown (TKImage2D scalar) st

emitImage2DArrayType :: Scalar -> GenState -> (Word32, GenState)
emitImage2DArrayType scalar st =
  emitImageType scalar dim2D 0 1 0 imageSampled imageFormatUnknown (TKImage2DArray scalar) st

emitImage3DType :: Scalar -> GenState -> (Word32, GenState)
emitImage3DType scalar st =
  emitImageType scalar dim3D 0 0 0 imageSampled imageFormatUnknown (TKImage3D scalar) st

emitImageCubeType :: Scalar -> GenState -> (Word32, GenState)
emitImageCubeType scalar st =
  emitImageType scalar dimCube 0 0 0 imageSampled imageFormatUnknown (TKImageCube scalar) st

emitImageCubeArrayType :: Scalar -> GenState -> (Word32, GenState)
emitImageCubeArrayType scalar st =
  emitImageType scalar dimCube 0 1 0 imageSampled imageFormatUnknown (TKImageCubeArray scalar) st

emitImage2DMultisampledType :: Scalar -> GenState -> (Word32, GenState)
emitImage2DMultisampledType scalar st =
  emitImageType scalar dim2D 0 0 1 imageSampled imageFormatUnknown (TKImage2DMultisampled scalar) st

emitImageDepth2DType :: GenState -> (Word32, GenState)
emitImageDepth2DType st =
  emitImageType F32 dim2D 1 0 0 imageSampled imageFormatUnknown TKImageDepth2D st

emitImageDepth2DArrayType :: GenState -> (Word32, GenState)
emitImageDepth2DArrayType st =
  emitImageType F32 dim2D 1 1 0 imageSampled imageFormatUnknown TKImageDepth2DArray st

emitImageDepthCubeType :: GenState -> (Word32, GenState)
emitImageDepthCubeType st =
  emitImageType F32 dimCube 1 0 0 imageSampled imageFormatUnknown TKImageDepthCube st

emitImageDepthCubeArrayType :: GenState -> (Word32, GenState)
emitImageDepthCubeArrayType st =
  emitImageType F32 dimCube 1 1 0 imageSampled imageFormatUnknown TKImageDepthCubeArray st

emitImageDepth2DMultisampledType :: GenState -> (Word32, GenState)
emitImageDepth2DMultisampledType st =
  emitImageType F32 dim2D 1 0 1 imageSampled imageFormatUnknown TKImageDepth2DMultisampled st

emitStorageImage1DType :: StorageFormat -> GenState -> (Word32, GenState)
emitStorageImage1DType fmt st =
  let scalar = storageFormatScalar fmt
      fmtId = storageFormatToImageFormat fmt
  in emitImageType scalar dim1D 0 0 0 imageStorage fmtId (TKStorageImage1D fmt) (addCapability capabilityImage1D st)

emitStorageImage2DType :: StorageFormat -> GenState -> (Word32, GenState)
emitStorageImage2DType fmt st =
  let scalar = storageFormatScalar fmt
      fmtId = storageFormatToImageFormat fmt
  in emitImageType scalar dim2D 0 0 0 imageStorage fmtId (TKStorageImage2D fmt) st

emitStorageImage2DArrayType :: StorageFormat -> GenState -> (Word32, GenState)
emitStorageImage2DArrayType fmt st =
  let scalar = storageFormatScalar fmt
      fmtId = storageFormatToImageFormat fmt
  in emitImageType scalar dim2D 0 1 0 imageStorage fmtId (TKStorageImage2DArray fmt) st

emitStorageImage3DType :: StorageFormat -> GenState -> (Word32, GenState)
emitStorageImage3DType fmt st =
  let scalar = storageFormatScalar fmt
      fmtId = storageFormatToImageFormat fmt
  in emitImageType scalar dim3D 0 0 0 imageStorage fmtId (TKStorageImage3D fmt) st

emitSampledImageType :: Word32 -> GenState -> (Word32, GenState)
emitSampledImageType imageId st =
  emitTypeCached st (TKSampledImage imageId) (Instr opTypeSampledImage [imageId])

emitArrayType :: Word32 -> Word32 -> Word32 -> GenState -> (Word32, GenState)
emitArrayType elemId lenVal stride st =
  let key = TKArray elemId (Just lenVal) stride
  in case lookup key (gsTypeCache st) of
       Just tid -> (tid, st)
       Nothing ->
         let (lenId, st1) = emitTypeConstU32 st lenVal
             instr = Instr opTypeArray [elemId, lenId]
             (tyId, st2) = emitTypeCached st1 key instr
             st3 = addDecoration (Instr opDecorate [tyId, decorationArrayStride, stride]) st2
         in (tyId, st3)

emitRuntimeArrayType :: Word32 -> Word32 -> GenState -> (Word32, GenState)
emitRuntimeArrayType elemId stride st =
  let key = TKArray elemId Nothing stride
      instr = Instr opTypeRuntimeArray [elemId]
  in case lookup key (gsTypeCache st) of
       Just tid -> (tid, st)
       Nothing ->
         let (tyId, st1) = emitTypeCached st key instr
             st2 = addDecoration (Instr opDecorate [tyId, decorationArrayStride, stride]) st1
         in (tyId, st2)

emitPointerType :: GenState -> Word32 -> Word32 -> (Word32, GenState)
emitPointerType st storageClass baseId =
  emitTypeCached st (TKPointer storageClass baseId) (Instr opTypePointer [storageClass, baseId])

emitFunctionType :: GenState -> Word32 -> [Word32] -> (Word32, GenState)
emitFunctionType st retTy paramTys =
  emitTypeCached st (TKFunction retTy paramTys) (Instr opTypeFunction (retTy : paramTys))

emitTypeFromLayout :: GenState -> TypeLayout -> (Word32, GenState)
emitTypeFromLayout st layout =
  case layout of
    TLScalar s _ _ ->
      case s of
        Bool -> emitBoolType st
        I32 -> emitIntType True st
        U32 -> emitIntType False st
        F32 -> emitFloatType st
        F16 -> emitFloatTypeWidth 16 st
    TLVector n s _ _ ->
      let (a, sz) = scalarLayout s
          (elemId, st1) = emitTypeFromLayout st (TLScalar s a sz)
      in emitVecType elemId (fromIntegral n) st1
    TLMatrix cols rows s _ _ _ ->
      let (a, sz) = scalarLayout s
          (elemId, st1) = emitTypeFromLayout st (TLScalar s a sz)
          (vecId, st2) = emitVecType elemId (fromIntegral rows) st1
          key = TKMatrix (fromIntegral cols) (fromIntegral rows) s
          instr = Instr opTypeMatrix [vecId, fromIntegral cols]
      in emitTypeCached st2 key instr
    TLSampler -> emitSamplerType st
    TLSamplerComparison -> emitSamplerType st
    TLTexture1D s -> emitImage1DType s st
    TLTexture1DArray s -> emitImage1DArrayType s st
    TLTexture2D s -> emitImage2DType s st
    TLTexture2DArray s -> emitImage2DArrayType s st
    TLTexture3D s -> emitImage3DType s st
    TLTextureCube s -> emitImageCubeType s st
    TLTextureCubeArray s -> emitImageCubeArrayType s st
    TLTextureMultisampled2D s -> emitImage2DMultisampledType s st
    TLTextureDepth2D -> emitImageDepth2DType st
    TLTextureDepth2DArray -> emitImageDepth2DArrayType st
    TLTextureDepthCube -> emitImageDepthCubeType st
    TLTextureDepthCubeArray -> emitImageDepthCubeArrayType st
    TLTextureDepthMultisampled2D -> emitImageDepth2DMultisampledType st
    TLStorageTexture1D fmt _ -> emitStorageImage1DType fmt st
    TLStorageTexture2D fmt _ -> emitStorageImage2DType fmt st
    TLStorageTexture2DArray fmt _ -> emitStorageImage2DArrayType fmt st
    TLStorageTexture3D fmt _ -> emitStorageImage3DType fmt st
    TLAtomic s ->
      case s of
        I32 -> emitIntType True st
        U32 -> emitIntType False st
        _ -> emitIntType True st
    TLPointer storageClass _ elemLayout ->
      let (elemId, st1) = emitTypeFromLayout st elemLayout
      in emitPointerType st1 storageClass elemId
    TLArray mlen stride elemLayout _ _ ->
      let (elemId, st1) = emitTypeFromLayout st elemLayout
      in case mlen of
        Nothing -> emitRuntimeArrayType elemId stride st1
        Just n ->
          emitArrayType elemId (fromIntegral n) stride st1
    TLStruct name fields _ _ ->
      let nameT = T.pack name
      in case lookup nameT (gsStructIds st) of
        Just sid -> (sid, st)
        Nothing ->
          let (sid, st1) = freshId st
              st2 = st1 { gsStructIds = (nameT, sid) : gsStructIds st1 }
              (st3, fieldTypeIds) = mapAccumL emitFieldType st2 fields
              st4 = addType (Instr opTypeStruct (sid : fieldTypeIds)) st3
              st5 = addName (Instr opName (sid : encodeString name)) st4
              st6 = foldl' (emitMemberDecorate sid) st5 (zip [0 :: Int ..] fields)
              st7 = if nameT `elem` gsBlockStructs st6
                then addDecoration (Instr opDecorate [sid, decorationBlock]) st6
                else st6
              st8 = foldl' (emitMemberName sid) st7 (zip [0 :: Int ..] fields)
          in (sid, st8)
  where
    emitFieldType st' field =
      let (tyId, st'') = emitTypeFromLayout st' (flType field)
      in (st'', tyId)
    emitMemberDecorate structId st' (ix, field) =
      let offset = flOffset field
      in addDecoration (Instr opMemberDecorate [structId, fromIntegral ix, decorationOffset, offset]) st'
    emitMemberName structId st' (ix, field) =
      addName (Instr opMemberName (structId : fromIntegral ix : encodeString (flName field))) st'

emitPointerForBinding :: GenState -> BindingKind -> TypeLayout -> (Word32, GenState)
emitPointerForBinding st kind layout =
  let storageClass = case kind of
        BUniform -> storageClassUniform
        BStorageRead -> storageClassStorageBuffer
        BStorageReadWrite -> storageClassStorageBuffer
        BSampler -> storageClassUniformConstant
        BSamplerComparison -> storageClassUniformConstant
        BTexture1D -> storageClassUniformConstant
        BTexture1DArray -> storageClassUniformConstant
        BTexture2D -> storageClassUniformConstant
        BTexture2DArray -> storageClassUniformConstant
        BTexture3D -> storageClassUniformConstant
        BTextureCube -> storageClassUniformConstant
        BTextureCubeArray -> storageClassUniformConstant
        BTextureMultisampled2D -> storageClassUniformConstant
        BTextureDepth2D -> storageClassUniformConstant
        BTextureDepth2DArray -> storageClassUniformConstant
        BTextureDepthCube -> storageClassUniformConstant
        BTextureDepthCubeArray -> storageClassUniformConstant
        BTextureDepthMultisampled2D -> storageClassUniformConstant
        BStorageTexture1D -> storageClassUniformConstant
        BStorageTexture2D -> storageClassUniformConstant
        BStorageTexture2DArray -> storageClassUniformConstant
        BStorageTexture3D -> storageClassUniformConstant
      (baseId, st1) = emitTypeFromLayout st layout
  in emitPointerType st1 storageClass baseId

emitConstU32 :: GenState -> Word32 -> (Word32, GenState)
emitConstU32 st val =
  emitConst st (ConstU32 val) $ \cid st1 ->
    let (u32Id, st2) = emitIntType False st1
        instr = Instr opConstant [u32Id, cid, val]
    in (instr, st2)

emitConstI32 :: GenState -> Word32 -> (Word32, GenState)
emitConstI32 st val =
  emitConst st (ConstI32 val) $ \cid st1 ->
    let (i32Id, st2) = emitIntType True st1
        instr = Instr opConstant [i32Id, cid, val]
    in (instr, st2)

emitTypeConstU32 :: GenState -> Word32 -> (Word32, GenState)
emitTypeConstU32 st val =
  let (u32Id, st1) = emitIntType False st
      (cid, st2) = freshId st1
      instr = Instr opConstant [u32Id, cid, val]
      st3 = addType instr st2
  in (cid, st3)

emitConstF32Bits :: GenState -> Word32 -> (Word32, GenState)
emitConstF32Bits st bits =
  emitConst st (ConstF32 bits) $ \cid st1 ->
    let (f32Id, st2) = emitFloatType st1
        instr = Instr opConstant [f32Id, cid, bits]
    in (instr, st2)

emitConstF32 :: GenState -> Float -> (Word32, GenState)
emitConstF32 st val =
  emitConstF32Bits st (castFloatToWord32 val)

emitConstF16Bits :: GenState -> Word16 -> (Word32, GenState)
emitConstF16Bits st bits16 =
  emitConst st (ConstF16 bits16) $ \cid st1 ->
    let (f16Id, st2) = emitFloatTypeWidth 16 st1
        instr = Instr opConstant [f16Id, cid, fromIntegral bits16]
    in (instr, st2)

emitConstF16 :: GenState -> Float -> (Word32, GenState)
emitConstF16 st val =
  emitConstF16Bits st (floatToHalfBits val)

emitConstBool :: GenState -> Bool -> (Word32, GenState)
emitConstBool st val =
  emitConst st (ConstBool val) $ \cid st1 ->
    let (boolId, st2) = emitBoolType st1
        instr = Instr (if val then opConstantTrue else opConstantFalse) [boolId, cid]
    in (instr, st2)

emitConstComposite :: TypeLayout -> [Word32] -> GenState -> (Word32, GenState)
emitConstComposite layout parts st =
  let (tyId, st1) = emitTypeFromLayout st layout
      (cid, st2) = freshId st1
      instr = Instr opConstantComposite (tyId : cid : parts)
      st3 = addConst instr st2
  in (cid, st3)

emitSpecConstBool :: GenState -> Bool -> (Word32, GenState)
emitSpecConstBool st val =
  let (boolId, st1) = emitBoolType st
      (cid, st2) = freshId st1
      instr = Instr (if val then opSpecConstantTrue else opSpecConstantFalse) [boolId, cid]
      st3 = addConst instr st2
  in (cid, st3)

emitSpecConstComposite :: TypeLayout -> [Word32] -> GenState -> (Word32, GenState)
emitSpecConstComposite layout parts st =
  let (tyId, st1) = emitTypeFromLayout st layout
      (cid, st2) = freshId st1
      instr = Instr opSpecConstantComposite (tyId : cid : parts)
      st3 = addConst instr st2
  in (cid, st3)


emitExtInst :: TypeLayout -> Word32 -> [Word32] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitExtInst layout inst args st fs = do
  let (setId, st1) = getExtInstSet st "GLSL.std.450"
  let (tyId, st2) = emitTypeFromLayout st1 layout
  let (resId, st3) = freshId st2
  let instr = Instr opExtInst (tyId : resId : setId : inst : args)
  let fs1 = addFuncInstr instr fs
  Right (st3, fs1, Value layout resId)

emitConstFloatScalar :: Scalar -> Float -> GenState -> Either CompileError (Word32, GenState)
emitConstFloatScalar scalar val st =
  case scalar of
    F32 -> Right (emitConstF32 st val)
    F16 -> Right (emitConstF16 st val)
    _ -> Left (CompileError "expected f16 or f32 scalar" Nothing Nothing)

emitConstIntScalar :: Scalar -> Integer -> GenState -> Either CompileError (Word32, GenState)
emitConstIntScalar scalar val st =
  case scalar of
    I32 ->
      if val < fromIntegral (minBound :: Int32) || val > fromIntegral (maxBound :: Int32)
        then Left (CompileError "integer literal is out of range for i32" Nothing Nothing)
        else Right (emitConstI32 st (fromIntegral val))
    U32 ->
      if val < 0 || val > fromIntegral (maxBound :: Word32)
        then Left (CompileError "integer literal is out of range for u32" Nothing Nothing)
        else Right (emitConstU32 st (fromIntegral val))
    _ -> Left (CompileError "expected i32 or u32 scalar" Nothing Nothing)

emitSpecConstFloatScalar :: Scalar -> Float -> GenState -> Either CompileError (Word32, GenState)
emitSpecConstFloatScalar scalar val st =
  case scalar of
    F32 ->
      let (f32Id, st1) = emitFloatType st
          (cid, st2) = freshId st1
          instr = Instr opSpecConstant [f32Id, cid, castFloatToWord32 val]
          st3 = addConst instr st2
      in Right (cid, st3)
    F16 ->
      let (f16Id, st1) = emitFloatTypeWidth 16 st
          (cid, st2) = freshId st1
          instr = Instr opSpecConstant [f16Id, cid, fromIntegral (floatToHalfBits val)]
          st3 = addConst instr st2
      in Right (cid, st3)
    _ -> Left (CompileError "expected f16 or f32 scalar" Nothing Nothing)

emitSpecConstIntScalar :: Scalar -> Integer -> GenState -> Either CompileError (Word32, GenState)
emitSpecConstIntScalar scalar val st =
  case scalar of
    I32 ->
      if val < fromIntegral (minBound :: Int32) || val > fromIntegral (maxBound :: Int32)
        then Left (CompileError "integer literal is out of range for i32" Nothing Nothing)
        else
          let (i32Id, st1) = emitIntType True st
              (cid, st2) = freshId st1
              instr = Instr opSpecConstant [i32Id, cid, fromIntegral (fromIntegral val :: Int32)]
              st3 = addConst instr st2
          in Right (cid, st3)
    U32 ->
      if val < 0 || val > fromIntegral (maxBound :: Word32)
        then Left (CompileError "integer literal is out of range for u32" Nothing Nothing)
        else
          let (u32Id, st1) = emitIntType False st
              (cid, st2) = freshId st1
              instr = Instr opSpecConstant [u32Id, cid, fromIntegral val]
              st3 = addConst instr st2
          in Right (cid, st3)
    _ -> Left (CompileError "expected i32 or u32 scalar" Nothing Nothing)


emitConstIntSplat :: TypeLayout -> Integer -> GenState -> Either CompileError (GenState, Value)
emitConstIntSplat layout val st =
  case layout of
    TLScalar scalar _ _ -> do
      (cid, st1) <- emitConstIntScalar scalar val st
      Right (st1, Value layout cid)
    TLVector n scalar _ _ -> do
      (cid, st1) <- emitConstIntScalar scalar val st
      let (resId, st2) = emitConstComposite layout (replicate n cid) st1
      Right (st2, Value layout resId)
    _ -> Left (CompileError "expected integer scalar or vector layout" Nothing Nothing)

emitConstFloatSplat :: TypeLayout -> Float -> GenState -> Either CompileError (GenState, Value)
emitConstFloatSplat layout val st =
  case layout of
    TLScalar scalar _ _ -> do
      (cid, st1) <- emitConstFloatScalar scalar val st
      Right (st1, Value layout cid)
    TLVector n scalar _ _ -> do
      (cid, st1) <- emitConstFloatScalar scalar val st
      let (resId, st2) = emitConstComposite layout (replicate n cid) st1
      Right (st2, Value layout resId)
    _ -> Left (CompileError "expected float scalar or vector layout" Nothing Nothing)

emitSplatVector :: TypeLayout -> Word32 -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSplatVector layout scalarId st fs =
  case layout of
    TLVector n _ _ _ -> do
      let (tyId, st1) = emitTypeFromLayout st layout
      let (resId, st2) = freshId st1
      let comps = replicate n scalarId
      let fs1 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : comps)) fs
      Right (st2, fs1, Value layout resId)
    _ -> Left (CompileError "expected vector layout for splat" Nothing Nothing)

emitConst :: GenState -> ConstKey -> (Word32 -> GenState -> (Instr, GenState)) -> (Word32, GenState)
emitConst = emitConstWith addConst

emitConstWith :: (Instr -> GenState -> GenState) -> GenState -> ConstKey -> (Word32 -> GenState -> (Instr, GenState)) -> (Word32, GenState)
emitConstWith addInstrFn st key build =
  case lookup key (gsConstCache st) of
    Just cid -> (cid, st)
    Nothing ->
      let (id', st1) = freshId st
          (instr, st2) = build id' st1
          st3 = addInstrFn instr st2
          st4 = st3 { gsConstCache = (key, id') : gsConstCache st3 }
      in (id', st4)

emitTypeCached :: GenState -> TypeKey -> Instr -> (Word32, GenState)
emitTypeCached st key instr =
  case lookup key (gsTypeCache st) of
    Just tid -> (tid, st)
    Nothing ->
      let (tid, st1) = freshId st
          st2 = addType (addResultId tid instr) st1
          st3 = st2 { gsTypeCache = (key, tid) : gsTypeCache st2 }
      in (tid, st3)

addResultId :: Word32 -> Instr -> Instr
addResultId rid (Instr opcode ops) =
  Instr opcode (rid : ops)

-- Type cache key

data TypeKey
  = TKVoid
  | TKBool
  | TKInt Bool Word32
  | TKFloat Word32
  | TKVector Word32 Word32
  | TKMatrix Word32 Word32 Scalar
  | TKSampler
  | TKImage1D Scalar
  | TKImage1DArray Scalar
  | TKImage2D Scalar
  | TKImage2DArray Scalar
  | TKImage3D Scalar
  | TKImageCube Scalar
  | TKImageCubeArray Scalar
  | TKImage2DMultisampled Scalar
  | TKImageDepth2D
  | TKImageDepth2DArray
  | TKImageDepthCube
  | TKImageDepthCubeArray
  | TKImageDepth2DMultisampled
  | TKStorageImage1D StorageFormat
  | TKStorageImage2D StorageFormat
  | TKStorageImage2DArray StorageFormat
  | TKStorageImage3D StorageFormat
  | TKSampledImage Word32
  | TKArray Word32 (Maybe Word32) Word32
  | TKPointer Word32 Word32
  | TKFunction Word32 [Word32]
  deriving (Eq, Show)

data ConstKey
  = ConstU32 Word32
  | ConstI32 Word32
  | ConstF32 Word32
  | ConstF16 Word16
  | ConstBool Bool
  deriving (Eq, Show)

-- Helpers

floatToHalfBits :: Float -> Word16
floatToHalfBits val =
  let w = castFloatToWord32 val
      sign = fromIntegral ((w `shiftR` 16) .&. 0x8000) :: Word16
      expBits = fromIntegral ((w `shiftR` 23) .&. 0xFF) :: Int
      mant = w .&. 0x7FFFFF
      expUnbiased = expBits - 127
  in case expBits of
      0xFF ->
        if mant == 0
          then sign .|. 0x7C00
          else sign .|. 0x7C00 .|. fromIntegral ((mant `shiftR` 13) .&. 0x3FF) .|. 0x1
      _ ->
        if expUnbiased > 15
          then sign .|. 0x7C00
          else if expUnbiased >= -14
            then
              let exp16 = expUnbiased + 15
                  mantRounded = mant + 0x1000
                  exp16' = if mantRounded .&. 0x800000 /= 0 then exp16 + 1 else exp16
                  mant16 = (mantRounded `shiftR` 13) .&. 0x3FF
              in if exp16' >= 31
                  then sign .|. 0x7C00
                  else sign .|. (fromIntegral exp16' `shiftL` 10) .|. fromIntegral mant16
            else if expUnbiased >= -24
              then
                let shift = (-expUnbiased) - 14
                    mant32 = mant .|. 0x800000
                    mantRounded = mant32 + (1 `shiftL` (shift + 12))
                    mant16 = mantRounded `shiftR` (shift + 13)
                in sign .|. fromIntegral mant16
              else sign

halfBitsToFloat :: Word16 -> Float
halfBitsToFloat bits =
  let sign = if bits .&. 0x8000 == 0 then 1.0 else -1.0
      expBits = fromIntegral ((bits `shiftR` 10) .&. 0x1F) :: Int
      mant = fromIntegral (bits .&. 0x3FF) :: Float
  in case expBits of
      0 ->
        if mant == 0
          then sign * 0.0
          else sign * (2 ** (-14)) * (mant / 1024.0)
      31 ->
        if mant == 0
          then sign * (1 / 0)
          else 0 / 0
      _ ->
        sign * (2 ** fromIntegral (expBits - 15)) * (1.0 + mant / 1024.0)

spirvToBytes :: [Word32] -> ByteString
spirvToBytes words32 = BS.pack (concatMap wordToBytes words32)

wordToBytes :: Word32 -> [Word8]
wordToBytes w =
  [ fromIntegral (w .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 24) .&. 0xFF)
  ]

encodeString :: String -> [Word32]
encodeString str =
  let bytes = map (fromIntegral . fromEnum) (str <> "\0")
      chunks = chunk4 bytes
  in map packWord chunks

chunk4 :: [Word32] -> [[Word32]]
chunk4 [] = []
chunk4 xs = take 4 (xs <> repeat 0) : chunk4 (drop 4 xs)

packWord :: [Word32] -> Word32
packWord [a, b, c, d] = a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24)
packWord _ = 0

bytesToExp :: ByteString -> Q Exp
bytesToExp bytes = do
  let ints = map (fromIntegral :: Word8 -> Integer) (BS.unpack bytes)
  listExp <- TH.listE (map (TH.litE . TH.integerL) ints)
  let listSig = TH.SigE listExp (TH.AppT TH.ListT (TH.ConT ''Word8))
  pure (TH.AppE (TH.VarE 'BS.pack) listSig)

interfaceToExp :: ShaderInterface -> Q Exp
interfaceToExp (ShaderInterface bindings overrides) = do
  bindingsExp <- TH.listE (map bindingToExp bindings)
  overridesExp <- TH.listE (map overrideToExp overrides)
  pure (TH.AppE (TH.AppE (TH.ConE 'ShaderInterface) bindingsExp) overridesExp)

bindingToExp :: BindingInfo -> Q Exp
bindingToExp info =
  TH.recConE 'BindingInfo
    [ TH.fieldExp 'biName (TH.litE (TH.stringL (biName info)))
    , TH.fieldExp 'biKind (TH.conE (bindingKindCon (biKind info)))
    , TH.fieldExp 'biGroup (TH.litE (TH.integerL (fromIntegral (biGroup info))))
    , TH.fieldExp 'biBinding (TH.litE (TH.integerL (fromIntegral (biBinding info))))
    , TH.fieldExp 'biType (typeLayoutToExp (biType info))
    ]

overrideToExp :: OverrideInfo -> Q Exp
overrideToExp info =
  TH.recConE 'OverrideInfo
    [ TH.fieldExp 'oiName (TH.litE (TH.stringL (oiName info)))
    , TH.fieldExp 'oiId (maybeToExp (oiId info))
    , TH.fieldExp 'oiSpecId (maybeToExp (oiSpecId info))
    , TH.fieldExp 'oiType (typeLayoutToExp (oiType info))
    ]

maybeToExp :: Maybe Word32 -> Q Exp
maybeToExp val =
  case val of
    Nothing -> TH.conE 'Nothing
    Just v -> TH.appE (TH.conE 'Just) (TH.litE (TH.integerL (fromIntegral v)))

bindingKindCon :: BindingKind -> TH.Name
bindingKindCon kind = case kind of
  BUniform -> 'BUniform
  BStorageRead -> 'BStorageRead
  BStorageReadWrite -> 'BStorageReadWrite
  BSampler -> 'BSampler
  BSamplerComparison -> 'BSamplerComparison
  BTexture1D -> 'BTexture1D
  BTexture1DArray -> 'BTexture1DArray
  BTexture2D -> 'BTexture2D
  BTexture2DArray -> 'BTexture2DArray
  BTexture3D -> 'BTexture3D
  BTextureCube -> 'BTextureCube
  BTextureCubeArray -> 'BTextureCubeArray
  BTextureMultisampled2D -> 'BTextureMultisampled2D
  BTextureDepth2D -> 'BTextureDepth2D
  BTextureDepth2DArray -> 'BTextureDepth2DArray
  BTextureDepthCube -> 'BTextureDepthCube
  BTextureDepthCubeArray -> 'BTextureDepthCubeArray
  BTextureDepthMultisampled2D -> 'BTextureDepthMultisampled2D
  BStorageTexture1D -> 'BStorageTexture1D
  BStorageTexture2D -> 'BStorageTexture2D
  BStorageTexture2DArray -> 'BStorageTexture2DArray
  BStorageTexture3D -> 'BStorageTexture3D

storageFormatCon :: StorageFormat -> TH.Name
storageFormatCon fmt = case fmt of
  FormatRgba8Unorm -> 'FormatRgba8Unorm
  FormatRgba8Snorm -> 'FormatRgba8Snorm
  FormatRgba8Uint -> 'FormatRgba8Uint
  FormatRgba8Sint -> 'FormatRgba8Sint
  FormatRgba16Float -> 'FormatRgba16Float
  FormatRgba16Uint -> 'FormatRgba16Uint
  FormatRgba16Sint -> 'FormatRgba16Sint
  FormatRgba16Unorm -> 'FormatRgba16Unorm
  FormatRgba16Snorm -> 'FormatRgba16Snorm
  FormatRgba32Float -> 'FormatRgba32Float
  FormatRgba32Uint -> 'FormatRgba32Uint
  FormatRgba32Sint -> 'FormatRgba32Sint
  FormatR32Float -> 'FormatR32Float
  FormatR32Uint -> 'FormatR32Uint
  FormatR32Sint -> 'FormatR32Sint
  FormatR16Float -> 'FormatR16Float
  FormatR16Uint -> 'FormatR16Uint
  FormatR16Sint -> 'FormatR16Sint
  FormatR16Unorm -> 'FormatR16Unorm
  FormatR16Snorm -> 'FormatR16Snorm
  FormatR8Unorm -> 'FormatR8Unorm
  FormatR8Snorm -> 'FormatR8Snorm
  FormatR8Uint -> 'FormatR8Uint
  FormatR8Sint -> 'FormatR8Sint
  FormatRg32Float -> 'FormatRg32Float
  FormatRg32Uint -> 'FormatRg32Uint
  FormatRg32Sint -> 'FormatRg32Sint
  FormatRg16Float -> 'FormatRg16Float
  FormatRg16Uint -> 'FormatRg16Uint
  FormatRg16Sint -> 'FormatRg16Sint
  FormatRg16Unorm -> 'FormatRg16Unorm
  FormatRg16Snorm -> 'FormatRg16Snorm
  FormatRg8Unorm -> 'FormatRg8Unorm
  FormatRg8Snorm -> 'FormatRg8Snorm
  FormatRg8Uint -> 'FormatRg8Uint
  FormatRg8Sint -> 'FormatRg8Sint
  FormatRgb10a2Unorm -> 'FormatRgb10a2Unorm
  FormatRgb10a2Uint -> 'FormatRgb10a2Uint
  FormatRg11b10Float -> 'FormatRg11b10Float

storageAccessCon :: StorageAccess -> TH.Name
storageAccessCon access = case access of
  StorageRead -> 'StorageRead
  StorageWrite -> 'StorageWrite
  StorageReadWrite -> 'StorageReadWrite

typeLayoutToExp :: TypeLayout -> Q Exp
typeLayoutToExp layout =
  case layout of
    TLScalar s a sz ->
      TH.appE (TH.appE (TH.appE (TH.conE 'TLScalar) (scalarToExp s)) (TH.litE (TH.integerL (fromIntegral a))))
        (TH.litE (TH.integerL (fromIntegral sz)))
    TLVector n s a sz ->
      TH.appE
        (TH.appE (TH.appE (TH.appE (TH.conE 'TLVector) (TH.litE (TH.integerL (fromIntegral n)))) (scalarToExp s)) (TH.litE (TH.integerL (fromIntegral a))))
        (TH.litE (TH.integerL (fromIntegral sz)))
    TLMatrix cols rows s a sz stride ->
      TH.appE
        (TH.appE (TH.appE (TH.appE (TH.appE (TH.appE (TH.conE 'TLMatrix) (TH.litE (TH.integerL (fromIntegral cols)))) (TH.litE (TH.integerL (fromIntegral rows)))) (scalarToExp s)) (TH.litE (TH.integerL (fromIntegral a))))
          (TH.litE (TH.integerL (fromIntegral sz))))
        (TH.litE (TH.integerL (fromIntegral stride)))
    TLArray mlen stride elemLayout a sz -> do
      elemExp <- typeLayoutToExp elemLayout
      mlenExp <- case mlen of
        Nothing -> TH.conE 'Nothing
        Just n -> TH.appE (TH.conE 'Just) (TH.litE (TH.integerL (fromIntegral n)))
      let exp1 = TH.AppE (TH.ConE 'TLArray) mlenExp
          exp2 = TH.AppE exp1 (TH.LitE (TH.integerL (fromIntegral stride)))
          exp3 = TH.AppE exp2 elemExp
          exp4 = TH.AppE exp3 (TH.LitE (TH.integerL (fromIntegral a)))
          exp5 = TH.AppE exp4 (TH.LitE (TH.integerL (fromIntegral sz)))
      pure exp5
    TLStruct name fields a sz -> do
      fieldsExp <- TH.listE (map fieldLayoutToExp fields)
      let exp1 = TH.AppE (TH.ConE 'TLStruct) (TH.LitE (TH.stringL name))
          exp2 = TH.AppE exp1 fieldsExp
          exp3 = TH.AppE exp2 (TH.LitE (TH.integerL (fromIntegral a)))
          exp4 = TH.AppE exp3 (TH.LitE (TH.integerL (fromIntegral sz)))
      pure exp4
    TLPointer storageClass access elemLayout -> do
      elemExp <- typeLayoutToExp elemLayout
      accessExp <- case access of
        Nothing -> TH.conE 'Nothing
        Just a -> TH.appE (TH.conE 'Just) (storageAccessToExp a)
      let exp1 = TH.AppE (TH.ConE 'TLPointer) (TH.LitE (TH.integerL (fromIntegral storageClass)))
          exp2 = TH.AppE exp1 accessExp
          exp3 = TH.AppE exp2 elemExp
      pure exp3
    TLSampler -> TH.conE 'TLSampler
    TLSamplerComparison -> TH.conE 'TLSamplerComparison
    TLTexture1D s -> TH.appE (TH.conE 'TLTexture1D) (scalarToExp s)
    TLTexture1DArray s -> TH.appE (TH.conE 'TLTexture1DArray) (scalarToExp s)
    TLTexture2D s -> TH.appE (TH.conE 'TLTexture2D) (scalarToExp s)
    TLTexture2DArray s -> TH.appE (TH.conE 'TLTexture2DArray) (scalarToExp s)
    TLTexture3D s -> TH.appE (TH.conE 'TLTexture3D) (scalarToExp s)
    TLTextureCube s -> TH.appE (TH.conE 'TLTextureCube) (scalarToExp s)
    TLTextureCubeArray s -> TH.appE (TH.conE 'TLTextureCubeArray) (scalarToExp s)
    TLTextureMultisampled2D s -> TH.appE (TH.conE 'TLTextureMultisampled2D) (scalarToExp s)
    TLTextureDepth2D -> TH.conE 'TLTextureDepth2D
    TLTextureDepth2DArray -> TH.conE 'TLTextureDepth2DArray
    TLTextureDepthCube -> TH.conE 'TLTextureDepthCube
    TLTextureDepthCubeArray -> TH.conE 'TLTextureDepthCubeArray
    TLTextureDepthMultisampled2D -> TH.conE 'TLTextureDepthMultisampled2D
    TLStorageTexture1D fmt access ->
      TH.appE (TH.appE (TH.conE 'TLStorageTexture1D) (storageFormatToExp fmt)) (storageAccessToExp access)
    TLStorageTexture2D fmt access ->
      TH.appE (TH.appE (TH.conE 'TLStorageTexture2D) (storageFormatToExp fmt)) (storageAccessToExp access)
    TLStorageTexture2DArray fmt access ->
      TH.appE (TH.appE (TH.conE 'TLStorageTexture2DArray) (storageFormatToExp fmt)) (storageAccessToExp access)
    TLStorageTexture3D fmt access ->
      TH.appE (TH.appE (TH.conE 'TLStorageTexture3D) (storageFormatToExp fmt)) (storageAccessToExp access)
    TLAtomic s -> TH.appE (TH.conE 'TLAtomic) (scalarToExp s)

fieldLayoutToExp :: FieldLayout -> Q Exp
fieldLayoutToExp fld =
  TH.recConE 'FieldLayout
    [ TH.fieldExp 'flName (TH.litE (TH.stringL (flName fld)))
    , TH.fieldExp 'flOffset (TH.litE (TH.integerL (fromIntegral (flOffset fld))))
    , TH.fieldExp 'flType (typeLayoutToExp (flType fld))
    , TH.fieldExp 'flAlign (TH.litE (TH.integerL (fromIntegral (flAlign fld))))
    , TH.fieldExp 'flSize (TH.litE (TH.integerL (fromIntegral (flSize fld))))
    ]

scalarToExp :: Scalar -> Q Exp
scalarToExp s = case s of
  I32 -> TH.conE 'I32
  U32 -> TH.conE 'U32
  F16 -> TH.conE 'F16
  F32 -> TH.conE 'F32
  Bool -> TH.conE 'Bool

storageFormatToExp :: StorageFormat -> Q Exp
storageFormatToExp fmt = TH.conE (storageFormatCon fmt)

storageAccessToExp :: StorageAccess -> Q Exp
storageAccessToExp access = TH.conE (storageAccessCon access)

interfaceToType :: ShaderInterface -> TH.Type
interfaceToType (ShaderInterface bindings _) =
  foldr
    (\b acc -> TH.AppT (TH.AppT TH.PromotedConsT (bindingToType b)) acc)
    TH.PromotedNilT
    bindings

bindingToType :: BindingInfo -> TH.Type
bindingToType info =
  foldl'
    TH.AppT
    (TH.PromotedT 'Binding)
    [ TH.LitT (TH.StrTyLit (biName info))
    , TH.PromotedT (bindingKindTypeCon (biKind info))
    , TH.LitT (TH.NumTyLit (fromIntegral (biGroup info)))
    , TH.LitT (TH.NumTyLit (fromIntegral (biBinding info)))
    , typeLayoutToType (biType info)
    ]

bindingKindTypeCon :: BindingKind -> TH.Name
bindingKindTypeCon kind = case kind of
  BUniform -> 'BUniform
  BStorageRead -> 'BStorageRead
  BStorageReadWrite -> 'BStorageReadWrite
  BSampler -> 'BSampler
  BSamplerComparison -> 'BSamplerComparison
  BTexture1D -> 'BTexture1D
  BTexture1DArray -> 'BTexture1DArray
  BTexture2D -> 'BTexture2D
  BTexture2DArray -> 'BTexture2DArray
  BTexture3D -> 'BTexture3D
  BTextureCube -> 'BTextureCube
  BTextureCubeArray -> 'BTextureCubeArray
  BTextureMultisampled2D -> 'BTextureMultisampled2D
  BTextureDepth2D -> 'BTextureDepth2D
  BTextureDepth2DArray -> 'BTextureDepth2DArray
  BTextureDepthCube -> 'BTextureDepthCube
  BTextureDepthCubeArray -> 'BTextureDepthCubeArray
  BTextureDepthMultisampled2D -> 'BTextureDepthMultisampled2D
  BStorageTexture1D -> 'BStorageTexture1D
  BStorageTexture2D -> 'BStorageTexture2D
  BStorageTexture2DArray -> 'BStorageTexture2DArray
  BStorageTexture3D -> 'BStorageTexture3D

typeLayoutToType :: TypeLayout -> TH.Type
typeLayoutToType layout =
  case layout of
    TLScalar s _ _ -> TH.AppT (TH.PromotedT 'TScalar) (scalarTypeToType s)
    TLVector n s _ _ ->
      TH.AppT
        (TH.AppT (TH.PromotedT 'TVec) (TH.LitT (TH.NumTyLit (fromIntegral n))))
        (scalarTypeToType s)
    TLMatrix cols rows s _ _ _ ->
      TH.AppT
        (TH.AppT (TH.AppT (TH.PromotedT 'TMatrix) (TH.LitT (TH.NumTyLit (fromIntegral cols)))) (TH.LitT (TH.NumTyLit (fromIntegral rows))))
        (scalarTypeToType s)
    TLArray mlen _ elemLayout _ _ ->
      case mlen of
        Nothing -> TH.AppT (TH.PromotedT 'TRuntimeArray) (typeLayoutToType elemLayout)
        Just n -> TH.AppT (TH.AppT (TH.PromotedT 'TArray) (TH.LitT (TH.NumTyLit (fromIntegral n)))) (typeLayoutToType elemLayout)
    TLStruct _ fields _ _ ->
      let fieldTypes = foldr (\f acc -> TH.AppT (TH.AppT TH.PromotedConsT (fieldLayoutToType f)) acc) TH.PromotedNilT fields
      in TH.AppT (TH.PromotedT 'TStruct) fieldTypes
    TLPointer _ _ _ -> error "pointer types are not supported in interface reflection"
    TLSampler -> TH.PromotedT 'TSampler
    TLSamplerComparison -> TH.PromotedT 'TSamplerComparison
    TLTexture1D s -> TH.AppT (TH.PromotedT 'TTexture1D) (scalarTypeToType s)
    TLTexture1DArray s -> TH.AppT (TH.PromotedT 'TTexture1DArray) (scalarTypeToType s)
    TLTexture2D s -> TH.AppT (TH.PromotedT 'TTexture2D) (scalarTypeToType s)
    TLTexture2DArray s -> TH.AppT (TH.PromotedT 'TTexture2DArray) (scalarTypeToType s)
    TLTexture3D s -> TH.AppT (TH.PromotedT 'TTexture3D) (scalarTypeToType s)
    TLTextureCube s -> TH.AppT (TH.PromotedT 'TTextureCube) (scalarTypeToType s)
    TLTextureCubeArray s -> TH.AppT (TH.PromotedT 'TTextureCubeArray) (scalarTypeToType s)
    TLTextureMultisampled2D s -> TH.AppT (TH.PromotedT 'TTextureMultisampled2D) (scalarTypeToType s)
    TLTextureDepth2D -> TH.PromotedT 'TTextureDepth2D
    TLTextureDepth2DArray -> TH.PromotedT 'TTextureDepth2DArray
    TLTextureDepthCube -> TH.PromotedT 'TTextureDepthCube
    TLTextureDepthCubeArray -> TH.PromotedT 'TTextureDepthCubeArray
    TLTextureDepthMultisampled2D -> TH.PromotedT 'TTextureDepthMultisampled2D
    TLStorageTexture1D fmt access ->
      TH.AppT (TH.AppT (TH.PromotedT 'TStorageTexture1D) (storageFormatToType fmt)) (storageAccessToType access)
    TLStorageTexture2D fmt access ->
      TH.AppT (TH.AppT (TH.PromotedT 'TStorageTexture2D) (storageFormatToType fmt)) (storageAccessToType access)
    TLStorageTexture2DArray fmt access ->
      TH.AppT (TH.AppT (TH.PromotedT 'TStorageTexture2DArray) (storageFormatToType fmt)) (storageAccessToType access)
    TLStorageTexture3D fmt access ->
      TH.AppT (TH.AppT (TH.PromotedT 'TStorageTexture3D) (storageFormatToType fmt)) (storageAccessToType access)
    TLAtomic s -> TH.AppT (TH.PromotedT 'TAtomic) (scalarTypeToType s)

fieldLayoutToType :: FieldLayout -> TH.Type
fieldLayoutToType fld =
  TH.AppT
    (TH.AppT (TH.PromotedT 'Field) (TH.LitT (TH.StrTyLit (flName fld))))
    (typeLayoutToType (flType fld))

scalarTypeToType :: Scalar -> TH.Type
scalarTypeToType s =
  TH.PromotedT $ case s of
    I32 -> 'SI32
    U32 -> 'SU32
    F16 -> 'SF16
    F32 -> 'SF32
    Bool -> 'SBool

storageFormatToType :: StorageFormat -> TH.Type
storageFormatToType fmt = TH.PromotedT (storageFormatCon fmt)

storageAccessToType :: StorageAccess -> TH.Type
storageAccessToType access = TH.PromotedT (storageAccessCon access)
