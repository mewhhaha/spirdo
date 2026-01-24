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

module Spirdo.Wesl.Types where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32)
import Data.List (find, intercalate, sortOn)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import GHC.Float (castFloatToWord32)
import GHC.Generics (Generic, Rep, K1(..), M1(..), (:*:)(..), Selector, selName, S, from)

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

type family HasBinding (name :: Symbol) (iface :: [Binding]) :: Bool where
  HasBinding _ '[] = 'False
  HasBinding name ('Binding name _ _ _ _ ': _) = 'True
  HasBinding name (_ ': rest) = HasBinding name rest

binding :: forall name iface. (KnownSymbol name, HasBinding name iface ~ 'True, ReflectBindings iface) => CompiledShader iface -> BindingDesc
binding _ =
  let key = symbolVal (Proxy @name)
  in case find (\b -> descName b == key) (reflectBindings (Proxy @iface)) of
      Just b -> b
      Nothing -> error ("binding: missing " <> key <> " (impossible)")

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
  toUniform (M2 (V2 a b) (V2 c d)) =
    UVMatrix 2 2 [toScalar a, toScalar b, toScalar c, toScalar d]

instance ToScalar a => ToUniform (M3 a) where
  toUniform (M3 (V3 a b c) (V3 d e f) (V3 g h i)) =
    UVMatrix 3 3 [toScalar a, toScalar b, toScalar c, toScalar d, toScalar e, toScalar f, toScalar g, toScalar h, toScalar i]

instance ToScalar a => ToUniform (M4 a) where
  toUniform (M4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) =
    UVMatrix 4 4
      [ toScalar a, toScalar b, toScalar c, toScalar d
      , toScalar e, toScalar f, toScalar g, toScalar h
      , toScalar i, toScalar j, toScalar k, toScalar l
      , toScalar m, toScalar n, toScalar o, toScalar p
      ]

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
        safeName = if null name then "_unnamed" else name
    in [(safeName, toUniform (unK1 (unM1 m1)))]

type UniformPath = String

packUniform :: TypeLayout -> UniformValue -> Either String ByteString
packUniform layout value = do
  let size = fromIntegral (layoutSize layout)
  segs <- collectSegments "" 0 layout value
  builder <- assembleSegments size segs
  pure (BSL.toStrict (toLazyByteString builder))

packUniformFrom :: ToUniform a => TypeLayout -> a -> Either String ByteString
packUniformFrom layout value = packUniform layout (uniform value)

data Segment = Segment
  { segOff :: !Int
  , segBytes :: !ByteString
  }

collectSegments :: UniformPath -> Int -> TypeLayout -> UniformValue -> Either String [Segment]
collectSegments ctx off layout value =
  case (layout, value) of
    (TLScalar s _ _, UVScalar v) ->
      case scalarValueBytes s v of
        Left err -> Left (formatAt ctx err)
        Right bytes -> Right [Segment off (BS.pack bytes)]
    (TLVector n s _ _, UVVector n' vals)
      | n == n' ->
          fmap concat $
            traverse
              (\(ix, val) ->
                case scalarValueBytes s val of
                  Left err -> Left (formatAt (ctxIndex ctx ix) err)
                  Right bytes -> Right [Segment (off + ix * scalarByteSize s) (BS.pack bytes)])
              (zip [0 ..] vals)
      | otherwise ->
          Left (formatAt ctx ("vector length mismatch: expected " <> show n <> ", got " <> show n'))
    (TLMatrix cols rows s _ _ stride, UVMatrix c r vals)
      | cols == c && rows == r ->
          let expected = cols * rows
          in if length vals /= expected
            then Left (formatAt ctx ("matrix value count mismatch: expected " <> show expected <> ", got " <> show (length vals)))
            else
              fmap concat $
                traverse
                  (\(ix, val) ->
                    let col = ix `div` rows
                        row = ix `mod` rows
                        base = off + col * fromIntegral stride + row * scalarByteSize s
                    in case scalarValueBytes s val of
                        Left err -> Left (formatAt (ctxIndex (ctxIndex ctx col) row) err)
                        Right bytes -> Right [Segment base (BS.pack bytes)])
                  (zip [0 ..] vals)
      | otherwise ->
          Left (formatAt ctx ("matrix size mismatch: expected " <> show cols <> "x" <> show rows <> ", got " <> show c <> "x" <> show r))
    (TLArray mlen stride elemLayout _ _, UVArray elems) ->
      case mlen of
        Just n | n /= length elems ->
          Left (formatAt ctx ("array length mismatch: expected " <> show n <> ", got " <> show (length elems)))
        _ ->
          fmap concat $
            traverse
              (\(ix, el) ->
                collectSegments (ctxIndex ctx ix) (off + ix * fromIntegral stride) elemLayout el)
              (zip [0 ..] elems)
    (TLStruct structName fields _ _, UVStruct vals) ->
      let structCtx = if null ctx then structName else ctx
          nameCounts = Map.fromListWith (+) [(n, 1 :: Int) | (n, _) <- vals]
          dupes = Map.keys (Map.filter (> 1) nameCounts)
      in if not (null dupes)
        then Left (formatAt structCtx ("duplicate struct fields: " <> intercalate ", " dupes))
        else
          let fieldNames = map flName fields
              fieldSet = Set.fromList fieldNames
              extra = filter (`Set.notMember` fieldSet) (map fst vals)
          in if not (null extra)
            then Left (formatAt structCtx ("unexpected struct fields: " <> intercalate ", " extra))
            else
              let valMap = Map.fromList vals
              in fmap concat $
                  traverse
                    (\fld ->
                      case Map.lookup (flName fld) valMap of
                        Nothing -> Left (formatAt structCtx ("missing struct field: " <> flName fld))
                        Just v ->
                          collectSegments (ctxField structCtx (flName fld)) (off + fromIntegral (flOffset fld)) (flType fld) v)
                    fields
    _ ->
      Left (formatAt ctx ("uniform value does not match layout: " <> show layout))

assembleSegments :: Int -> [Segment] -> Either String Builder
assembleSegments size segs = go 0 (sortOn segOff segs)
  where
    go pos [] =
      if pos > size
        then Left "uniform write out of bounds"
        else Right (pad (size - pos))
    go pos (Segment off bytes : rest)
      | off < pos = Left "uniform write overlap"
      | off > size = Left "uniform write out of bounds"
      | off + BS.length bytes > size = Left "uniform write out of bounds"
      | otherwise = do
          next <- go (off + BS.length bytes) rest
          Right (pad (off - pos) <> byteString bytes <> next)
    pad n
      | n <= 0 = mempty
      | otherwise = byteString (BS.replicate n 0)

formatAt :: UniformPath -> String -> String
formatAt ctx msg =
  if null ctx
    then msg
    else "at " <> ctx <> ": " <> msg

ctxField :: UniformPath -> String -> UniformPath
ctxField ctx name =
  if null ctx then name else ctx <> "." <> name

ctxIndex :: UniformPath -> Int -> UniformPath
ctxIndex ctx ix =
  if null ctx
    then "[" <> show ix <> "]"
    else ctx <> "[" <> show ix <> "]"

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

newtype BindingMap = BindingMap (Map.Map String BindingInfo)

bindingMap :: ShaderInterface -> BindingMap
bindingMap iface =
  BindingMap (Map.fromList [(biName info, info) | info <- siBindings iface])

bindingInfoFor :: String -> ShaderInterface -> Either String BindingInfo
bindingInfoFor name iface =
  case bindingInfoForMap name (bindingMap iface) of
    Just info -> Right info
    Nothing -> Left ("binding not found in interface: " <> name)

bindingInfoForMap :: String -> BindingMap -> Maybe BindingInfo
bindingInfoForMap name (BindingMap m) = Map.lookup name m

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

roundUp :: Word32 -> Word32 -> Word32
roundUp val align =
  if align == 0
    then val
    else ((val + align - 1) `div` align) * align

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
