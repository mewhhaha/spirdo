{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Uniform value types and packing helpers.
module Spirdo.Wesl.Types.Uniform
  ( Half(..)
  , V2(..)
  , V3(..)
  , V4(..)
  , M2(..)
  , M3(..)
  , M4(..)
  , M3x4(..)
  , M4x3(..)
  , ScalarValue(..)
  , UniformValue(..)
  , ToScalar(..)
  , ToUniform(..)
  , uniform
  , packUniform
  , packUniformFrom
  , validateUniformStorable
  , packUniformStorable
  ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word8, Word16, Word32)
import Data.Proxy (Proxy(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Float (castFloatToWord32)
import GHC.Generics (Generic, Rep, K1(..), M1(..), (:*:)(..), Selector, selName, S, from)

import Spirdo.Wesl.Types.Layout
  ( FieldLayout(..)
  , Scalar(..)
  , TypeLayout(..)
  , layoutAlign
  , layoutSize
  )

-- | Typed uniform values for packing.

-- | 16-bit float storage (IEEE 754 half).
newtype Half = Half Word16
  deriving (Eq, Show)

-- | 2D vector.
data V2 a = V2 !a !a
  deriving (Eq, Show)

instance Functor V2 where
  fmap f (V2 a b) = V2 (f a) (f b)

instance Foldable V2 where
  foldMap f (V2 a b) = f a <> f b

instance Traversable V2 where
  traverse f (V2 a b) = V2 <$> f a <*> f b

-- | 3D vector.
data V3 a = V3 !a !a !a
  deriving (Eq, Show)

instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)

instance Foldable V3 where
  foldMap f (V3 a b c) = f a <> f b <> f c

instance Traversable V3 where
  traverse f (V3 a b c) = V3 <$> f a <*> f b <*> f c

-- | 4D vector.
data V4 a = V4 !a !a !a !a
  deriving (Eq, Show)

instance Functor V4 where
  fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)

instance Foldable V4 where
  foldMap f (V4 a b c d) = f a <> f b <> f c <> f d

instance Traversable V4 where
  traverse f (V4 a b c d) = V4 <$> f a <*> f b <*> f c <*> f d

-- | 2x2 matrix (column-major).
data M2 a = M2 !(V2 a) !(V2 a)
  deriving (Eq, Show)

instance Functor M2 where
  fmap f (M2 a b) = M2 (fmap f a) (fmap f b)

instance Foldable M2 where
  foldMap f (M2 a b) = foldMap f a <> foldMap f b

instance Traversable M2 where
  traverse f (M2 a b) = M2 <$> traverse f a <*> traverse f b

-- | 3x3 matrix (column-major).
data M3 a = M3 !(V3 a) !(V3 a) !(V3 a)
  deriving (Eq, Show)

instance Functor M3 where
  fmap f (M3 a b c) = M3 (fmap f a) (fmap f b) (fmap f c)

instance Foldable M3 where
  foldMap f (M3 a b c) = foldMap f a <> foldMap f b <> foldMap f c

instance Traversable M3 where
  traverse f (M3 a b c) = M3 <$> traverse f a <*> traverse f b <*> traverse f c

-- | 4x4 matrix (column-major).
data M4 a = M4 !(V4 a) !(V4 a) !(V4 a) !(V4 a)
  deriving (Eq, Show)

instance Functor M4 where
  fmap f (M4 a b c d) = M4 (fmap f a) (fmap f b) (fmap f c) (fmap f d)

instance Foldable M4 where
  foldMap f (M4 a b c d) = foldMap f a <> foldMap f b <> foldMap f c <> foldMap f d

instance Traversable M4 where
  traverse f (M4 a b c d) = M4 <$> traverse f a <*> traverse f b <*> traverse f c <*> traverse f d

-- | 3x4 matrix (column-major).
data M3x4 a = M3x4 !(V4 a) !(V4 a) !(V4 a)
  deriving (Eq, Show)

instance Functor M3x4 where
  fmap f (M3x4 a b c) = M3x4 (fmap f a) (fmap f b) (fmap f c)

instance Foldable M3x4 where
  foldMap f (M3x4 a b c) = foldMap f a <> foldMap f b <> foldMap f c

instance Traversable M3x4 where
  traverse f (M3x4 a b c) = M3x4 <$> traverse f a <*> traverse f b <*> traverse f c

-- | 4x3 matrix (column-major).
data M4x3 a = M4x3 !(V3 a) !(V3 a) !(V3 a) !(V3 a)
  deriving (Eq, Show)

instance Functor M4x3 where
  fmap f (M4x3 a b c d) = M4x3 (fmap f a) (fmap f b) (fmap f c) (fmap f d)

instance Foldable M4x3 where
  foldMap f (M4x3 a b c d) = foldMap f a <> foldMap f b <> foldMap f c <> foldMap f d

instance Traversable M4x3 where
  traverse f (M4x3 a b c d) = M4x3 <$> traverse f a <*> traverse f b <*> traverse f c <*> traverse f d

-- | Scalar leaf for manual uniform construction.
data ScalarValue
  = SVI32 !Int32
  | SVU32 !Word32
  | SVF16 !Word16
  | SVF32 !Float
  | SVBool !Bool
  deriving (Eq, Show)

-- | Uniform value tree used for layout-aware packing.
data UniformValue
  = UVScalar !ScalarValue
  | UVVector !Int ![ScalarValue]
  | UVMatrix !Int !Int ![ScalarValue]
  | UVArray ![UniformValue]
  | UVStruct ![(String, UniformValue)]
  deriving (Eq, Show)

-- | Convert a scalar host value into a 'ScalarValue'.
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

-- | Convert host values into uniform trees.
class ToUniform a where
  toUniform :: a -> UniformValue
  default toUniform :: (Generic a, GUniform (Rep a)) => a -> UniformValue
  toUniform a = UVStruct (gUniform (from a))

-- | Convenience wrapper for 'toUniform'.
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

instance ToScalar a => ToUniform (M3x4 a) where
  toUniform (M3x4 (V4 a b c d) (V4 e f g h) (V4 i j k l)) =
    UVMatrix 3 4
      [ toScalar a, toScalar b, toScalar c, toScalar d
      , toScalar e, toScalar f, toScalar g, toScalar h
      , toScalar i, toScalar j, toScalar k, toScalar l
      ]

instance ToScalar a => ToUniform (M4x3 a) where
  toUniform (M4x3 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l)) =
    UVMatrix 4 3
      [ toScalar a, toScalar b, toScalar c
      , toScalar d, toScalar e, toScalar f
      , toScalar g, toScalar h, toScalar i
      , toScalar j, toScalar k, toScalar l
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

-- | Pack a uniform value against a reflected layout.
packUniform :: TypeLayout -> UniformValue -> Either String ByteString
packUniform layout value = do
  let size = fromIntegral (layoutSize layout)
  (builder, pos) <- emitValue size "" 0 layout value 0
  if pos > size
    then Left "uniform write out of bounds"
    else
      let final = builder <> padBytes (size - pos)
      in pure (BSL.toStrict (toLazyByteString final))

-- | Pack a host value by first converting it via 'ToUniform'.
packUniformFrom :: ToUniform a => TypeLayout -> a -> Either String ByteString
packUniformFrom layout value = packUniform layout (uniform value)

-- | Validate that a 'Storable' matches the requested layout.
validateUniformStorable :: forall a. Storable a => TypeLayout -> Proxy a -> Either String ()
validateUniformStorable layout _ =
  let wantSize = fromIntegral (layoutSize layout)
      wantAlign = fromIntegral (layoutAlign layout)
      gotSize = sizeOf (undefined :: a)
      gotAlign = alignment (undefined :: a)
  in if gotSize /= wantSize
      then Left ("storable size mismatch: expected " <> show wantSize <> ", got " <> show gotSize)
      else if wantAlign > gotAlign
        then Left ("storable alignment mismatch: expected >= " <> show wantAlign <> ", got " <> show gotAlign)
        else Right ()

-- | Pack a 'Storable' into a layout-compatible byte blob.
packUniformStorable :: forall a. Storable a => TypeLayout -> a -> IO (Either String ByteString)
packUniformStorable layout value =
  case validateUniformStorable layout (Proxy @a) of
    Left err -> pure (Left err)
    Right () -> do
      let size = fromIntegral (layoutSize layout)
      alloca $ \ptr -> do
        poke ptr value
        Right <$> BS.packCStringLen (castPtr ptr, size)

emitValue :: Int -> UniformPath -> Int -> TypeLayout -> UniformValue -> Int -> Either String (Builder, Int)
emitValue size ctx off layout value pos =
  case (layout, value) of
    (TLScalar s _ _, UVScalar v) ->
      emitScalar ctx s v >>= \bytes ->
        emitSegment size off bytes pos
    (TLVector n s _ _, UVVector n' vals)
      | n == n' ->
          foldMBuilder pos (zip [0 ..] vals) $ \(ix, val) curPos ->
            emitScalar (ctxIndex ctx ix) s val >>= \bytes ->
              emitSegment size (off + ix * scalarByteSize s) bytes curPos
      | otherwise ->
          Left (formatAt ctx ("vector length mismatch: expected " <> show n <> ", got " <> show n'))
    (TLMatrix cols rows s _ _ stride, UVMatrix c r vals)
      | cols == c && rows == r ->
          let expected = cols * rows
          in if length vals /= expected
            then Left (formatAt ctx ("matrix value count mismatch: expected " <> show expected <> ", got " <> show (length vals)))
            else
              foldMBuilder pos (zip [0 ..] vals) $ \(ix, val) curPos ->
                let col = ix `div` rows
                    row = ix `mod` rows
                    base = off + col * fromIntegral stride + row * scalarByteSize s
                in emitScalar (ctxIndex (ctxIndex ctx col) row) s val >>= \bytes ->
                    emitSegment size base bytes curPos
      | otherwise ->
          Left (formatAt ctx ("matrix size mismatch: expected " <> show cols <> "x" <> show rows <> ", got " <> show c <> "x" <> show r))
    (TLArray mlen stride elemLayout _ _, UVArray elems) ->
      case mlen of
        Just n | n /= length elems ->
          Left (formatAt ctx ("array length mismatch: expected " <> show n <> ", got " <> show (length elems)))
        _ ->
          foldMBuilder pos (zip [0 ..] elems) $ \(ix, el) curPos ->
            emitValue size (ctxIndex ctx ix) (off + ix * fromIntegral stride) elemLayout el curPos
    (TLStruct structName fields _ _, UVStruct vals) ->
      let structCtx = if null ctx then structName else ctx
          nameCounts = Map.fromListWith (+) [(n, 1 :: Int) | (n, _) <- vals]
          dupes = Map.keys (Map.filter (> 1) nameCounts)
      in if not (null dupes)
        then Left (formatAt structCtx ("duplicate struct fields: " <> intercalate ", " dupes))
        else
          let fieldNames = map (.flName) fields
              fieldSet = Set.fromList fieldNames
              extra = filter (`Set.notMember` fieldSet) (map fst vals)
          in if not (null extra)
            then Left (formatAt structCtx ("unexpected struct fields: " <> intercalate ", " extra))
            else
              let valMap = Map.fromList vals
              in foldMBuilder pos fields $ \fld curPos -> do
                  v <- maybe (Left (formatAt structCtx ("missing struct field: " <> fld.flName))) Right
                    (Map.lookup fld.flName valMap)
                  emitValue size (ctxField structCtx fld.flName) (off + fromIntegral fld.flOffset) fld.flType v curPos
    _ ->
      Left (formatAt ctx ("uniform value does not match layout: " <> show layout))

emitScalar :: UniformPath -> Scalar -> ScalarValue -> Either String ByteString
emitScalar ctx scalar value =
  BS.pack <$> either (Left . formatAt ctx) Right (scalarValueBytes scalar value)

emitSegment :: Int -> Int -> ByteString -> Int -> Either String (Builder, Int)
emitSegment size off bytes pos
  | off < pos = Left "uniform write overlap"
  | off > size = Left "uniform write out of bounds"
  | off + BS.length bytes > size = Left "uniform write out of bounds"
  | otherwise =
      let padding = padBytes (off - pos)
          nextPos = off + BS.length bytes
      in Right (padding <> byteString bytes, nextPos)

padBytes :: Int -> Builder
padBytes n
  | n <= 0 = mempty
  | otherwise = byteString (BS.replicate n 0)

foldMBuilder :: Int -> [a] -> (a -> Int -> Either String (Builder, Int)) -> Either String (Builder, Int)
foldMBuilder start items step = go mempty start items
  where
    go acc pos [] = Right (acc, pos)
    go acc pos (x:xs) = do
      (chunk, pos') <- step x pos
      go (acc <> chunk) pos' xs

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
