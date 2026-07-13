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
  , validateUniformStorableUnchecked
  , packUniformStorableUnchecked
  , validateUniformStorable
  , packUniformStorable
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString, word16LE, word32LE)
import qualified Data.ByteString.Lazy as BSL
import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word16, Word32)
import Data.Proxy (Proxy(..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (alignPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.Float (castFloatToWord32)
import GHC.Generics (Generic, Rep, K1(..), M1(..), (:*:)(..), Selector, selName, S, from)

import Spirdo.Wesl.Types.Layout
  ( FieldLayout(..)
  , Scalar(..)
  , TypeLayout(..)
  , layoutAlign
  , layoutSize
  , matrixLayout
  , scalarLayout
  , vectorLayout
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
  size <- validatePackingLayout layout
  (builder, pos) <- emitValue size "" 0 layout value 0
  if pos > size
    then Left "uniform write out of bounds"
    else
      let final = builder <> padBytes (size - pos)
      in pure (BSL.toStrict (toLazyByteString final))

-- | Pack a host value by first converting it via 'ToUniform'.
packUniformFrom :: ToUniform a => TypeLayout -> a -> Either String ByteString
packUniformFrom layout value = packUniform layout (uniform value)

-- | Check only the size and alignment of a 'Storable' against a uniform layout.
--
-- This does not verify field offsets, padding, scalar representation, byte
-- order, or platform ABI compatibility. Prefer 'packUniformFrom' unless the
-- host type's ABI has been independently verified for every target platform.
validateUniformStorableUnchecked :: forall a. Storable a => TypeLayout -> Proxy a -> Either String ()
validateUniformStorableUnchecked layout _ = do
  wantSize <- validatePackingLayout layout
  let wantAlign = fromIntegral (layoutAlign layout)
      gotSize = sizeOf (undefined :: a)
      gotAlign = alignment (undefined :: a)
  if gotSize /= wantSize
    then Left ("storable size mismatch: expected " <> show wantSize <> ", got " <> show gotSize)
    else if not (isPowerOfTwoInt gotAlign) || wantAlign > gotAlign
      then Left ("storable alignment mismatch: expected >= " <> show wantAlign <> ", got " <> show gotAlign)
      else if toInteger gotSize + toInteger gotAlign - 1 > maxPackingBytes
        then Left ("storable allocation exceeds packing limit: size " <> show gotSize <> ", alignment " <> show gotAlign)
        else Right ()

-- | Copy a 'Storable' value into bytes after only a size-and-alignment check.
--
-- This inherits the ABI caveats of 'validateUniformStorableUnchecked'. In
-- particular, it cannot establish that fields or padding match the shader
-- layout. Prefer 'packUniformFrom' for portable, layout-aware packing.
packUniformStorableUnchecked :: forall a. Storable a => TypeLayout -> a -> IO (Either String ByteString)
packUniformStorableUnchecked layout value =
  case validateUniformStorableUnchecked layout (Proxy @a) of
    Left err -> pure (Left err)
    Right () -> do
      let size = fromIntegral (layoutSize layout)
          align = alignment (undefined :: a)
          allocationSize = size + align - 1
      bracket (mallocBytes allocationSize) free $ \allocation -> do
        let ptr = alignPtr allocation align
        fillBytes ptr 0 size
        poke ptr value
        Right <$> BS.packCStringLen (castPtr ptr, size)

-- | Legacy name for 'validateUniformStorableUnchecked'.
{-# DEPRECATED validateUniformStorable "Use validateUniformStorableUnchecked; this check covers only size and alignment, not the complete shader ABI." #-}
validateUniformStorable :: forall a. Storable a => TypeLayout -> Proxy a -> Either String ()
validateUniformStorable = validateUniformStorableUnchecked

-- | Legacy name for 'packUniformStorableUnchecked'.
{-# DEPRECATED packUniformStorable "Use packUniformStorableUnchecked; this function relies on an unchecked host ABI layout." #-}
packUniformStorable :: forall a. Storable a => TypeLayout -> a -> IO (Either String ByteString)
packUniformStorable = packUniformStorableUnchecked

-- Public layout constructors must not permit unbounded allocation or traversal.
maxPackingBytes :: Integer
maxPackingBytes = 64 * 1024 * 1024

maxPackingLayoutDepth :: Int
maxPackingLayoutDepth = 128

maxPackingLayoutNodes :: Int
maxPackingLayoutNodes = 4096

validatePackingLayout :: TypeLayout -> Either String Int
validatePackingLayout layout = do
  let byteCount = toInteger (layoutSize layout)
  validateByteCount "" byteCount
  _ <- validateLayout 0 maxPackingLayoutNodes "" layout
  if byteCount > toInteger (maxBound :: Int)
    then Left "uniform layout size exceeds host Int"
    else Right (fromInteger byteCount)

validateLayout :: Int -> Int -> UniformPath -> TypeLayout -> Either String Int
validateLayout depth remaining ctx layout
  | depth > maxPackingLayoutDepth =
      Left (formatAt ctx ("uniform layout nesting exceeds " <> show maxPackingLayoutDepth))
  | remaining <= 0 =
      Left (formatAt ctx ("uniform layout contains more than " <> show maxPackingLayoutNodes <> " nodes"))
  | otherwise = do
      validateByteCount ctx (toInteger (layoutSize layout))
      case layout of
        TLScalar scalar align size -> do
          let (expectedAlign, expectedSize) = scalarLayout scalar
          requireLayoutWord ctx "scalar alignment" expectedAlign align
          requireLayoutWord ctx "scalar size" expectedSize size
          Right (remaining - 1)
        TLVector width scalar align size -> do
          requireDimension ctx "vector width" width
          let (expectedAlign, expectedSize) = vectorLayout scalar width
          requireLayoutWord ctx "vector alignment" expectedAlign align
          requireLayoutWord ctx "vector size" expectedSize size
          Right (remaining - 1)
        TLMatrix cols rows scalar align size stride -> do
          requireDimension ctx "matrix column count" cols
          requireDimension ctx "matrix row count" rows
          case scalar of
            F16 -> pure ()
            F32 -> pure ()
            _ -> Left (formatAt ctx ("matrix scalar must be F16 or F32, got " <> show scalar))
          case matrixLayout cols rows scalar of
            TLMatrix _ _ _ expectedAlign expectedSize expectedStride -> do
              requireLayoutWord ctx "matrix alignment" expectedAlign align
              requireLayoutWord ctx "matrix size" expectedSize size
              requireLayoutWord ctx "matrix column stride" expectedStride stride
            _ -> Left (formatAt ctx "internal matrix layout error")
          Right (remaining - 1)
        TLArray Nothing _ _ _ _ ->
          Left (formatAt ctx "runtime-sized arrays cannot be packed as uniforms")
        TLArray (Just count) stride elemLayout align size -> do
          if count <= 0
            then Left (formatAt ctx ("array length must be positive, got " <> show count))
            else pure ()
          remaining' <- validateLayout (depth + 1) (remaining - 1) (ctxIndex ctx 0) elemLayout
          let elemAlign = layoutAlign elemLayout
              elemSize = layoutSize elemLayout
              expectedStride = roundUpInteger (toInteger elemSize) (toInteger elemAlign)
              expectedSize = expectedStride * toInteger count
          requireLayoutWord ctx "array alignment" elemAlign align
          requireLayoutInteger ctx "array stride" expectedStride stride
          requireLayoutInteger ctx "array size" expectedSize size
          Right remaining'
        TLStruct structName fields align size ->
          let structCtx = if null ctx then structName else ctx
          in do
            (remaining', expectedAlign, fieldEnd) <-
              validateFields depth (remaining - 1) structCtx Set.empty 0 1 fields
            requireLayoutWord structCtx "struct alignment" expectedAlign align
            requireLayoutInteger structCtx "struct size" (roundUpInteger fieldEnd (toInteger expectedAlign)) size
            Right remaining'
        _ -> Left (formatAt ctx "layout is not a packable uniform value type")

validateFields
  :: Int
  -> Int
  -> UniformPath
  -> Set.Set String
  -> Integer
  -> Word32
  -> [FieldLayout]
  -> Either String (Int, Word32, Integer)
validateFields _ remaining _ _ fieldEnd structAlign [] =
  Right (remaining, structAlign, fieldEnd)
validateFields depth remaining ctx names fieldEnd structAlign (field : fields)
  | Set.member field.flName names =
      Left (formatAt ctx ("duplicate layout field: " <> field.flName))
  | otherwise = do
      let fieldCtx = ctxField ctx field.flName
      remaining' <- validateLayout (depth + 1) remaining fieldCtx field.flType
      if not (isPowerOfTwo field.flAlign)
        then Left (formatAt fieldCtx ("field alignment must be a power of two, got " <> show field.flAlign))
        else pure ()
      if field.flAlign < layoutAlign field.flType
        then Left (formatAt fieldCtx ("field alignment " <> show field.flAlign <> " is below natural alignment " <> show (layoutAlign field.flType)))
        else pure ()
      if field.flSize < layoutSize field.flType
        then Left (formatAt fieldCtx ("field size " <> show field.flSize <> " is below natural size " <> show (layoutSize field.flType)))
        else pure ()
      let expectedOffset = roundUpInteger fieldEnd (toInteger field.flAlign)
      requireLayoutInteger fieldCtx "field offset" expectedOffset field.flOffset
      let nextEnd = expectedOffset + toInteger field.flSize
      validateByteCount fieldCtx nextEnd
      validateFields
        depth
        remaining'
        ctx
        (Set.insert field.flName names)
        nextEnd
        (max structAlign field.flAlign)
        fields

validateByteCount :: UniformPath -> Integer -> Either String ()
validateByteCount ctx byteCount
  | byteCount < 0 = Left (formatAt ctx ("uniform byte size is negative: " <> show byteCount))
  | byteCount > maxPackingBytes =
      Left (formatAt ctx ("uniform byte size " <> show byteCount <> " exceeds packing limit " <> show maxPackingBytes))
  | otherwise = Right ()

requireDimension :: UniformPath -> String -> Int -> Either String ()
requireDimension ctx name dimension
  | dimension >= 2 && dimension <= 4 = Right ()
  | otherwise = Left (formatAt ctx (name <> " must be between 2 and 4, got " <> show dimension))

requireLayoutWord :: UniformPath -> String -> Word32 -> Word32 -> Either String ()
requireLayoutWord ctx name expected actual
  | actual == expected = Right ()
  | otherwise = Left (formatAt ctx (name <> " mismatch: expected " <> show expected <> ", got " <> show actual))

requireLayoutInteger :: UniformPath -> String -> Integer -> Word32 -> Either String ()
requireLayoutInteger ctx name expected actual
  | toInteger actual == expected = Right ()
  | otherwise = Left (formatAt ctx (name <> " mismatch: expected " <> show expected <> ", got " <> show actual))

roundUpInteger :: Integer -> Integer -> Integer
roundUpInteger value align = ((value + align - 1) `div` align) * align

isPowerOfTwo :: Word32 -> Bool
isPowerOfTwo value = value /= 0 && value .&. (value - 1) == 0

isPowerOfTwoInt :: Int -> Bool
isPowerOfTwoInt value = value > 0 && value .&. (value - 1) == 0

emitValue :: Int -> UniformPath -> Int -> TypeLayout -> UniformValue -> Int -> Either String (Builder, Int)
emitValue size ctx off layout value pos =
  case (layout, value) of
    (TLScalar s _ _, UVScalar v) ->
      emitScalar ctx s v >>= \(chunk, chunkLen) ->
        emitSegment size off chunkLen chunk pos
    (TLVector n s _ _, UVVector n' vals)
      | n == n' -> do
          requireExactCount ctx "vector value count" n vals
          foldMBuilderIndexed pos vals $ \ix val curPos ->
            emitScalar (ctxIndex ctx ix) s val >>= \(chunk, chunkLen) ->
              emitSegment size (off + ix * scalarByteSize s) chunkLen chunk curPos
      | otherwise ->
          Left (formatAt ctx ("vector length mismatch: expected " <> show n <> ", got " <> show n'))
    (TLMatrix cols rows s _ _ stride, UVMatrix c r vals)
      | cols == c && rows == r -> do
          requireExactCount ctx "matrix value count" expectedVals vals
          foldMBuilderIndexed pos vals $ \ix val curPos ->
            let col = ix `div` rows
                row = ix `mod` rows
                base = off + col * fromIntegral stride + row * scalarByteSize s
            in emitScalar (ctxIndex (ctxIndex ctx col) row) s val >>= \(chunk, chunkLen) ->
                emitSegment size base chunkLen chunk curPos
      | otherwise ->
          Left (formatAt ctx ("matrix size mismatch: expected " <> show cols <> "x" <> show rows <> ", got " <> show c <> "x" <> show r))
      where
        expectedVals = cols * rows
    (TLArray (Just n) stride elemLayout _ _, UVArray elems) -> do
      requireExactCount ctx "array length" n elems
      foldMBuilderIndexed pos elems $ \ix el curPos ->
        emitValue size (ctxIndex ctx ix) (off + ix * fromIntegral stride) elemLayout el curPos
    (TLStruct structName fields _ _, UVStruct vals) ->
      let structCtx = if null ctx then structName else ctx
      in emitStruct structCtx fields vals
    _ ->
      Left (formatAt ctx ("uniform value does not match layout: " <> show layout))
  where
    emitStruct structCtx fields vals =
      let fieldSet = Set.fromList (map (.flName) fields)
          boundedVals = take (length fields + 1) vals
          (valMap, dupes, extras) = foldl' (collectField fieldSet) (Map.empty, Set.empty, Set.empty) boundedVals
      in case structFieldError dupes extras of
          Just err -> Left (formatAt structCtx err)
          Nothing ->
            foldMBuilder pos fields $ \fld curPos -> do
              v <- maybe (Left (formatAt structCtx ("missing struct field: " <> fld.flName))) Right
                (Map.lookup fld.flName valMap)
              emitValue size (ctxField structCtx fld.flName) (off + fromIntegral fld.flOffset) fld.flType v curPos

    collectField fieldSet (valMap, dupes, extras) (name, fieldVal) =
      let dupes' =
            if Map.member name valMap
              then Set.insert name dupes
              else dupes
          extras' =
            if Set.member name fieldSet
              then extras
              else Set.insert name extras
          valMap' = Map.insert name fieldVal valMap
      in (valMap', dupes', extras')

    structFieldError dupes extras =
      renderSet "duplicate struct fields: " dupes
        <|> renderSet "unexpected struct fields: " extras

    renderSet prefix names
      | Set.null names = Nothing
      | otherwise = Just (prefix <> intercalate ", " (Set.toList names))

emitScalar :: UniformPath -> Scalar -> ScalarValue -> Either String (Builder, Int)
emitScalar ctx scalar value =
  first (formatAt ctx) (scalarValueBuilder scalar value)

emitSegment :: Int -> Int -> Int -> Builder -> Int -> Either String (Builder, Int)
emitSegment size off chunkLen chunk pos
  | off < pos = Left "uniform write overlap"
  | off > size = Left "uniform write out of bounds"
  | off + chunkLen > size = Left "uniform write out of bounds"
  | otherwise =
      let padding = padBytes (off - pos)
          nextPos = off + chunkLen
      in Right (padding <> chunk, nextPos)

padBytes :: Int -> Builder
padBytes n
  | n <= 0 = mempty
  | otherwise = lazyByteString (BSL.replicate (fromIntegral n) 0)

foldMBuilder :: Int -> [a] -> (a -> Int -> Either String (Builder, Int)) -> Either String (Builder, Int)
foldMBuilder start items step = foldM go (mempty, start) items
  where
    go (acc, pos) x = do
      (chunk, pos') <- step x pos
      pure (acc <> chunk, pos')

foldMBuilderIndexed :: Int -> [a] -> (Int -> a -> Int -> Either String (Builder, Int)) -> Either String (Builder, Int)
foldMBuilderIndexed start items step = go mempty start 0 items
  where
    go acc pos _ [] = Right (acc, pos)
    go acc pos ix (x:xs) = do
      (chunk, pos') <- step ix x pos
      go (acc <> chunk) pos' (ix + 1) xs

requireExactCount :: UniformPath -> String -> Int -> [a] -> Either String ()
requireExactCount ctx name expected values =
  case countUpTo expected values of
    Just actual
      | actual == expected -> Right ()
      | otherwise -> Left (formatAt ctx (name <> " mismatch: expected " <> show expected <> ", got " <> show actual))
    Nothing -> Left (formatAt ctx (name <> " mismatch: expected " <> show expected <> ", got more than " <> show expected))

countUpTo :: Int -> [a] -> Maybe Int
countUpTo limit = go 0 limit
  where
    go count _ [] = Just count
    go _ 0 (_ : _) = Nothing
    go count remaining (_ : rest) = go (count + 1) (remaining - 1) rest

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

scalarByteSize :: Scalar -> Int
scalarByteSize s = case s of
  F16 -> 2
  _ -> 4

scalarValueBuilder :: Scalar -> ScalarValue -> Either String (Builder, Int)
scalarValueBuilder scalar value =
  case (scalar, value) of
    (I32, SVI32 v) -> Right (word32LE (fromIntegral v), 4)
    (U32, SVU32 v) -> Right (word32LE v, 4)
    (F32, SVF32 v) -> Right (word32LE (castFloatToWord32 v), 4)
    (F16, SVF16 v) -> Right (word16LE v, 2)
    (Bool, SVBool v) -> Right (word32LE (if v then 1 else 0), 4)
    _ -> Left ("scalar type mismatch: expected " <> show scalar <> ", got " <> show value)
