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
  , ScalarValue(..)
  , UniformValue(..)
  , ToScalar(..)
  , ToUniform(..)
  , uniform
  , packUniform
  , packUniformFrom
  ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32)
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word8, Word16, Word32)
import GHC.Float (castFloatToWord32)
import GHC.Generics (Generic, Rep, K1(..), M1(..), (:*:)(..), Selector, selName, S, from)

import Spirdo.Wesl.Types.Layout
  ( FieldLayout(..)
  , Scalar(..)
  , TypeLayout(..)
  , layoutSize
  )

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

data Segment = Segment !Int !ByteString

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
assembleSegments size segs = go 0 (sortOn segOffset segs)
  where
    segOffset (Segment off _) = off
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
