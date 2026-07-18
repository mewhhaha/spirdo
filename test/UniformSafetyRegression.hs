module UniformSafetyRegression (checks) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Word (Word16, Word32)
import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff)

import Spirdo.Wesl.Reflection
  ( FieldLayout(..)
  , Scalar(..)
  , TypeLayout(..)
  )
import Spirdo.Wesl.Uniform
  ( ScalarValue(..)
  , UniformValue(..)
  , packUniform
  , packUniformStorableUnchecked
  , validateUniformStorableUnchecked
  )

data PaddedHost = PaddedHost !Word16 !Word32

instance Storable PaddedHost where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = PaddedHost <$> peekByteOff ptr 0 <*> peekByteOff ptr 4
  poke ptr (PaddedHost low high) = do
    pokeByteOff ptr 0 low
    pokeByteOff ptr 4 high

newtype StrictHost = StrictHost Word32

instance Storable StrictHost where
  sizeOf (StrictHost _) = 4
  alignment (StrictHost _) = 4
  peek ptr = StrictHost <$> peekByteOff ptr 0
  poke ptr (StrictHost value) = pokeByteOff ptr 0 value

checks :: [(String, IO ())]
checks =
  [ ("uniform-safety:storable-padding-zero", checkStorablePaddingIsZero)
  , ("uniform-safety:strict-storable-metadata", checkStrictStorableMetadata)
  , ("uniform-safety:short-vector", expectPackingFailure "short vector" vectorLayout shortVector)
  , ("uniform-safety:long-vector", expectPackingFailure "long vector" vectorLayout longVector)
  , ("uniform-safety:negative-vector-width", expectPackingFailure "negative vector width" negativeVectorLayout (UVVector (-1) []))
  , ("uniform-safety:impossible-matrix-dimensions", expectPackingFailure "impossible matrix dimensions" impossibleMatrixLayout impossibleMatrixValue)
  , ("uniform-safety:negative-array-length", expectPackingFailure "negative array length" negativeArrayLayout (UVArray []))
  , ("uniform-safety:overflowing-array-size", expectPackingFailure "overflowing array size" overflowingArrayLayout (UVArray []))
  , ("uniform-safety:inconsistent-array-stride", expectPackingFailure "inconsistent array stride" inconsistentArrayLayout twoScalars)
  , ("uniform-safety:overlapping-struct-fields", expectPackingFailure "overlapping struct fields" overlappingStructLayout overlappingStructValue)
  , ("uniform-safety:runtime-sized-array", expectPackingFailure "runtime-sized array" runtimeArrayLayout (UVArray [scalarValue]))
  , ("uniform-safety:pathological-allocation", expectPackingFailure "pathological allocation" pathologicalArrayLayout (UVArray []))
  ]

checkStrictStorableMetadata :: IO ()
checkStrictStorableMetadata =
  case validateUniformStorableUnchecked scalarLayout (StrictHost 0) of
    Left err -> fail ("strict Storable validation failed: " <> err)
    Right () -> pure ()

checkStorablePaddingIsZero :: IO ()
checkStorablePaddingIsZero = do
  let halfLayout = TLScalar F16 2 2
      wordLayout = TLScalar U32 4 4
      layout =
        TLStruct
          "PaddedHost"
          [ FieldLayout "low" 0 halfLayout 2 2
          , FieldLayout "high" 4 wordLayout 4 4
          ]
          4
          8
  packed <- packUniformStorableUnchecked layout (PaddedHost 0xA5A5 0xA5A5A5A5)
  bytes <- case packed of
    Left err -> fail ("padded Storable packing failed: " <> err)
    Right result -> pure result
  let expected = BS.pack [0xA5, 0xA5, 0, 0, 0xA5, 0xA5, 0xA5, 0xA5]
  unless (bytes == expected) $
    fail ("padded Storable expected " <> show expected <> ", got " <> show bytes)

vectorLayout :: TypeLayout
vectorLayout = TLVector 3 F32 16 12

shortVector :: UniformValue
shortVector = UVVector 3 [SVF32 1, SVF32 2]

longVector :: UniformValue
longVector = UVVector 3 [SVF32 1, SVF32 2, SVF32 3, SVF32 4]

negativeVectorLayout :: TypeLayout
negativeVectorLayout = TLVector (-1) F32 16 16

impossibleMatrixLayout :: TypeLayout
impossibleMatrixLayout = TLMatrix maxBound maxBound F32 16 16 16

impossibleMatrixValue :: UniformValue
impossibleMatrixValue = UVMatrix maxBound maxBound []

negativeArrayLayout :: TypeLayout
negativeArrayLayout = TLArray (Just (-1)) 4 scalarLayout 4 4

overflowingArrayLayout :: TypeLayout
overflowingArrayLayout = TLArray (Just maxBound) 4 scalarLayout 4 4

inconsistentArrayLayout :: TypeLayout
inconsistentArrayLayout = TLArray (Just 2) 8 scalarLayout 4 16

twoScalars :: UniformValue
twoScalars = UVArray [scalarValue, scalarValue]

overlappingStructLayout :: TypeLayout
overlappingStructLayout =
  TLStruct
    "Overlap"
    [ FieldLayout "first" 0 scalarLayout 4 4
    , FieldLayout "second" 2 scalarLayout 4 4
    ]
    4
    8

overlappingStructValue :: UniformValue
overlappingStructValue = UVStruct [("first", scalarValue), ("second", scalarValue)]

runtimeArrayLayout :: TypeLayout
runtimeArrayLayout = TLArray Nothing 4 scalarLayout 4 0

pathologicalArrayLayout :: TypeLayout
pathologicalArrayLayout =
  TLArray (Just pathologicalLength) 4 scalarLayout 4 pathologicalSize
  where
    pathologicalLength = 16 * 1024 * 1024 + 1
    pathologicalSize = fromIntegral (pathologicalLength * 4) :: Word32

scalarLayout :: TypeLayout
scalarLayout = TLScalar U32 4 4

scalarValue :: UniformValue
scalarValue = UVScalar (SVU32 0)

expectPackingFailure :: String -> TypeLayout -> UniformValue -> IO ()
expectPackingFailure label layout value =
  case packUniform layout value of
    Left _ -> pure ()
    Right bytes -> fail (label <> " unexpectedly packed " <> show (BS.length bytes) <> " bytes")
