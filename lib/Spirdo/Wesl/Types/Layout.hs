{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Runtime type/layout metadata for uniforms and bindings.
module Spirdo.Wesl.Types.Layout
  ( StorageAccess(..)
  , StorageFormat(..)
  , Scalar(..)
  , ConstExpr(..)
  , ConstBinOp(..)
  , ArrayLen(..)
  , Type(..)
  , FieldLayout(..)
  , TypeLayout(..)
  , layoutAlign
  , layoutSize
  , scalarLayout
  , vectorLayout
  , roundUp
  , matrixLayout
  ) where

import Data.Text (Text)
import Data.Word (Word32)

-- Storage access/format

-- | Access qualifier for storage buffers/textures.
data StorageAccess = StorageRead | StorageWrite | StorageReadWrite
  deriving (Eq, Show, Read)

-- | Storage texture format (subset of WGSL storage formats).
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
  deriving (Eq, Ord, Show, Read)

-- Runtime interface representation

-- | Runtime scalar type.
data Scalar = I32 | U32 | F16 | F32 | Bool
  deriving (Eq, Ord, Show, Read)

-- | Constant integer expression (subset) used in array lengths and attributes.
data ConstExpr
  = CEInt !Integer
  | CEIdent !Text
  | CEUnaryNeg !ConstExpr
  | CEBinary !ConstBinOp !ConstExpr !ConstExpr
  | CECall !Text ![ConstExpr]
  deriving (Eq, Show, Read)

data ConstBinOp
  = CAdd
  | CSub
  | CMul
  | CDiv
  | CMod
  | CShl
  | CShr
  | CBitAnd
  | CBitOr
  | CBitXor
  deriving (Eq, Show, Read)

data ArrayLen
  = ArrayLenRuntime
  | ArrayLenFixed !Int
  | ArrayLenExpr !ConstExpr
  deriving (Eq, Show, Read)

-- | Parsed WGSL type for reflection and layout.
data Type
  = TyScalar Scalar
  | TyVector Int Scalar
  | TyMatrix Int Int Scalar
  | TyArray Type ArrayLen
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
  deriving (Eq, Show, Read)

-- Layout metadata

-- | Struct field layout information.
data FieldLayout = FieldLayout
  { flName :: !String
  , flOffset :: !Word32
  , flType :: !TypeLayout
  , flAlign :: !Word32
  , flSize :: !Word32
  } deriving (Eq, Show, Read)

-- | Layout metadata used for uniform packing and reflection.
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

-- | Alignment (bytes) for a layout. Returns 0 for non-uniform types.
layoutAlign :: TypeLayout -> Word32
layoutAlign tl = case tl of
  TLScalar _ a _ -> a
  TLVector _ _ a _ -> a
  TLMatrix _ _ _ a _ _ -> a
  TLArray _ _ _ a _ -> a
  TLStruct _ _ a _ -> a
  TLPointer {} -> 0
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

-- | Size (bytes) for a layout. Returns 0 for non-uniform types.
layoutSize :: TypeLayout -> Word32
layoutSize tl = case tl of
  TLScalar _ _ s -> s
  TLVector _ _ _ s -> s
  TLMatrix _ _ _ _ s _ -> s
  TLArray _ _ _ _ s -> s
  TLStruct _ _ _ s -> s
  TLPointer {} -> 0
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

-- | Alignment/size for a scalar type.
scalarLayout :: Scalar -> (Word32, Word32)
scalarLayout s =
  case s of
    F16 -> (2, 2)
    _ -> (4, 4)

-- | Alignment/size for a vector type.
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

-- | Round @val@ up to the next multiple of @align@.
roundUp :: Word32 -> Word32 -> Word32
roundUp val align =
  if align == 0
    then val
    else ((val + align - 1) `div` align) * align

-- | Layout metadata for a column-major matrix.
matrixLayout :: Int -> Int -> Scalar -> TypeLayout
matrixLayout cols rows scalar =
  let (a, sz) = vectorLayout scalar rows
      stride = roundUp sz a
      total = stride * fromIntegral cols
  in TLMatrix cols rows scalar a total stride
