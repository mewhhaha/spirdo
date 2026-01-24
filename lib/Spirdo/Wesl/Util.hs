{-# LANGUAGE OverloadedStrings #-}

-- | Internal utilities for diagnostics and formatting.
module Spirdo.Wesl.Util
  ( errorAt
  , errorAtPos
  , withPos
  , renderError
  , textToString
  , attrInt
  , attrIntMaybe
  , attrLocation
  , attrBuiltin
  , attrInts
  , paramLocation
  , paramBuiltin
  , bindingNumbers
  , entryAttributesMaybe
  , toBindingKind
  , bindingKindFromType
  , parseMatrixName
  , vecSize
  , selectIntLiteralScalar
  , exprToLValue
  , ptrAccessAllowsWrite
  , ptrAccessCompatible
  , vectorFieldIndex
  , vectorFieldIndices
  , floatToHalfBits
  , halfBitsToFloat
  , builtInGlobalInvocationId
  , builtInLocalInvocationId
  , builtInLocalInvocationIndex
  , builtInWorkgroupId
  , builtInNumWorkgroups
  , builtInPosition
  , builtInVertexIndex
  , builtInInstanceIndex
  , builtInFragCoord
  , builtInFrontFacing
  , builtInSampleIndex
  , builtInFragDepth
  , imageFormatUnknown
  , storageFormatScalar
  , storageFormatToImageFormat
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (isDigit)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word32)
import GHC.Float (castFloatToWord32)
import Spirdo.Wesl.Syntax (Expr(..), LValue(..), SrcPos(..), Stage(..), Token(..), UnaryOp(..))
import Spirdo.Wesl.Types

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

paramLocation :: [Attr] -> Maybe Word32
paramLocation = attrLocation

paramBuiltin :: [Attr] -> Maybe Text
paramBuiltin = attrBuiltin

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

exprToLValue :: Expr -> Maybe LValue
exprToLValue expr =
  case expr of
    EVar name -> Just (LVVar name)
    EField base field -> LVField <$> exprToLValue base <*> pure field
    EIndex base idx -> LVIndex <$> exprToLValue base <*> pure idx
    EUnary OpDeref inner -> Just (LVDeref inner)
    _ -> Nothing

ptrAccessAllowsWrite :: Maybe StorageAccess -> Bool
ptrAccessAllowsWrite acc =
  case acc of
    Nothing -> True
    Just StorageRead -> False
    Just StorageReadWrite -> True
    Just StorageWrite -> True

ptrAccessCompatible :: Maybe StorageAccess -> Maybe StorageAccess -> Bool
ptrAccessCompatible expected actual =
  if ptrAccessAllowsWrite expected
    then ptrAccessAllowsWrite actual
    else True

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
