{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level interface and reflection types.
module Spirdo.Wesl.Types.Interface
  ( BindingKind(..)
  , SamplerBindingMode(..)
  , ScalarType(..)
  , Field(..)
  , Ty(..)
  , Binding(..)
  , ShaderStage(..)
  , IOParam(..)
  , StageIO(..)
  , BindingPlan(..)
  , shaderStage
  , PreparedShader(..)
  , SomePreparedShader(..)
  , prepareShader
  , preparedSpirv
  , preparedInterface
  , preparedStage
  , preparedPlan
  , preparedVertexAttributes
  , somePreparedSpirv
  , somePreparedInterface
  , stageIO
  , stageInputs
  , stageOutputs
  , vertexInputs
  , vertexOutputs
  , vertexAttributes
  , VertexFormat(..)
  , VertexAttribute(..)
  , bindingPlan
  , isUniformKind
  , isSamplerKind
  , isTextureKind
  , isStorageBufferKind
  , isStorageTextureKind
  , pushConstantLayout
  , BindingInfo(..)
  , BindingMap(..)
  , bindingMap
  , bindingInfoFor
  , bindingInfoForMap
  , OverrideInfo(..)
  , ShaderInterface(..)
  , specializableOverrides
  , CompiledShader(..)
  , SomeCompiledShader(..)
  ) where

import Data.ByteString (ByteString)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Word (Word32)
import Data.Ord (comparing)
import GHC.TypeLits (Symbol, Nat)

import Spirdo.Wesl.Types.Layout
  ( StorageAccess(..)
  , StorageFormat(..)
  , Scalar(..)
  , TypeLayout(..)
  )

-- Type-level interface representation

-- | Kind of a shader binding (uniform, sampler, texture, storage, ...).
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

-- | How samplers are represented in the reflected interface.
data SamplerBindingMode
  = SamplerSeparate
  | SamplerCombined
  deriving (Eq, Show, Read)

-- | Scalar element type for type-level bindings.
data ScalarType = SI32 | SU32 | SF16 | SF32 | SBool
  deriving (Eq, Show, Read)

-- | Type-level struct field (name + type).
data Field = Field Symbol Ty

-- | Type-level binding type for interface reflection.
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

-- | Type-level binding descriptor: name, kind, group, binding, type.
data Binding = Binding Symbol BindingKind Nat Nat Ty

-- | Entry-point shader stage.
data ShaderStage
  = ShaderStageCompute
  | ShaderStageFragment
  | ShaderStageVertex
  deriving (Eq, Show, Read)

-- | Reflected input/output parameter.
data IOParam = IOParam
  { ioName :: !String
  , ioLocation :: !(Maybe Word32)
  , ioBuiltin :: !(Maybe String)
  , ioType :: !TypeLayout
  } deriving (Eq, Show, Read)

-- | Reflected IO for a single entry point.
data StageIO = StageIO
  { sioStage :: !ShaderStage
  , sioWorkgroupSize :: !(Maybe (Word32, Word32, Word32))
  , sioInputs :: ![IOParam]
  , sioOutputs :: ![IOParam]
  } deriving (Eq, Show, Read)

-- | Precomputed binding plan grouped by resource kind.
data BindingPlan = BindingPlan
  { bpBindings :: ![BindingInfo]
  , bpByGroup :: !(Map.Map Word32 [BindingInfo])
  , bpUniforms :: ![BindingInfo]
  , bpSamplers :: ![BindingInfo]
  , bpTextures :: ![BindingInfo]
  , bpStorageBuffers :: ![BindingInfo]
  , bpStorageTextures :: ![BindingInfo]
  } deriving (Eq, Show, Read)

-- Runtime interface representation

-- | Runtime binding metadata (name, kind, group, binding, layout).
data BindingInfo = BindingInfo
  { biName :: !String
  , biKind :: !BindingKind
  , biGroup :: !Word32
  , biBinding :: !Word32
  , biType :: !TypeLayout
  } deriving (Eq, Show, Read)

-- | Name-indexed binding map for quick lookup.
newtype BindingMap = BindingMap (Map.Map String BindingInfo)

-- | Build a binding map from a shader interface.
bindingMap :: ShaderInterface -> BindingMap
bindingMap iface =
  BindingMap (Map.fromList [(info.biName, info) | info <- iface.siBindings])

-- | Lookup a binding by name in a shader interface.
bindingInfoFor :: String -> ShaderInterface -> Either String BindingInfo
bindingInfoFor name iface =
  case bindingInfoForMap name (bindingMap iface) of
    Just info -> Right info
    Nothing -> Left ("binding not found in interface: " <> name)

-- | Lookup a binding by name in a binding map.
bindingInfoForMap :: String -> BindingMap -> Maybe BindingInfo
bindingInfoForMap name (BindingMap m) = Map.lookup name m

-- | Reflected override (specialization constant) metadata.
data OverrideInfo = OverrideInfo
  { oiName :: !String
  , oiId :: !(Maybe Word32)
  , oiSpecId :: !(Maybe Word32)
  , oiType :: !TypeLayout
  } deriving (Eq, Show, Read)

-- | Reflected shader interface (bindings, stage IO, overrides, push constants).
data ShaderInterface = ShaderInterface
  { siBindings :: ![BindingInfo]
  , siOverrides :: ![OverrideInfo]
  , siStageIO :: !(Maybe StageIO)
  , siPushConstants :: !(Maybe TypeLayout)
  , siSamplerMode :: !SamplerBindingMode
  } deriving (Eq, Show, Read)

-- | Overrides that can be specialized at runtime (have a spec ID).
specializableOverrides :: ShaderInterface -> [OverrideInfo]
specializableOverrides iface =
  filter (isJust . (.oiSpecId)) iface.siOverrides

-- | Access combined stage IO info (if any).
stageIO :: ShaderInterface -> Maybe StageIO
stageIO iface = iface.siStageIO

-- | Extract the shader stage from a reflected interface.
shaderStage :: ShaderInterface -> Maybe ShaderStage
shaderStage iface = (.sioStage) <$> iface.siStageIO

-- | Stage inputs for any entry point (empty if none).
stageInputs :: ShaderInterface -> [IOParam]
stageInputs iface = maybe [] (.sioInputs) iface.siStageIO

-- | Stage outputs for any entry point (empty if none).
stageOutputs :: ShaderInterface -> [IOParam]
stageOutputs iface = maybe [] (.sioOutputs) iface.siStageIO

-- | Vertex-stage inputs (empty for non-vertex shaders).
vertexInputs :: ShaderInterface -> [IOParam]
vertexInputs iface =
  case iface.siStageIO of
    Just sio | sio.sioStage == ShaderStageVertex -> sio.sioInputs
    _ -> []

-- | Vertex-stage outputs (empty for non-vertex shaders).
vertexOutputs :: ShaderInterface -> [IOParam]
vertexOutputs iface =
  case iface.siStageIO of
    Just sio | sio.sioStage == ShaderStageVertex -> sio.sioOutputs
    _ -> []

-- | Host-friendly vertex attribute formats.
data VertexFormat
  = VF16
  | VF16x2
  | VF16x3
  | VF16x4
  | VF32
  | VF32x2
  | VF32x3
  | VF32x4
  | VI32
  | VI32x2
  | VI32x3
  | VI32x4
  | VU32
  | VU32x2
  | VU32x3
  | VU32x4
  deriving (Eq, Show, Read)

-- | Reflected vertex attribute (name, location, format).
data VertexAttribute = VertexAttribute
  { vaName :: !String
  , vaLocation :: !Word32
  , vaFormat :: !VertexFormat
  } deriving (Eq, Show, Read)

-- | Extract vertex attributes from a vertex-stage interface.
vertexAttributes :: ShaderInterface -> Either String [VertexAttribute]
vertexAttributes iface =
  let attrs = mapMaybe toAttr (vertexInputs iface)
  in traverse buildAttr attrs
  where
    toAttr param = case param.ioLocation of
      Nothing -> Nothing
      Just loc -> Just (param, loc)

    buildAttr (param, loc) = do
      fmt <- vertexFormat param.ioType
      pure VertexAttribute
        { vaName = param.ioName
        , vaLocation = loc
        , vaFormat = fmt
        }

vertexFormat :: TypeLayout -> Either String VertexFormat
vertexFormat layout =
  case layout of
    TLScalar s _ _ -> scalarFormat s 1
    TLVector n s _ _ -> scalarFormat s n
    _ -> Left "vertex attributes must be scalar or vector types"

scalarFormat :: Scalar -> Int -> Either String VertexFormat
scalarFormat scalar n =
  case (scalar, n) of
    (F16, 1) -> Right VF16
    (F16, 2) -> Right VF16x2
    (F16, 3) -> Right VF16x3
    (F16, 4) -> Right VF16x4
    (F32, 1) -> Right VF32
    (F32, 2) -> Right VF32x2
    (F32, 3) -> Right VF32x3
    (F32, 4) -> Right VF32x4
    (I32, 1) -> Right VI32
    (I32, 2) -> Right VI32x2
    (I32, 3) -> Right VI32x3
    (I32, 4) -> Right VI32x4
    (U32, 1) -> Right VU32
    (U32, 2) -> Right VU32x2
    (U32, 3) -> Right VU32x3
    (U32, 4) -> Right VU32x4
    (Bool, _) -> Left "boolean vertex attributes are not supported"
    _ -> Left "unsupported vertex attribute width"

-- | Build a binding plan from the interface.
bindingPlan :: ShaderInterface -> BindingPlan
bindingPlan iface =
  let sorted =
        sortBy
          (comparing (.biGroup) <> comparing (.biBinding) <> comparing (.biName))
          iface.siBindings
      grouped = Map.fromListWith (<>) [(info.biGroup, [info]) | info <- sorted]
  in BindingPlan
        { bpBindings = sorted
        , bpByGroup = grouped
        , bpUniforms = filter (isUniformKind . (.biKind)) sorted
        , bpSamplers = filter (isSamplerKind . (.biKind)) sorted
        , bpTextures = filter (isTextureKind . (.biKind)) sorted
        , bpStorageBuffers = filter (isStorageBufferKind . (.biKind)) sorted
        , bpStorageTextures = filter (isStorageTextureKind . (.biKind)) sorted
        }

-- | Check if a binding kind is a uniform.
isUniformKind :: BindingKind -> Bool
isUniformKind BUniform = True
isUniformKind _ = False

-- | Check if a binding kind is a sampler.
isSamplerKind :: BindingKind -> Bool
isSamplerKind kind = kind == BSampler || kind == BSamplerComparison

-- | Check if a binding kind is a texture.
isTextureKind :: BindingKind -> Bool
isTextureKind kind =
  kind `elem`
    [ BTexture1D
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
    ]

-- | Check if a binding kind is a storage buffer.
isStorageBufferKind :: BindingKind -> Bool
isStorageBufferKind kind = kind == BStorageRead || kind == BStorageReadWrite

-- | Check if a binding kind is a storage texture.
isStorageTextureKind :: BindingKind -> Bool
isStorageTextureKind kind =
  kind `elem` [BStorageTexture1D, BStorageTexture2D, BStorageTexture2DArray, BStorageTexture3D]

-- | Push-constant layout if the shader declares one.
pushConstantLayout :: ShaderInterface -> Maybe TypeLayout
pushConstantLayout iface = iface.siPushConstants

-- | Compiled shader with a type-level interface description and sampler mode.
data CompiledShader (mode :: SamplerBindingMode) (iface :: [Binding]) = CompiledShader
  { shaderSpirv :: ByteString
  , shaderInterface :: ShaderInterface
  }

-- | Existential wrapper for runtime compilation output.
data SomeCompiledShader = forall mode iface. SomeCompiledShader (CompiledShader mode iface)

-- | Prepared shader with cached stage and binding plan.
data PreparedShader (mode :: SamplerBindingMode) iface = PreparedShader
  { psShader :: CompiledShader mode iface
  , psStage :: ShaderStage
  , psPlan :: BindingPlan
  , psVertexAttributes :: Maybe [VertexAttribute]
  }

-- | Existential wrapper for prepared shaders.
data SomePreparedShader = forall mode iface. SomePreparedShader (PreparedShader mode iface)

-- | Prepare a compiled shader for host use (stage + plan + vertex attrs).
prepareShader :: CompiledShader mode iface -> Either String (PreparedShader mode iface)
prepareShader shader =
  case shaderStage shader.shaderInterface of
    Nothing -> Left "shader has no entry point"
    Just stage -> do
      let iface = shader.shaderInterface
      vattrs <- case stage of
        ShaderStageVertex -> Just <$> vertexAttributes iface
        _ -> Right Nothing
      pure PreparedShader
        { psShader = shader
        , psStage = stage
        , psPlan = bindingPlan iface
        , psVertexAttributes = vattrs
        }

-- | SPIR-V bytes for a prepared shader.
preparedSpirv :: PreparedShader mode iface -> ByteString
preparedSpirv prep = prep.psShader.shaderSpirv

-- | Reflected interface for a prepared shader.
preparedInterface :: PreparedShader mode iface -> ShaderInterface
preparedInterface prep = prep.psShader.shaderInterface

-- | Cached stage for a prepared shader.
preparedStage :: PreparedShader mode iface -> ShaderStage
preparedStage prep = prep.psStage

-- | Cached binding plan for a prepared shader.
preparedPlan :: PreparedShader mode iface -> BindingPlan
preparedPlan prep = prep.psPlan

-- | Cached vertex attributes for vertex shaders.
preparedVertexAttributes :: PreparedShader mode iface -> Maybe [VertexAttribute]
preparedVertexAttributes prep = prep.psVertexAttributes

-- | SPIR-V bytes for an existential prepared shader.
somePreparedSpirv :: SomePreparedShader -> ByteString
somePreparedSpirv (SomePreparedShader prep) = preparedSpirv prep

-- | Interface for an existential prepared shader.
somePreparedInterface :: SomePreparedShader -> ShaderInterface
somePreparedInterface (SomePreparedShader prep) = preparedInterface prep
