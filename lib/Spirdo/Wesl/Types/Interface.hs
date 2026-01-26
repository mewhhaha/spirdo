{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level interface and reflection types.
module Spirdo.Wesl.Types.Interface
  ( BindingKind(..)
  , ScalarType(..)
  , Field(..)
  , Ty(..)
  , Binding(..)
  , BindingDesc(..)
  , ShaderStage(..)
  , IOParam(..)
  , StageIO(..)
  , BindingPlan(..)
  , ReflectBindings(..)
  , HasBinding
  , binding
  , shaderStage
  , VertexShader(..)
  , FragmentShader(..)
  , ComputeShader(..)
  , asVertexShader
  , asFragmentShader
  , asComputeShader
  , PreparedShader(..)
  , prepareShader
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
  , samplerBindings
  , uniformBindings
  , storageBufferBindings
  , storageTextureBindings
  ) where

import Data.ByteString (ByteString)
import Data.List (find, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Word (Word32)
import Data.Ord (comparing)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)

import Spirdo.Wesl.Types.Layout
  ( StorageAccess(..)
  , StorageFormat(..)
  , Scalar(..)
  , TypeLayout(..)
  )

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

data ShaderStage
  = ShaderStageCompute
  | ShaderStageFragment
  | ShaderStageVertex
  deriving (Eq, Show, Read)

data IOParam = IOParam
  { ioName :: !String
  , ioLocation :: !(Maybe Word32)
  , ioBuiltin :: !(Maybe String)
  , ioType :: !TypeLayout
  } deriving (Eq, Show, Read)

data StageIO = StageIO
  { sioStage :: !ShaderStage
  , sioWorkgroupSize :: !(Maybe (Word32, Word32, Word32))
  , sioInputs :: ![IOParam]
  , sioOutputs :: ![IOParam]
  } deriving (Eq, Show, Read)

data BindingPlan = BindingPlan
  { bpBindings :: ![BindingInfo]
  , bpByGroup :: !(Map.Map Word32 [BindingInfo])
  , bpUniforms :: ![BindingInfo]
  , bpSamplers :: ![BindingInfo]
  , bpTextures :: ![BindingInfo]
  , bpStorageBuffers :: ![BindingInfo]
  , bpStorageTextures :: ![BindingInfo]
  } deriving (Eq, Show, Read)

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


-- Runtime interface representation

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

data OverrideInfo = OverrideInfo
  { oiName :: !String
  , oiId :: !(Maybe Word32)
  , oiSpecId :: !(Maybe Word32)
  , oiType :: !TypeLayout
  } deriving (Eq, Show, Read)

data ShaderInterface = ShaderInterface
  { siBindings :: ![BindingInfo]
  , siOverrides :: ![OverrideInfo]
  , siStageIO :: !(Maybe StageIO)
  , siPushConstants :: !(Maybe TypeLayout)
  } deriving (Eq, Show, Read)

specializableOverrides :: ShaderInterface -> [OverrideInfo]
specializableOverrides iface =
  filter (isJust . oiSpecId) (siOverrides iface)

stageIO :: ShaderInterface -> Maybe StageIO
stageIO = siStageIO

shaderStage :: ShaderInterface -> Maybe ShaderStage
shaderStage iface = sioStage <$> siStageIO iface

stageInputs :: ShaderInterface -> [IOParam]
stageInputs iface = maybe [] sioInputs (siStageIO iface)

stageOutputs :: ShaderInterface -> [IOParam]
stageOutputs iface = maybe [] sioOutputs (siStageIO iface)

vertexInputs :: ShaderInterface -> [IOParam]
vertexInputs iface =
  case siStageIO iface of
    Just sio | sioStage sio == ShaderStageVertex -> sioInputs sio
    _ -> []

vertexOutputs :: ShaderInterface -> [IOParam]
vertexOutputs iface =
  case siStageIO iface of
    Just sio | sioStage sio == ShaderStageVertex -> sioOutputs sio
    _ -> []

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

data VertexAttribute = VertexAttribute
  { vaName :: !String
  , vaLocation :: !Word32
  , vaFormat :: !VertexFormat
  } deriving (Eq, Show, Read)

vertexAttributes :: ShaderInterface -> Either String [VertexAttribute]
vertexAttributes iface =
  let attrs = mapMaybe toAttr (vertexInputs iface)
  in traverse buildAttr attrs
  where
    toAttr param = case ioLocation param of
      Nothing -> Nothing
      Just loc -> Just (param, loc)

    buildAttr (param, loc) = do
      fmt <- vertexFormat (ioType param)
      pure VertexAttribute
        { vaName = ioName param
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

bindingPlan :: ShaderInterface -> BindingPlan
bindingPlan iface =
  let sorted =
        sortBy
          (comparing biGroup <> comparing biBinding <> comparing biName)
          (siBindings iface)
      grouped = Map.fromListWith (<>) [(biGroup info, [info]) | info <- sorted]
  in BindingPlan
        { bpBindings = sorted
        , bpByGroup = grouped
        , bpUniforms = filter (isUniformKind . biKind) sorted
        , bpSamplers = filter (isSamplerKind . biKind) sorted
        , bpTextures = filter (isTextureKind . biKind) sorted
        , bpStorageBuffers = filter (isStorageBufferKind . biKind) sorted
        , bpStorageTextures = filter (isStorageTextureKind . biKind) sorted
        }

isUniformKind :: BindingKind -> Bool
isUniformKind BUniform = True
isUniformKind _ = False

isSamplerKind :: BindingKind -> Bool
isSamplerKind kind = kind == BSampler || kind == BSamplerComparison

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

isStorageBufferKind :: BindingKind -> Bool
isStorageBufferKind kind = kind == BStorageRead || kind == BStorageReadWrite

isStorageTextureKind :: BindingKind -> Bool
isStorageTextureKind kind =
  kind `elem` [BStorageTexture1D, BStorageTexture2D, BStorageTexture2DArray, BStorageTexture3D]

pushConstantLayout :: ShaderInterface -> Maybe TypeLayout
pushConstantLayout = siPushConstants

-- | Compiled shader with a type-level interface description.
data CompiledShader (iface :: [Binding]) = CompiledShader
  { shaderSpirv :: ByteString
  , shaderInterface :: ShaderInterface
  }

-- | Existential wrapper for runtime compilation.
data SomeCompiledShader = forall iface. SomeCompiledShader (CompiledShader iface)

newtype VertexShader iface = VertexShader { unVertexShader :: CompiledShader iface }
newtype FragmentShader iface = FragmentShader { unFragmentShader :: CompiledShader iface }
newtype ComputeShader iface = ComputeShader { unComputeShader :: CompiledShader iface }

asVertexShader :: CompiledShader iface -> Either String (VertexShader iface)
asVertexShader shader =
  case shaderStage (shaderInterface shader) of
    Just ShaderStageVertex -> Right (VertexShader shader)
    Just ShaderStageFragment -> Left "expected vertex shader, found fragment shader"
    Just ShaderStageCompute -> Left "expected vertex shader, found compute shader"
    Nothing -> Left "expected vertex shader, but shader has no entry point"

asFragmentShader :: CompiledShader iface -> Either String (FragmentShader iface)
asFragmentShader shader =
  case shaderStage (shaderInterface shader) of
    Just ShaderStageFragment -> Right (FragmentShader shader)
    Just ShaderStageVertex -> Left "expected fragment shader, found vertex shader"
    Just ShaderStageCompute -> Left "expected fragment shader, found compute shader"
    Nothing -> Left "expected fragment shader, but shader has no entry point"

asComputeShader :: CompiledShader iface -> Either String (ComputeShader iface)
asComputeShader shader =
  case shaderStage (shaderInterface shader) of
    Just ShaderStageCompute -> Right (ComputeShader shader)
    Just ShaderStageVertex -> Left "expected compute shader, found vertex shader"
    Just ShaderStageFragment -> Left "expected compute shader, found fragment shader"
    Nothing -> Left "expected compute shader, but shader has no entry point"

data PreparedShader iface = PreparedShader
  { psShader :: CompiledShader iface
  , psStage :: ShaderStage
  , psPlan :: BindingPlan
  , psVertexAttributes :: Maybe [VertexAttribute]
  }

prepareShader :: CompiledShader iface -> Either String (PreparedShader iface)
prepareShader shader =
  case shaderStage (shaderInterface shader) of
    Nothing -> Left "shader has no entry point"
    Just stage -> do
      let iface = shaderInterface shader
      vattrs <- case stage of
        ShaderStageVertex -> Just <$> vertexAttributes iface
        _ -> Right Nothing
      pure PreparedShader
        { psShader = shader
        , psStage = stage
        , psPlan = bindingPlan iface
        , psVertexAttributes = vattrs
        }
