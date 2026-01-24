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
  , ReflectBindings(..)
  , HasBinding
  , binding
  , bindingMaybe
  , bindingEither
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
  , samplerBindingsFor
  , uniformBindingsFor
  , storageBufferBindingsFor
  , storageTextureBindingsFor
  ) where

import Data.ByteString (ByteString)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.Word (Word32)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)

import Spirdo.Wesl.Types.Layout
  ( StorageAccess(..)
  , StorageFormat(..)
  , TypeLayout
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
binding shader =
  case bindingEither @name shader of
    Right b -> b
    Left err -> error ("binding: " <> err <> " (impossible)")

bindingMaybe :: forall name iface. (KnownSymbol name, HasBinding name iface ~ 'True, ReflectBindings iface) => CompiledShader iface -> Maybe BindingDesc
bindingMaybe _ =
  let key = symbolVal (Proxy @name)
  in find (\b -> descName b == key) (reflectBindings (Proxy @iface))

bindingEither :: forall name iface. (KnownSymbol name, HasBinding name iface ~ 'True, ReflectBindings iface) => CompiledShader iface -> Either String BindingDesc
bindingEither shader =
  case bindingMaybe @name shader of
    Just b -> Right b
    Nothing -> Left ("missing " <> symbolVal (Proxy @name))

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
