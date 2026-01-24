{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typed, host-agnostic shader input lists.
module Spirdo.Wesl.Inputs
  ( HList(..)
  , InputFor
  , InputsOf
  , SamplerHandle(..)
  , TextureHandle(..)
  , BufferHandle(..)
  , UniformInput(..)
  , SamplerInput(..)
  , TextureInput(..)
  , StorageBufferInput(..)
  , StorageTextureInput(..)
  , ShaderInputs(..)
  , emptyInputs
  , inputsFor
  ) where

import Data.ByteString (ByteString)
import qualified Data.Kind as K
import Data.Proxy (Proxy(..))
import Data.Word (Word32, Word64)
import GHC.TypeLits (KnownSymbol, symbolVal)

import Spirdo.Wesl.Types
  ( Binding(..)
  , BindingInfo(..)
  , BindingKind(..)
  , BindingMap
  , CompiledShader(..)
  , UniformValue
  , bindingInfoForMap
  , bindingMap
  , packUniform
  )

data HList (xs :: [K.Type]) where
  HNil :: HList '[]
  (:&) :: x -> HList xs -> HList (x ': xs)

infixr 5 :&

data BindingCategory
  = CatUniform
  | CatSampler
  | CatTexture
  | CatStorageBuffer
  | CatStorageTexture

type family CategoryOf (k :: BindingKind) :: BindingCategory where
  CategoryOf 'BUniform = 'CatUniform
  CategoryOf 'BSampler = 'CatSampler
  CategoryOf 'BSamplerComparison = 'CatSampler
  CategoryOf 'BStorageRead = 'CatStorageBuffer
  CategoryOf 'BStorageReadWrite = 'CatStorageBuffer
  CategoryOf 'BStorageTexture1D = 'CatStorageTexture
  CategoryOf 'BStorageTexture2D = 'CatStorageTexture
  CategoryOf 'BStorageTexture2DArray = 'CatStorageTexture
  CategoryOf 'BStorageTexture3D = 'CatStorageTexture
  CategoryOf _ = 'CatTexture

type family InputForCategory (c :: BindingCategory) :: K.Type where
  InputForCategory 'CatUniform = UniformValue
  InputForCategory 'CatSampler = SamplerHandle
  InputForCategory 'CatTexture = TextureHandle
  InputForCategory 'CatStorageBuffer = BufferHandle
  InputForCategory 'CatStorageTexture = TextureHandle

type family InputForKind (k :: BindingKind) :: K.Type where
  InputForKind k = InputForCategory (CategoryOf k)

type family InputFor (b :: Binding) :: K.Type where
  InputFor ('Binding _ kind _ _ _) = InputForKind kind

type family InputsOf (iface :: [Binding]) :: [K.Type] where
  InputsOf '[] = '[]
  InputsOf (b ': bs) = InputFor b ': InputsOf bs

newtype SamplerHandle = SamplerHandle Word64
  deriving (Eq, Show)

newtype TextureHandle = TextureHandle Word64
  deriving (Eq, Show)

newtype BufferHandle = BufferHandle Word64
  deriving (Eq, Show)

data UniformInput = UniformInput
  { uiName :: !String
  , uiGroup :: !Word32
  , uiBinding :: !Word32
  , uiBytes :: !ByteString
  } deriving (Eq, Show)

data SamplerInput = SamplerInput
  { samplerName :: !String
  , samplerGroup :: !Word32
  , samplerBinding :: !Word32
  , samplerHandle :: !SamplerHandle
  } deriving (Eq, Show)

data TextureInput = TextureInput
  { textureName :: !String
  , textureGroup :: !Word32
  , textureBinding :: !Word32
  , textureHandle :: !TextureHandle
  } deriving (Eq, Show)

data StorageBufferInput = StorageBufferInput
  { storageBufferName :: !String
  , storageBufferGroup :: !Word32
  , storageBufferBinding :: !Word32
  , storageBufferHandle :: !BufferHandle
  } deriving (Eq, Show)

data StorageTextureInput = StorageTextureInput
  { storageTextureName :: !String
  , storageTextureGroup :: !Word32
  , storageTextureBinding :: !Word32
  , storageTextureHandle :: !TextureHandle
  } deriving (Eq, Show)

data ShaderInputs (iface :: [Binding]) = ShaderInputs
  { siShader :: !(CompiledShader iface)
  , siUniforms :: ![UniformInput]
  , siSamplers :: ![SamplerInput]
  , siTextures :: ![TextureInput]
  , siStorageBuffers :: ![StorageBufferInput]
  , siStorageTextures :: ![StorageTextureInput]
  }

emptyInputs :: CompiledShader iface -> ShaderInputs iface
emptyInputs shader =
  ShaderInputs
    { siShader = shader
    , siUniforms = []
    , siSamplers = []
    , siTextures = []
    , siStorageBuffers = []
    , siStorageTextures = []
    }

class ApplyCategory (c :: BindingCategory) where
  applyCategory :: BindingInfo -> InputForCategory c -> ShaderInputs iface -> Either String (ShaderInputs iface)

instance ApplyCategory 'CatUniform where
  applyCategory info val inputs =
    case packUniform (biType info) val of
      Left err -> Left err
      Right bytes ->
        let entry =
              UniformInput
                { uiName = biName info
                , uiGroup = biGroup info
                , uiBinding = biBinding info
                , uiBytes = bytes
                }
        in Right inputs { siUniforms = entry : siUniforms inputs }

instance ApplyCategory 'CatSampler where
  applyCategory info handle inputs =
    let entry =
          SamplerInput
            { samplerName = biName info
            , samplerGroup = biGroup info
            , samplerBinding = biBinding info
            , samplerHandle = handle
            }
    in Right inputs { siSamplers = entry : siSamplers inputs }

instance ApplyCategory 'CatTexture where
  applyCategory info handle inputs =
    let entry =
          TextureInput
            { textureName = biName info
            , textureGroup = biGroup info
            , textureBinding = biBinding info
            , textureHandle = handle
            }
    in Right inputs { siTextures = entry : siTextures inputs }

instance ApplyCategory 'CatStorageBuffer where
  applyCategory info handle inputs =
    let entry =
          StorageBufferInput
            { storageBufferName = biName info
            , storageBufferGroup = biGroup info
            , storageBufferBinding = biBinding info
            , storageBufferHandle = handle
            }
    in Right inputs { siStorageBuffers = entry : siStorageBuffers inputs }

instance ApplyCategory 'CatStorageTexture where
  applyCategory info handle inputs =
    let entry =
          StorageTextureInput
            { storageTextureName = biName info
            , storageTextureGroup = biGroup info
            , storageTextureBinding = biBinding info
            , storageTextureHandle = handle
            }
    in Right inputs { siStorageTextures = entry : siStorageTextures inputs }

class ApplyKind (k :: BindingKind) where
  applyKind :: BindingInfo -> InputForKind k -> ShaderInputs iface -> Either String (ShaderInputs iface)

instance ApplyCategory (CategoryOf k) => ApplyKind k where
  applyKind = applyCategory @(CategoryOf k)

class BuildInputsWith (iface :: [Binding]) (bs :: [Binding]) where
  buildInputsWith :: BindingMap -> CompiledShader iface -> HList (InputsOf bs) -> Either String (ShaderInputs iface)

instance BuildInputsWith iface '[] where
  buildInputsWith _ shader HNil = Right (emptyInputs shader)

instance
  forall iface name kind set binding ty bs.
  ( KnownSymbol name
  , ApplyKind kind
  , BuildInputsWith iface bs
  ) =>
  BuildInputsWith iface ('Binding name kind set binding ty ': bs)
  where
  buildInputsWith bmap shader (x :& xs) =
    case buildInputsWith @iface @bs bmap shader xs of
      Left err -> Left err
      Right baseInputs ->
        let name = symbolVal (Proxy @name)
        in case bindingInfoForMap name bmap of
            Nothing -> Left ("binding not found in interface: " <> name)
            Just info ->
              case applyKind @kind info x baseInputs of
                Left err -> Left ("binding " <> name <> ": " <> err)
                Right ok -> Right ok

inputsFor :: forall iface. BuildInputsWith iface iface => CompiledShader iface -> HList (InputsOf iface) -> Either String (ShaderInputs iface)
inputsFor shader xs =
  let bmap = bindingMap (shaderInterface shader)
  in buildInputsWith @iface @iface bmap shader xs
