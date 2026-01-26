{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
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
  , StorageTextureHandle(..)
  , UniformInput(..)
  , SamplerInput(..)
  , TextureInput(..)
  , StorageBufferInput(..)
  , StorageTextureInput(..)
  , UniformSlot(..)
  , uniformSlots
  , InputsBuilder
  , uniform
  , sampler
  , texture
  , storageBuffer
  , storageTexture
  , uniforms
  , samplers
  , textures
  , storageBuffers
  , storageTextures
  , inputsFrom
  , inputsFromPrepared
  , ShaderInputs(..)
  , emptyInputs
  , orderedUniforms
  , inputsForPrepared
  , inputsFor
  ) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.List (sortOn)
import qualified Data.Kind as K
import Data.Proxy (Proxy(..))
import Data.Word (Word32, Word64)
import GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

import Spirdo.Wesl.Types
  ( Binding(..)
  , BindingInfo(..)
  , BindingKind(..)
  , BindingMap
  , CompiledShader(..)
  , PreparedShader(..)
  , UniformValue
  , ToUniform(..)
  , bindingInfoForMap
  , bindingMap
  , isSamplerKind
  , isStorageBufferKind
  , isStorageTextureKind
  , isTextureKind
  , isUniformKind
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
  InputForCategory 'CatStorageTexture = StorageTextureHandle

type family InputForKind (k :: BindingKind) :: K.Type where
  InputForKind k = InputForCategory (CategoryOf k)

type family InputFor (b :: Binding) :: K.Type where
  InputFor ('Binding _ kind _ _ _) = InputForKind kind

type family InputsOf (iface :: [Binding]) :: [K.Type] where
  InputsOf '[] = '[]
  InputsOf (b ': bs) = InputFor b ': InputsOf bs

type family RequireBinding (name :: Symbol) (iface :: [Binding]) :: K.Constraint where
  RequireBinding name '[] =
    TypeError ('Text "binding not found: " ':<>: 'ShowType name)
  RequireBinding name ('Binding name _ _ _ _ ': _) = ()
  RequireBinding name (_ ': rest) = RequireBinding name rest

type family RequireUniform (name :: Symbol) (iface :: [Binding]) :: K.Constraint where
  RequireUniform name '[] =
    TypeError ('Text "uniform binding not found: " ':<>: 'ShowType name)
  RequireUniform name ('Binding name 'BUniform _ _ _ ': _) = ()
  RequireUniform name ('Binding name kind _ _ _ ': _) =
    TypeError
      ( 'Text "binding "
          ':<>: 'ShowType name
          ':<>: 'Text " is not a uniform (kind: "
          ':<>: 'ShowType kind
          ':<>: 'Text ")"
      )
  RequireUniform name (_ ': rest) = RequireUniform name rest

type family RequireSampler (name :: Symbol) (iface :: [Binding]) :: K.Constraint where
  RequireSampler name '[] =
    TypeError ('Text "sampler binding not found: " ':<>: 'ShowType name)
  RequireSampler name ('Binding name 'BSampler _ _ _ ': _) = ()
  RequireSampler name ('Binding name 'BSamplerComparison _ _ _ ': _) = ()
  RequireSampler name ('Binding name kind _ _ _ ': _) =
    TypeError
      ( 'Text "binding "
          ':<>: 'ShowType name
          ':<>: 'Text " is not a sampler (kind: "
          ':<>: 'ShowType kind
          ':<>: 'Text ")"
      )
  RequireSampler name (_ ': rest) = RequireSampler name rest

type family RequireTexture (name :: Symbol) (iface :: [Binding]) :: K.Constraint where
  RequireTexture name '[] =
    TypeError ('Text "texture binding not found: " ':<>: 'ShowType name)
  RequireTexture name ('Binding name kind _ _ _ ': _) =
    IfTextureKind name kind
  RequireTexture name (_ ': rest) = RequireTexture name rest

type family IfTextureKind (name :: Symbol) (kind :: BindingKind) :: K.Constraint where
  IfTextureKind _ 'BTexture1D = ()
  IfTextureKind _ 'BTexture1DArray = ()
  IfTextureKind _ 'BTexture2D = ()
  IfTextureKind _ 'BTexture2DArray = ()
  IfTextureKind _ 'BTexture3D = ()
  IfTextureKind _ 'BTextureCube = ()
  IfTextureKind _ 'BTextureCubeArray = ()
  IfTextureKind _ 'BTextureMultisampled2D = ()
  IfTextureKind _ 'BTextureDepth2D = ()
  IfTextureKind _ 'BTextureDepth2DArray = ()
  IfTextureKind _ 'BTextureDepthCube = ()
  IfTextureKind _ 'BTextureDepthCubeArray = ()
  IfTextureKind _ 'BTextureDepthMultisampled2D = ()
  IfTextureKind name kind =
    TypeError
      ( 'Text "binding "
          ':<>: 'ShowType name
          ':<>: 'Text " is not a texture (kind: "
          ':<>: 'ShowType kind
          ':<>: 'Text ")"
      )

type family RequireStorageBuffer (name :: Symbol) (iface :: [Binding]) :: K.Constraint where
  RequireStorageBuffer name '[] =
    TypeError ('Text "storage buffer binding not found: " ':<>: 'ShowType name)
  RequireStorageBuffer name ('Binding name 'BStorageRead _ _ _ ': _) = ()
  RequireStorageBuffer name ('Binding name 'BStorageReadWrite _ _ _ ': _) = ()
  RequireStorageBuffer name ('Binding name kind _ _ _ ': _) =
    TypeError
      ( 'Text "binding "
          ':<>: 'ShowType name
          ':<>: 'Text " is not a storage buffer (kind: "
          ':<>: 'ShowType kind
          ':<>: 'Text ")"
      )
  RequireStorageBuffer name (_ ': rest) = RequireStorageBuffer name rest

type family RequireStorageTexture (name :: Symbol) (iface :: [Binding]) :: K.Constraint where
  RequireStorageTexture name '[] =
    TypeError ('Text "storage texture binding not found: " ':<>: 'ShowType name)
  RequireStorageTexture name ('Binding name 'BStorageTexture1D _ _ _ ': _) = ()
  RequireStorageTexture name ('Binding name 'BStorageTexture2D _ _ _ ': _) = ()
  RequireStorageTexture name ('Binding name 'BStorageTexture2DArray _ _ _ ': _) = ()
  RequireStorageTexture name ('Binding name 'BStorageTexture3D _ _ _ ': _) = ()
  RequireStorageTexture name ('Binding name kind _ _ _ ': _) =
    TypeError
      ( 'Text "binding "
          ':<>: 'ShowType name
          ':<>: 'Text " is not a storage texture (kind: "
          ':<>: 'ShowType kind
          ':<>: 'Text ")"
      )
  RequireStorageTexture name (_ ': rest) = RequireStorageTexture name rest

newtype SamplerHandle = SamplerHandle Word64
  deriving (Eq, Show)

newtype TextureHandle = TextureHandle Word64
  deriving (Eq, Show)

newtype BufferHandle = BufferHandle Word64
  deriving (Eq, Show)

newtype StorageTextureHandle = StorageTextureHandle Word64
  deriving (Eq, Show)

data UniformInput = UniformInput
  { uiName :: !String
  , uiGroup :: !Word32
  , uiBinding :: !Word32
  , uiBytes :: !ByteString
  } deriving (Eq, Show)

data UniformSlot = UniformSlot
  { usGroup :: !Word32
  , usBinding :: !Word32
  , usBytes :: !ByteString
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
  , storageTextureHandle :: !StorageTextureHandle
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

orderedUniforms :: ShaderInputs iface -> [UniformInput]
orderedUniforms inputs =
  orderUniforms (siUniforms inputs)

uniformSlots :: ShaderInputs iface -> [UniformSlot]
uniformSlots inputs =
  [ UniformSlot (uiGroup u) (uiBinding u) (uiBytes u)
  | u <- orderedUniforms inputs
  ]

normalizeInputs :: ShaderInputs iface -> ShaderInputs iface
normalizeInputs inputs =
  inputs
    { siUniforms = orderUniforms (siUniforms inputs)
    , siSamplers = orderSamplers (siSamplers inputs)
    , siTextures = orderTextures (siTextures inputs)
    , siStorageBuffers = orderStorageBuffers (siStorageBuffers inputs)
    , siStorageTextures = orderStorageTextures (siStorageTextures inputs)
    }

orderUniforms :: [UniformInput] -> [UniformInput]
orderUniforms = sortOn (\u -> (uiGroup u, uiBinding u, uiName u))

orderSamplers :: [SamplerInput] -> [SamplerInput]
orderSamplers = sortOn (\s -> (samplerGroup s, samplerBinding s, samplerName s))

orderTextures :: [TextureInput] -> [TextureInput]
orderTextures = sortOn (\t -> (textureGroup t, textureBinding t, textureName t))

orderStorageBuffers :: [StorageBufferInput] -> [StorageBufferInput]
orderStorageBuffers = sortOn (\b -> (storageBufferGroup b, storageBufferBinding b, storageBufferName b))

orderStorageTextures :: [StorageTextureInput] -> [StorageTextureInput]
orderStorageTextures = sortOn (\t -> (storageTextureGroup t, storageTextureBinding t, storageTextureName t))

data InputItem
  = InputUniform !String !UniformValue
  | InputSampler !String !SamplerHandle
  | InputTexture !String !TextureHandle
  | InputStorageBuffer !String !BufferHandle
  | InputStorageTexture !String !StorageTextureHandle
  deriving (Eq, Show)

newtype InputsBuilder (iface :: [Binding]) = InputsBuilder [InputItem]
  deriving (Eq, Show)

instance Semigroup (InputsBuilder iface) where
  InputsBuilder a <> InputsBuilder b = InputsBuilder (a <> b)

instance Monoid (InputsBuilder iface) where
  mempty = InputsBuilder []

uniform :: forall name iface a. (KnownSymbol name, RequireUniform name iface, ToUniform a) => a -> InputsBuilder iface
uniform val = InputsBuilder [InputUniform (symbolVal (Proxy @name)) (toUniform val)]

sampler :: forall name iface. (KnownSymbol name, RequireSampler name iface) => SamplerHandle -> InputsBuilder iface
sampler handle = InputsBuilder [InputSampler (symbolVal (Proxy @name)) handle]

texture :: forall name iface. (KnownSymbol name, RequireTexture name iface) => TextureHandle -> InputsBuilder iface
texture handle = InputsBuilder [InputTexture (symbolVal (Proxy @name)) handle]

storageBuffer :: forall name iface. (KnownSymbol name, RequireStorageBuffer name iface) => BufferHandle -> InputsBuilder iface
storageBuffer handle = InputsBuilder [InputStorageBuffer (symbolVal (Proxy @name)) handle]

storageTexture :: forall name iface. (KnownSymbol name, RequireStorageTexture name iface) => StorageTextureHandle -> InputsBuilder iface
storageTexture handle = InputsBuilder [InputStorageTexture (symbolVal (Proxy @name)) handle]

uniforms :: InputsBuilder iface -> InputsBuilder iface
uniforms = id

samplers :: InputsBuilder iface -> InputsBuilder iface
samplers = id

textures :: InputsBuilder iface -> InputsBuilder iface
textures = id

storageBuffers :: InputsBuilder iface -> InputsBuilder iface
storageBuffers = id

storageTextures :: InputsBuilder iface -> InputsBuilder iface
storageTextures = id

inputsFrom :: forall iface. CompiledShader iface -> InputsBuilder iface -> Either String (ShaderInputs iface)
inputsFrom shader (InputsBuilder items) =
  let bmap = bindingMap (shaderInterface shader)
      initInputs = emptyInputs shader
  in fmap normalizeInputs (foldM (applyItem bmap) initInputs items)

inputsFromPrepared :: forall iface. PreparedShader iface -> InputsBuilder iface -> Either String (ShaderInputs iface)
inputsFromPrepared prepared inputs =
  inputsFrom (psShader prepared) inputs

applyItem :: BindingMap -> ShaderInputs iface -> InputItem -> Either String (ShaderInputs iface)
applyItem bmap inputs item = do
  let name = itemName item
  info <- case bindingInfoForMap name bmap of
    Nothing -> Left ("binding not found in interface: " <> name)
    Just bi -> Right bi
  case (item, biKind info) of
    (InputUniform _ val, kind)
      | isUniformKind kind ->
          case packUniform (biType info) val of
            Left err -> Left ("binding " <> name <> ": " <> err)
            Right bytes ->
              Right inputs
                { siUniforms =
                    UniformInput
                      { uiName = biName info
                      , uiGroup = biGroup info
                      , uiBinding = biBinding info
                      , uiBytes = bytes
                      }
                      : siUniforms inputs
                }
    (InputSampler _ handle, kind)
      | isSamplerKind kind ->
          Right inputs
            { siSamplers =
                SamplerInput
                  { samplerName = biName info
                  , samplerGroup = biGroup info
                  , samplerBinding = biBinding info
                  , samplerHandle = handle
                  }
                  : siSamplers inputs
            }
    (InputTexture _ handle, kind)
      | isTextureKind kind ->
          Right inputs
            { siTextures =
                TextureInput
                  { textureName = biName info
                  , textureGroup = biGroup info
                  , textureBinding = biBinding info
                  , textureHandle = handle
                  }
                  : siTextures inputs
            }
    (InputStorageBuffer _ handle, kind)
      | isStorageBufferKind kind ->
          Right inputs
            { siStorageBuffers =
                StorageBufferInput
                  { storageBufferName = biName info
                  , storageBufferGroup = biGroup info
                  , storageBufferBinding = biBinding info
                  , storageBufferHandle = handle
                  }
                  : siStorageBuffers inputs
            }
    (InputStorageTexture _ handle, kind)
      | isStorageTextureKind kind ->
          Right inputs
            { siStorageTextures =
                StorageTextureInput
                  { storageTextureName = biName info
                  , storageTextureGroup = biGroup info
                  , storageTextureBinding = biBinding info
                  , storageTextureHandle = handle
                  }
                  : siStorageTextures inputs
            }
    _ -> Left ("binding " <> name <> ": kind mismatch")

itemName :: InputItem -> String
itemName item =
  case item of
    InputUniform name _ -> name
    InputSampler name _ -> name
    InputTexture name _ -> name
    InputStorageBuffer name _ -> name
    InputStorageTexture name _ -> name

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
  in fmap normalizeInputs (buildInputsWith @iface @iface bmap shader xs)

inputsForPrepared :: forall iface. BuildInputsWith iface iface => PreparedShader iface -> HList (InputsOf iface) -> Either String (ShaderInputs iface)
inputsForPrepared prepared xs =
  inputsFor (psShader prepared) xs
