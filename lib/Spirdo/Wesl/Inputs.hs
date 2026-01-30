{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typed, host-agnostic shader input lists.
module Spirdo.Wesl.Inputs
  ( SamplerHandle(..)
  , TextureHandle(..)
  , BufferHandle(..)
  , StorageTextureHandle(..)
  , RequireBinding
  , RequireUniform
  , RequireSampler
  , RequireTexture
  , RequireStorageBuffer
  , RequireStorageTexture
  , InputsError(..)
  , UniformInput(..)
  , SamplerInput(..)
  , TextureInput(..)
  , StorageBufferInput(..)
  , StorageTextureInput(..)
  , InputsCombined
  , InputsSeparate
  , InputsBuilder
  , uniform
  , sampler
  , sampledTexture
  , texture
  , storageBuffer
  , storageTexture
  , inputsFor
  , ShaderInputs
  , inputsInterface
  , inputsUniforms
  , inputsSamplers
  , inputsTextures
  , inputsStorageBuffers
  , inputsStorageTextures
  , emptyInputs
  , orderedUniforms
  ) where

import Control.Monad (foldM, when)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.List (intercalate, sortOn)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Kind as K
import Data.Proxy (Proxy(..))
import Data.Word (Word32, Word64)
import GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

import Spirdo.Wesl.Types
  ( Binding(..)
  , BindingInfo(..)
  , BindingKind(..)
  , BindingMap
  , Shader(..)
  , shaderInterface
  , ShaderInterface(..)
  , SamplerBindingMode(..)
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

-- | Compile-time requirement that a binding named @name@ exists in @iface@.
type family RequireBinding (name :: Symbol) (iface :: [Binding]) :: K.Constraint where
  RequireBinding name '[] =
    TypeError ('Text "binding not found: " ':<>: 'ShowType name)
  RequireBinding name ('Binding name _ _ _ _ ': _) = ()
  RequireBinding name (_ ': rest) = RequireBinding name rest

-- | Compile-time requirement that a binding named @name@ is a uniform in @iface@.
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

-- | Compile-time requirement that a binding named @name@ is a sampler in @iface@.
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

-- | Compile-time requirement that a binding named @name@ is a texture in @iface@.
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

-- | Compile-time requirement that a binding named @name@ is a storage buffer in @iface@.
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

-- | Compile-time requirement that a binding named @name@ is a storage texture in @iface@.
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

-- | Opaque sampler handle supplied by the host renderer.
newtype SamplerHandle = SamplerHandle Word64
  deriving (Eq, Show)

-- | Opaque texture handle supplied by the host renderer.
newtype TextureHandle = TextureHandle Word64
  deriving (Eq, Show)

-- | Opaque storage-buffer handle supplied by the host renderer.
newtype BufferHandle = BufferHandle Word64
  deriving (Eq, Show)

-- | Opaque storage-texture handle supplied by the host renderer.
newtype StorageTextureHandle = StorageTextureHandle Word64
  deriving (Eq, Show)

-- | Packed uniform entry ready for submission to a renderer.
data UniformInput = UniformInput
  { uiName :: !String
  , uiGroup :: !Word32
  , uiBinding :: !Word32
  , uiBytes :: !ByteString
  } deriving (Eq, Show)

-- | Compact uniform slot view: group, binding, bytes.
-- | Sampler binding entry.
data SamplerInput = SamplerInput
  { samplerName :: !String
  , samplerGroup :: !Word32
  , samplerBinding :: !Word32
  , samplerHandle :: !SamplerHandle
  } deriving (Eq, Show)

-- | Texture binding entry. In combined mode, @textureSampler@ is @Just@.
data TextureInput = TextureInput
  { textureName :: !String
  , textureGroup :: !Word32
  , textureBinding :: !Word32
  , textureHandle :: !TextureHandle
  , textureSampler :: !(Maybe SamplerHandle)
  } deriving (Eq, Show)

-- | Storage-buffer binding entry.
data StorageBufferInput = StorageBufferInput
  { storageBufferName :: !String
  , storageBufferGroup :: !Word32
  , storageBufferBinding :: !Word32
  , storageBufferHandle :: !BufferHandle
  } deriving (Eq, Show)

-- | Storage-texture binding entry.
data StorageTextureInput = StorageTextureInput
  { storageTextureName :: !String
  , storageTextureGroup :: !Word32
  , storageTextureBinding :: !Word32
  , storageTextureHandle :: !StorageTextureHandle
  } deriving (Eq, Show)

-- | Validation error when building inputs.
data InputsError = InputsError
  { ieMessage :: !String
  , ieBinding :: !(Maybe String)
  } deriving (Eq, Show)

-- | Fully validated, normalized inputs derived from a shader interface.
data ShaderInputs (iface :: [Binding]) = ShaderInputs
  { siInterface :: !ShaderInterface
  , siUniforms :: ![UniformInput]
  , siSamplers :: ![SamplerInput]
  , siTextures :: ![TextureInput]
  , siStorageBuffers :: ![StorageBufferInput]
  , siStorageTextures :: ![StorageTextureInput]
  }

-- | Access the reflected interface from the input bundle.
inputsInterface :: ShaderInputs iface -> ShaderInterface
inputsInterface inputs = inputs.siInterface

-- | Access normalized uniform inputs.
inputsUniforms :: ShaderInputs iface -> [UniformInput]
inputsUniforms inputs = inputs.siUniforms

-- | Access normalized sampler inputs.
inputsSamplers :: ShaderInputs iface -> [SamplerInput]
inputsSamplers inputs = inputs.siSamplers

-- | Access normalized texture inputs.
inputsTextures :: ShaderInputs iface -> [TextureInput]
inputsTextures inputs = inputs.siTextures

-- | Access normalized storage-buffer inputs.
inputsStorageBuffers :: ShaderInputs iface -> [StorageBufferInput]
inputsStorageBuffers inputs = inputs.siStorageBuffers

-- | Access normalized storage-texture inputs.
inputsStorageTextures :: ShaderInputs iface -> [StorageTextureInput]
inputsStorageTextures inputs = inputs.siStorageTextures

-- | Build an empty input bundle for a given interface.
emptyInputs :: ShaderInterface -> ShaderInputs iface
emptyInputs iface =
  ShaderInputs
    { siInterface = iface
    , siUniforms = []
    , siSamplers = []
    , siTextures = []
    , siStorageBuffers = []
    , siStorageTextures = []
    }

-- | Uniform inputs sorted by @(group, binding, name)@.
orderedUniforms :: ShaderInputs iface -> [UniformInput]
orderedUniforms inputs =
  orderUniforms inputs.siUniforms

-- | Uniform slots sorted by @(group, binding, name)@.

normalizeInputs :: ShaderInputs iface -> ShaderInputs iface
normalizeInputs inputs =
  inputs
    { siUniforms = orderUniforms inputs.siUniforms
    , siSamplers = orderSamplers inputs.siSamplers
    , siTextures = orderTextures inputs.siTextures
    , siStorageBuffers = orderStorageBuffers inputs.siStorageBuffers
    , siStorageTextures = orderStorageTextures inputs.siStorageTextures
    }

orderUniforms :: [UniformInput] -> [UniformInput]
orderUniforms = sortOn (\u -> (u.uiGroup, u.uiBinding, u.uiName))

orderSamplers :: [SamplerInput] -> [SamplerInput]
orderSamplers = sortOn (\s -> (s.samplerGroup, s.samplerBinding, s.samplerName))

orderTextures :: [TextureInput] -> [TextureInput]
orderTextures = sortOn (\t -> (t.textureGroup, t.textureBinding, t.textureName))

orderStorageBuffers :: [StorageBufferInput] -> [StorageBufferInput]
orderStorageBuffers = sortOn (\b -> (b.storageBufferGroup, b.storageBufferBinding, b.storageBufferName))

orderStorageTextures :: [StorageTextureInput] -> [StorageTextureInput]
orderStorageTextures = sortOn (\t -> (t.storageTextureGroup, t.storageTextureBinding, t.storageTextureName))

data InputItem
  = InputUniform !String !UniformValue
  | InputSampler !String !SamplerHandle
  | InputTexture !String !TextureHandle
  | InputSampledTexture !String !TextureHandle !SamplerHandle
  | InputStorageBuffer !String !BufferHandle
  | InputStorageTexture !String !StorageTextureHandle
  deriving (Eq, Show)

-- | Inputs builder pinned to combined-sampler mode.
type InputsCombined iface = InputsBuilder 'SamplerCombined iface

-- | Inputs builder pinned to separate-sampler mode.
type InputsSeparate iface = InputsBuilder 'SamplerSeparate iface

-- | Declarative, composable input builder.
newtype InputsBuilder (mode :: SamplerBindingMode) (iface :: [Binding]) = InputsBuilder [InputItem]
  deriving (Eq, Show)

instance Semigroup (InputsBuilder mode iface) where
  InputsBuilder a <> InputsBuilder b = InputsBuilder (a <> b)

instance Monoid (InputsBuilder mode iface) where
  mempty = InputsBuilder []

-- | Add a uniform binding by name.
uniform :: forall name mode iface a. (KnownSymbol name, RequireUniform name iface, ToUniform a) => a -> InputsBuilder mode iface
uniform val = InputsBuilder [InputUniform (symbolVal (Proxy @name)) (toUniform val)]

-- | Add a sampler binding by name (separate-sampler mode).
sampler :: forall name mode iface. (KnownSymbol name, RequireSampler name iface) => SamplerHandle -> InputsBuilder mode iface
sampler handle = InputsBuilder [InputSampler (symbolVal (Proxy @name)) handle]

-- | Add a texture binding by name (separate-sampler mode).
texture :: forall name mode iface. (KnownSymbol name, RequireTexture name iface) => TextureHandle -> InputsBuilder mode iface
texture handle = InputsBuilder [InputTexture (symbolVal (Proxy @name)) handle]

-- | Add a combined texture+sampler binding by name.
sampledTexture :: forall name mode iface. (KnownSymbol name, RequireTexture name iface) => TextureHandle -> SamplerHandle -> InputsBuilder mode iface
sampledTexture texHandle samplerHandle =
  InputsBuilder [InputSampledTexture (symbolVal (Proxy @name)) texHandle samplerHandle]

-- | Add a storage-buffer binding by name.
storageBuffer :: forall name mode iface. (KnownSymbol name, RequireStorageBuffer name iface) => BufferHandle -> InputsBuilder mode iface
storageBuffer handle = InputsBuilder [InputStorageBuffer (symbolVal (Proxy @name)) handle]

-- | Add a storage-texture binding by name.
storageTexture :: forall name mode iface. (KnownSymbol name, RequireStorageTexture name iface) => StorageTextureHandle -> InputsBuilder mode iface
storageTexture handle = InputsBuilder [InputStorageTexture (symbolVal (Proxy @name)) handle]

inputsFrom :: forall mode iface. ShaderInterface -> InputsBuilder mode iface -> Either InputsError (ShaderInputs iface)
inputsFrom iface (InputsBuilder items) =
  let bmap = bindingMap iface
      initInputs = emptyInputs iface
  in do
      (inputs, _) <- foldM (applyItem bmap) (initInputs, Set.empty) items
      let normalized = normalizeInputs inputs
      case iface.siSamplerMode of
        SamplerCombined ->
          let missing = [t.textureName | t <- normalized.siTextures, isNothing t.textureSampler]
          in if null missing
              then Right normalized
              else Left (InputsError ("missing sampler for textures: " <> intercalate ", " missing) Nothing)
        SamplerSeparate -> Right normalized

-- | Validate and normalize inputs against a shader.
inputsFor :: forall mode iface. Shader mode iface -> InputsBuilder mode iface -> Either InputsError (ShaderInputs iface)
inputsFor shader = inputsFrom (shaderInterface shader)

applyItem :: BindingMap -> (ShaderInputs iface, Set.Set String) -> InputItem -> Either InputsError (ShaderInputs iface, Set.Set String)
applyItem bmap (inputs, seen) item = do
  let name = itemName item
  when (Set.member name seen) $
    Left (InputsError ("duplicate binding entry: " <> name) (Just name))
  info <- maybe (Left (InputsError ("binding not found in interface: " <> name) (Just name))) Right (bindingInfoForMap name bmap)
  inputs' <- applyBinding name info item inputs
  pure (inputs', Set.insert name seen)

applyBinding :: String -> BindingInfo -> InputItem -> ShaderInputs iface -> Either InputsError (ShaderInputs iface)
applyBinding name info item inputs =
  case (item, info.biKind) of
    (InputUniform _ val, kind)
      | isUniformKind kind -> do
          bytes <- first (\err -> InputsError ("binding " <> name <> ": " <> err) (Just name)) (packUniform info.biType val)
          pure inputs
            { siUniforms =
                UniformInput
                  { uiName = info.biName
                  , uiGroup = info.biGroup
                  , uiBinding = info.biBinding
                  , uiBytes = bytes
                  }
                  : inputs.siUniforms
            }
    (InputSampler _ handle, kind)
      | isSamplerKind kind ->
          Right inputs
            { siSamplers =
                SamplerInput
                  { samplerName = info.biName
                  , samplerGroup = info.biGroup
                  , samplerBinding = info.biBinding
                  , samplerHandle = handle
                  }
                  : inputs.siSamplers
            }
    (InputTexture _ handle, kind)
      | isTextureKind kind ->
          Right inputs
            { siTextures =
                TextureInput
                  { textureName = info.biName
                  , textureGroup = info.biGroup
                  , textureBinding = info.biBinding
                  , textureHandle = handle
                  , textureSampler = Nothing
                  }
                  : inputs.siTextures
            }
    (InputSampledTexture _ handle sampHandle, kind)
      | isTextureKind kind ->
          Right inputs
            { siTextures =
                TextureInput
                  { textureName = info.biName
                  , textureGroup = info.biGroup
                  , textureBinding = info.biBinding
                  , textureHandle = handle
                  , textureSampler = Just sampHandle
                  }
                  : inputs.siTextures
            }
    (InputStorageBuffer _ handle, kind)
      | isStorageBufferKind kind ->
          Right inputs
            { siStorageBuffers =
                StorageBufferInput
                  { storageBufferName = info.biName
                  , storageBufferGroup = info.biGroup
                  , storageBufferBinding = info.biBinding
                  , storageBufferHandle = handle
                  }
                  : inputs.siStorageBuffers
            }
    (InputStorageTexture _ handle, kind)
      | isStorageTextureKind kind ->
          Right inputs
            { siStorageTextures =
                StorageTextureInput
                  { storageTextureName = info.biName
                  , storageTextureGroup = info.biGroup
                  , storageTextureBinding = info.biBinding
                  , storageTextureHandle = handle
                  }
                  : inputs.siStorageTextures
            }
    _ -> Left (InputsError ("binding " <> name <> ": kind mismatch") (Just name))

itemName :: InputItem -> String
itemName item =
  case item of
    InputUniform name _ -> name
    InputSampler name _ -> name
    InputTexture name _ -> name
    InputSampledTexture name _ _ -> name
    InputStorageBuffer name _ -> name
    InputStorageTexture name _ -> name
