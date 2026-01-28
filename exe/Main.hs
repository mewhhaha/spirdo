{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Word (Word32)
import Foreign.Ptr (nullPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Slop hiding (V4)
import Slop.SDL.Raw (GPUDevice(..), GPUSampler(..), GPUTexture(..))

import Spirdo.Wesl
  ( BindingPlan(..)
  , BindingInfo(..)
  , CompiledShader(..)
  , PreparedShader(..)
  , SamplerBindingMode(..)
  , ShaderInterface(..)
  , ToUniform
  , V4(..)
  , prepareShader
  )
import Spirdo.Wesl.Inputs
  ( SamplerHandle(..)
  , TextureHandle(..)
  , StorageTextureHandle(..)
  , RequireUniform
  , UniformInput(..)
  , SamplerInput(..)
  , TextureInput(..)
  , StorageTextureInput(..)
  , ShaderInputs(..)
  , InputsBuilder
  , inputsFromPrepared
  , orderedUniforms
  )
import qualified Spirdo.Wesl.Inputs as Inputs

import Examples.Fragments
import Examples.Vertex (vertexShader)

-- Uniforms shared by fragment variants.
data ParamsU = ParamsU
  { time_res :: V4 Float
  , color :: V4 Float
  } deriving (Generic)

instance ToUniform ParamsU

-- Vertex data for fullscreen quad.
newtype Vertex = Vertex (Float, Float, Float, Float)

instance Storable Vertex where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr =
    Vertex
      <$> ( (,,,)
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 4
              <*> peekByteOff ptr 8
              <*> peekByteOff ptr 12
          )
  poke ptr (Vertex (x, y, u, v)) =
    pokeByteOff ptr 0 x
      >> pokeByteOff ptr 4 y
      >> pokeByteOff ptr 8 u
      >> pokeByteOff ptr 12 v

vertexLayout :: VertexLayout
vertexLayout =
  VertexLayout
    { layoutStride = 16
    , layoutAttrs =
        [ VertexAttr 0 VertexFloat2 0
        , VertexAttr 1 VertexFloat2 8
        ]
    }

quadVertices :: [Vertex]
quadVertices = fmap Vertex
  [ (-1, -1, 0, 0)
  , (1, -1, 1, 0)
  , (1, 1, 1, 1)
  , (-1, -1, 0, 0)
  , (1, 1, 1, 1)
  , (-1, 1, 0, 1)
  ]

data Variant where
  Variant :: RequireUniform "params" iface => String -> Pipeline -> PreparedShader iface -> (ParamsU -> InputsBuilder iface) -> Variant

data FragmentVariant where
  FragmentVariant :: RequireUniform "params" iface => String -> CompiledShader iface -> (ParamsU -> InputsBuilder iface) -> FragmentVariant

main :: IO ()
main = do
  let cfg =
        defaultConfig
          { windowTitle = "Spirdo Slop demo"
          , windowWidth = 960
          , windowHeight = 540
          , windowResizable = True
          }
  runWindow cfg $ do
    mesh <- createMesh vertexLayout quadVertices
    vprep <- unsafePrepare "vertexShader" vertexShader
    vshader <- createVertexShader (shaderSpirv (psShader vprep)) (countsFromPrepared vprep)

    noiseSampler <- createSampler (defaultSamplerDesc { samplerAddressU = SamplerRepeat, samplerAddressV = SamplerRepeat, samplerAddressW = SamplerRepeat })
    noise3D <- createNoiseTexture3D 128 128 128 (loopNoise3D 128 128 128 (defaultNoiseSettings { noiseType = NoisePerlin, noiseScale = 32, noiseOctaves = 4 }))
    let noiseSamplerHandle = samplerHandleFromSlop noiseSampler
    let noise3DHandle = textureHandleFromSlop noise3D

    let baseVariants =
          [ FragmentVariant "Feature Mix" fragmentFeatureShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Raymarch" fragmentRaymarchShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Triangle" fragmentTriangleShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Plasma" fragmentPlasmaShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Grid" fragmentGridShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "SDF Text" fragmentSdfTextShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Clouds" fragmentCloudShader
              (\params ->
                Inputs.uniform @"params" params
                  <> Inputs.sampledTexture @"noiseTex3D" noise3DHandle noiseSamplerHandle
              )
          , FragmentVariant "Bits" fragmentBitsShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Aurora" fragmentAuroraShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Starfield" fragmentStarfieldShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Tunnel" fragmentTunnelShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Voronoi" fragmentVoronoiShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Mandelbrot" fragmentMandelbrotShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Terrain" fragmentTerrainShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Seascape" fragmentSeascapeShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Protean Clouds" fragmentProteanCloudsShader
              (\params ->
                Inputs.uniform @"params" params
                  <> Inputs.sampledTexture @"noiseTex3D" noise3DHandle noiseSamplerHandle
              )
          , FragmentVariant "Primitives" fragmentPrimitivesShader (\params -> Inputs.uniform @"params" params)
          , FragmentVariant "Grass" fragmentGrassShader (\params -> Inputs.uniform @"params" params)
          ]

    variants <- mapM (\(FragmentVariant name shader mkInputs) -> mkVariant name shader mkInputs vshader) baseVariants

    _ <- loop (0 :: Int) $ \frame idx -> do
      let count = length variants
      let delta
            | keyPressed KeyLeft frame.input = -1
            | keyPressed KeyRight frame.input = 1
            | otherwise = 0
      let idx' = if count == 0 then 0 else (idx + delta + count) `mod` count
      case variants !! idx' of
        Variant vName vPipeline fprep mkInputs -> do
          let (w, h) = frame.renderSize
          let params =
                ParamsU
                  { time_res = V4 frame.time (fromIntegral w) (fromIntegral h) 0.0
                  , color = V4 1 1 1 1
                  }
          case inputsFromPrepared fprep (mkInputs params) of
            Left err -> error (vName <> ": " <> err)
            Right inputs -> do
              clear (rgb 0.06 0.07 0.1)
              drawMesh vPipeline mesh (bindingsFromInputs inputs)
              pure (Continue idx')
    pure ()

unsafePrepare :: String -> CompiledShader iface -> WindowM (PreparedShader iface)
unsafePrepare label shader =
  case prepareShader shader of
    Left err -> error (label <> ": " <> err)
    Right value -> pure value

mkVariant :: RequireUniform "params" iface => String -> CompiledShader iface -> (ParamsU -> InputsBuilder iface) -> VertexShader -> WindowM Variant
mkVariant name fragShader mkInputs vshader = do
  prep <- unsafePrepare ("fragment:" <> name) fragShader
  fshader <- createFragmentShader (shaderSpirv (psShader prep)) (countsFromPrepared prep)
  pipeline <- graphicsPipeline
    GraphicsDesc
      { gfxVertex = vshader
      , gfxFragment = fshader
      , gfxLayout = vertexLayout
      , gfxPrimitive = PrimTriangles
      , gfxTarget = TargetSwapchain
      , gfxBlend = BlendNone
      , gfxDepth = DepthNone
      , gfxDepthFormat = 0
      }
  pure (Variant name pipeline prep mkInputs)


countsFromPrepared :: PreparedShader iface -> ShaderCounts
countsFromPrepared prepared =
  let bindingPlan = psPlan prepared
      iface = shaderInterface (psShader prepared)
      samplerBindings =
        case siSamplerMode iface of
          SamplerCombined -> bpTextures bindingPlan
          SamplerSeparate -> bpSamplers bindingPlan
      samplerCount = bindingCount samplerBindings
      storageTextureCount = bindingCount (bpStorageTextures bindingPlan)
      storageBufferCount = bindingCount (bpStorageBuffers bindingPlan)
      uniformCount = bindingCount (bpUniforms bindingPlan)
  in ShaderCounts
      samplerCount
      storageTextureCount
      storageBufferCount
      uniformCount
  where
    bindingCount :: [BindingInfo] -> Word32
    bindingCount [] = 0
    bindingCount xs = maximum (map biBinding xs) + 1

bindingsFromInputs :: ShaderInputs iface -> [Binding]
bindingsFromInputs inputs =
  let iface = shaderInterface (siShader inputs)
      uniforms =
        [ fUniformBytes (uiBinding u) (uiBytes u)
        | u <- orderedUniforms inputs
        ]
      samplers =
        case siSamplerMode iface of
          SamplerCombined ->
            [ fSamplerWith (textureBinding t) (textureFromHandle (textureHandle t)) (samplerFromHandle (resolveSampler t))
            | t <- siTextures inputs
            ]
          SamplerSeparate ->
            [ fSamplerWith (samplerBinding s) (textureFromHandle (TextureHandle (samplerTex s))) (samplerFromHandle (samplerHandle s))
            | s <- siSamplers inputs
            ]
      storage =
        [ fStorageTexture (storageTextureBinding t) (storageTextureFromHandle (storageTextureHandle t))
        | t <- siStorageTextures inputs
        ]
  in uniforms <> samplers <> storage
  where
    resolveSampler texInput =
      case textureSampler texInput of
        Just handle -> handle
        Nothing ->
          case findSamplerForTexture (textureName texInput) (siSamplers inputs) of
            Just handle -> handle
            Nothing -> error ("missing sampler for texture: " <> textureName texInput)
    samplerTex (SamplerInput samplerName _ _ _) =
      case findSamplerTexture samplerName (siTextures inputs) of
        Just tex -> unTexture tex
        Nothing -> 0
    unTexture (TextureInput _ _ _ textureHandle _) =
      case textureHandle of
        TextureHandle v -> v
    findSamplerTexture name textures =
      findTexture name textures
        <|> (stripSuffix "_sampler" name >>= \base -> findTexture base textures)
        <|> (stripSuffix "Sampler" name >>= \base -> findTexture base textures)
    findSamplerForTexture name samplers =
      findSampler name samplers
        <|> findSampler (name <> "_sampler") samplers
        <|> findSampler (name <> "Sampler") samplers
    findSampler name samplers =
      samplerHandle <$> find (\s -> samplerName s == name) samplers

    findTexture name =
      foldr (\t acc -> if textureName t == name then Just t else acc) Nothing

    stripSuffix suffix value =
      let suffixLen = length suffix
          valueLen = length value
      in if valueLen >= suffixLen && drop (valueLen - suffixLen) value == suffix
          then Just (take (valueLen - suffixLen) value)
          else Nothing

textureFromHandle :: TextureHandle -> Texture
textureFromHandle (TextureHandle value) =
  Texture
    { textureHandle = GPUTexture (wordPtrToPtr (fromIntegral value))
    , textureDevice = GPUDevice nullPtr
    , textureWidth = 0
    , textureHeight = 0
    , textureDepth = 0
  }

samplerHandleFromSlop :: Sampler -> SamplerHandle
samplerHandleFromSlop (Sampler sampler) =
  case sampler of
    GPUSampler ptr -> SamplerHandle (fromIntegral (ptrToWordPtr ptr))

textureHandleFromSlop :: Texture -> TextureHandle
textureHandleFromSlop tex =
  case tex.textureHandle of
    GPUTexture ptr -> TextureHandle (fromIntegral (ptrToWordPtr ptr))

storageTextureFromHandle :: StorageTextureHandle -> Texture
storageTextureFromHandle (StorageTextureHandle value) =
  Texture
    { textureHandle = GPUTexture (wordPtrToPtr (fromIntegral value))
    , textureDevice = GPUDevice nullPtr
    , textureWidth = 0
    , textureHeight = 0
    , textureDepth = 0
    }

samplerFromHandle :: SamplerHandle -> Sampler
samplerFromHandle (SamplerHandle value) =
  Sampler (GPUSampler (wordPtrToPtr (fromIntegral value)))
