{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Foreign.Ptr (nullPtr, wordPtrToPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic)

import Slop hiding (V4)
import Slop.SDL.Raw (GPUDevice(..), GPUSampler(..), GPUTexture(..))

import Spirdo.Wesl
  ( BindingPlan(..)
  , CompiledShader(..)
  , PreparedShader(..)
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
  Variant :: RequireUniform "params" iface => String -> Pipeline -> PreparedShader iface -> Variant

data FragmentVariant where
  FragmentVariant :: RequireUniform "params" iface => String -> CompiledShader iface -> FragmentVariant

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
    vshader <- createVertexShader (shaderSpirv (psShader vprep)) (countsFromPlan (psPlan vprep))

    variants <- mapM (\(FragmentVariant name shader) -> mkVariant name shader vshader) fragmentVariants

    _ <- loop (0 :: Int) $ \frame idx -> do
      let idx' = stepIndex frame.input idx (length variants)
      case variants !! idx' of
        Variant vName vPipeline fprep -> do
          let params = frameParams frame
          case inputsFromPrepared fprep (Inputs.uniform @"params" params) of
            Left err -> error (vName <> ": " <> err)
            Right inputs -> do
              clear (rgb 0.06 0.07 0.1)
              drawMesh vPipeline mesh (bindingsFromInputs inputs)
              pure (Continue idx')
    pure ()

frameParams :: Frame -> ParamsU
frameParams frame =
  let (w, h) = frame.size
  in ParamsU
      { time_res = V4 frame.time (fromIntegral w) (fromIntegral h) 0.0
      , color = V4 1 1 1 1
      }

stepIndex :: InputFrame -> Int -> Int -> Int
stepIndex input idx count
  | count == 0 = 0
  | otherwise = (idx + deltaInput input + count) `mod` count

deltaInput :: InputFrame -> Int
deltaInput input
  | keyPressed KeyLeft input = -1
  | keyPressed KeyRight input = 1
  | otherwise = 0

unsafePrepare :: String -> CompiledShader iface -> WindowM (PreparedShader iface)
unsafePrepare label shader =
  case prepareShader shader of
    Left err -> error (label <> ": " <> err)
    Right value -> pure value

mkVariant :: RequireUniform "params" iface => String -> CompiledShader iface -> VertexShader -> WindowM Variant
mkVariant name fragShader vshader = do
  prep <- unsafePrepare ("fragment:" <> name) fragShader
  fshader <- createFragmentShader (shaderSpirv (psShader prep)) (countsFromPlan (psPlan prep))
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
  pure (Variant name pipeline prep)

fragmentVariants :: [FragmentVariant]
fragmentVariants =
  [ FragmentVariant "Feature Mix" fragmentFeatureShader
  , FragmentVariant "Raymarch" fragmentRaymarchShader
  , FragmentVariant "Triangle" fragmentTriangleShader
  , FragmentVariant "Plasma" fragmentPlasmaShader
  , FragmentVariant "Grid" fragmentGridShader
  , FragmentVariant "SDF Text" fragmentSdfTextShader
  , FragmentVariant "Clouds" fragmentCloudShader
  , FragmentVariant "Bits" fragmentBitsShader
  , FragmentVariant "Aurora" fragmentAuroraShader
  , FragmentVariant "Starfield" fragmentStarfieldShader
  , FragmentVariant "Tunnel" fragmentTunnelShader
  , FragmentVariant "Voronoi" fragmentVoronoiShader
  , FragmentVariant "Mandelbrot" fragmentMandelbrotShader
  , FragmentVariant "Terrain" fragmentTerrainShader
  , FragmentVariant "Seascape" fragmentSeascapeShader
  , FragmentVariant "Protean Clouds" fragmentProteanCloudsShader
  , FragmentVariant "Primitives" fragmentPrimitivesShader
  , FragmentVariant "Grass" fragmentGrassShader
  ]

countsFromPlan :: BindingPlan -> ShaderCounts
countsFromPlan bindingPlan =
  ShaderCounts
    (fromIntegral (length (bpSamplers bindingPlan)))
    (fromIntegral (length (bpStorageTextures bindingPlan)))
    (fromIntegral (length (bpStorageBuffers bindingPlan)))
    (fromIntegral (length (bpUniforms bindingPlan)))

bindingsFromInputs :: ShaderInputs iface -> [Binding]
bindingsFromInputs inputs =
  let uniforms =
        [ fUniformBytes (uiBinding u) (uiBytes u)
        | u <- orderedUniforms inputs
        ]
      samplers =
        [ fSamplerWith (samplerBinding s) (textureFromHandle (TextureHandle (samplerTex s))) (samplerFromHandle (samplerHandle s))
        | s <- siSamplers inputs
        ]
      storage =
        [ fStorageTexture (storageTextureBinding t) (storageTextureFromHandle (storageTextureHandle t))
        | t <- siStorageTextures inputs
        ]
  in uniforms <> samplers <> storage
  where
    samplerTex (SamplerInput samplerName _ _ _) =
      case findTexture samplerName (siTextures inputs) of
        Just tex -> unTexture tex
        Nothing -> 0
    unTexture (TextureInput _ _ _ textureHandle) =
      case textureHandle of
        TextureHandle v -> v
    findTexture name =
      foldr (\t acc -> if textureName t == name then Just t else acc) Nothing

textureFromHandle :: TextureHandle -> Texture
textureFromHandle (TextureHandle value) =
  Texture
    { textureHandle = GPUTexture (wordPtrToPtr (fromIntegral value))
    , textureDevice = GPUDevice nullPtr
    , textureWidth = 0
    , textureHeight = 0
    }

storageTextureFromHandle :: StorageTextureHandle -> Texture
storageTextureFromHandle (StorageTextureHandle value) =
  Texture
    { textureHandle = GPUTexture (wordPtrToPtr (fromIntegral value))
    , textureDevice = GPUDevice nullPtr
    , textureWidth = 0
    , textureHeight = 0
    }

samplerFromHandle :: SamplerHandle -> Sampler
samplerFromHandle (SamplerHandle value) =
  Sampler (GPUSampler (wordPtrToPtr (fromIntegral value)))
