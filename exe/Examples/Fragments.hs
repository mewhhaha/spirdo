{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Fragment shader example registry.
module Examples.Fragments
  ( fragmentFeatureShader
  , fragmentRaymarchShader
  , fragmentTriangleShader
  , fragmentPlasmaShader
  , fragmentGridShader
  , fragmentSdfTextShader
  , fragmentCloudShader
  , fragmentBitsShader
  , fragmentAuroraShader
  , fragmentStarfieldShader
  , fragmentTunnelShader
  , fragmentVoronoiShader
  , fragmentMandelbrotShader
  , fragmentTerrainShader
  , fragmentSeascapeShader
  , fragmentProteanCloudsShader
  , fragmentPrimitivesShader
  , fragmentGrassShader
  ) where

import Examples.Fragments.Aurora (fragmentAuroraShader)
import Examples.Fragments.Bits (fragmentBitsShader)
import Examples.Fragments.Clouds (fragmentCloudShader)
import Examples.Fragments.FeatureMix (fragmentFeatureShader)
import Examples.Fragments.Grass (fragmentGrassShader)
import Examples.Fragments.Grid (fragmentGridShader)
import Examples.Fragments.Mandelbrot (fragmentMandelbrotShader)
import Examples.Fragments.Plasma (fragmentPlasmaShader)
import Examples.Fragments.Primitives (fragmentPrimitivesShader)
import Examples.Fragments.ProteanClouds (fragmentProteanCloudsShader)
import Examples.Fragments.Raymarch (fragmentRaymarchShader)
import Examples.Fragments.SdfText (fragmentSdfTextShader)
import Examples.Fragments.Seascape (fragmentSeascapeShader)
import Examples.Fragments.Starfield (fragmentStarfieldShader)
import Examples.Fragments.Terrain (fragmentTerrainShader)
import Examples.Fragments.Triangle (fragmentTriangleShader)
import Examples.Fragments.Tunnel (fragmentTunnelShader)
import Examples.Fragments.Voronoi (fragmentVoronoiShader)
