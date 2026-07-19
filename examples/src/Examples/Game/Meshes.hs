{-# LANGUAGE OverloadedRecordDot #-}

module Examples.Game.Meshes
  ( cubeVertices
  , crystalVertices
  , groundVertices
  , rotationY
  , rotationZ
  , scale
  , shipModel
  , shipVertices
  , translation
  ) where

import Slop (M44(..), Vertex3D(..))

import Examples.Game.Logic (GroundPosition(..), MoveDirection(..))

type Point3 = (Float, Float, Float)
type Tint = (Float, Float, Float)

shipVertices :: [Vertex3D]
shipVertices =
  concat
    [ triangle nose left top cyan
    , triangle nose top right cyan
    , triangle nose right bottom blue
    , triangle nose bottom left blue
    , triangle left rear top violet
    , triangle top rear right violet
    , triangle right rear bottom darkViolet
    , triangle bottom rear left darkViolet
    ]
  where
    nose = (0, 0, -1.15)
    left = (-0.75, 0, 0.55)
    right = (0.75, 0, 0.55)
    top = (0, 0.34, 0.38)
    bottom = (0, -0.22, 0.38)
    rear = (0, 0, 0.82)
    cyan = (0.15, 0.9, 1.0)
    blue = (0.08, 0.3, 0.9)
    violet = (0.55, 0.25, 1.0)
    darkViolet = (0.22, 0.08, 0.55)

shipModel :: GroundPosition -> Float -> MoveDirection -> M44 Float
shipModel playerPosition facing direction =
  translation playerPosition.x 0.48 playerPosition.z
    * rotationY facing
    * rotationZ ((-direction.x) * 0.16)
    * scale 0.78 0.78 0.78

crystalVertices :: [Vertex3D]
crystalVertices =
  concat
    [ triangle top east north gold
    , triangle top north west amber
    , triangle top west south gold
    , triangle top south east amber
    , triangle bottom north east orange
    , triangle bottom west north redOrange
    , triangle bottom south west orange
    , triangle bottom east south redOrange
    ]
  where
    top = (0, 0.9, 0)
    bottom = (0, -0.9, 0)
    east = (0.48, 0, 0)
    north = (0, 0, -0.48)
    west = (-0.48, 0, 0)
    south = (0, 0, 0.48)
    gold = (1.0, 0.92, 0.25)
    amber = (1.0, 0.55, 0.08)
    orange = (0.95, 0.25, 0.04)
    redOrange = (0.7, 0.08, 0.08)

cubeVertices :: [Vertex3D]
cubeVertices =
  concat
    [ quad p000 p100 p110 p010 stone
    , quad p101 p001 p011 p111 stoneDark
    , quad p001 p000 p010 p011 stoneBlue
    , quad p100 p101 p111 p110 stoneBlue
    , quad p010 p110 p111 p011 stoneLight
    , quad p001 p101 p100 p000 stoneDark
    ]
  where
    p000 = (-0.5, -0.5, -0.5)
    p100 = (0.5, -0.5, -0.5)
    p110 = (0.5, 0.5, -0.5)
    p010 = (-0.5, 0.5, -0.5)
    p001 = (-0.5, -0.5, 0.5)
    p101 = (0.5, -0.5, 0.5)
    p111 = (0.5, 0.5, 0.5)
    p011 = (-0.5, 0.5, 0.5)
    stone = (0.18, 0.24, 0.38)
    stoneDark = (0.08, 0.1, 0.2)
    stoneBlue = (0.12, 0.2, 0.34)
    stoneLight = (0.28, 0.35, 0.52)

groundVertices :: [Vertex3D]
groundVertices =
  quadWithUv
    (-9, 0, -8)
    (9, 0, -8)
    (9, 0, 8)
    (-9, 0, 8)
    (0, 0)
    (9, 0)
    (9, 8)
    (0, 8)
    (0.12, 0.18, 0.32)

translation :: Float -> Float -> Float -> M44 Float
translation x y z =
  M44
    1 0 0 x
    0 1 0 y
    0 0 1 z
    0 0 0 1

scale :: Float -> Float -> Float -> M44 Float
scale x y z =
  M44
    x 0 0 0
    0 y 0 0
    0 0 z 0
    0 0 0 1

rotationY :: Float -> M44 Float
rotationY angle =
  let c = cos angle
      s = sin angle
  in M44
      c 0 s 0
      0 1 0 0
      (-s) 0 c 0
      0 0 0 1

rotationZ :: Float -> M44 Float
rotationZ angle =
  let c = cos angle
      s = sin angle
  in M44
      c (-s) 0 0
      s c 0 0
      0 0 1 0
      0 0 0 1

triangle :: Point3 -> Point3 -> Point3 -> Tint -> [Vertex3D]
triangle a b c tint =
  [ vertex a (0, 0) tint
  , vertex b (1, 0) tint
  , vertex c (0.5, 1) tint
  ]

quad :: Point3 -> Point3 -> Point3 -> Point3 -> Tint -> [Vertex3D]
quad a b c d tint =
  quadWithUv a b c d (0, 0) (1, 0) (1, 1) (0, 1) tint

quadWithUv :: Point3 -> Point3 -> Point3 -> Point3 -> (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Tint -> [Vertex3D]
quadWithUv a b c d uvA uvB uvC uvD tint =
  [ vertex a uvA tint
  , vertex b uvB tint
  , vertex c uvC tint
  , vertex a uvA tint
  , vertex c uvC tint
  , vertex d uvD tint
  ]

vertex :: Point3 -> (Float, Float) -> Tint -> Vertex3D
vertex (x, y, z) (u, v) (r, g, b) =
  Vertex3D
    (realToFrac x) (realToFrac y) (realToFrac z) 1
    (realToFrac u) (realToFrac v)
    (realToFrac r) (realToFrac g) (realToFrac b) 1
