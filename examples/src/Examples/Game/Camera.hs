{-# LANGUAGE OverloadedRecordDot #-}

module Examples.Game.Camera (gameViewProjection) where

import Slop (M44(..), V3(..), Vector(normalize), dot, v3Cross)

import Examples.Game.Logic (GroundPosition(..))

gameViewProjection :: (Int, Int) -> GroundPosition -> M44 Float
gameViewProjection (width, height) playerPosition =
  projectionMatrix (pi / 3.2) aspect 0.1 80
    * viewMatrix
      (V3 playerPosition.x 8.2 (playerPosition.z + 10.5))
      (V3 playerPosition.x 0.1 (playerPosition.z - 1.2))
      (V3 0 1 0)
  where
    aspect
      | width <= 0 || height <= 0 = 1
      | otherwise = fromIntegral width / fromIntegral height

viewMatrix :: V3 Float -> V3 Float -> V3 Float -> M44 Float
viewMatrix eye target up =
  M44
    sideX sideY sideZ (-dot side eye)
    upX upY upZ (-dot cameraUp eye)
    (-forwardX) (-forwardY) (-forwardZ) (dot forward eye)
    0 0 0 1
  where
    forward@(V3 forwardX forwardY forwardZ) = normalize (target - eye)
    side@(V3 sideX sideY sideZ) = normalize (v3Cross forward up)
    cameraUp@(V3 upX upY upZ) = v3Cross side forward

projectionMatrix :: Float -> Float -> Float -> Float -> M44 Float
projectionMatrix fieldOfViewY aspect nearPlane farPlane =
  M44
    (focalLength / aspect) 0 0 0
    0 focalLength 0 0
    0 0 depthScale depthOffset
    0 0 (-1) 0
  where
    focalLength = 1 / tan (fieldOfViewY / 2)
    depthRange = nearPlane - farPlane
    depthScale = farPlane / depthRange
    depthOffset = farPlane * nearPlane / depthRange
