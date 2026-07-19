{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Examples.Game.Input
  ( interactiveFrameSeconds
  , movementFromKeys
  ) where

import Slop (Key(..))

import Examples.Game.Logic (MoveDirection(..))

interactiveFrameSeconds :: Float -> Float
interactiveFrameSeconds = min 0.25

movementFromKeys :: (Key -> Bool) -> MoveDirection
movementFromKeys isKeyDown =
  MoveDirection
    { x = keyAxis KeyA KeyLeft KeyD KeyRight
    , z = keyAxis KeyW KeyUp KeyS KeyDown
    }
  where
    keyAxis negativeKey alternateNegativeKey positiveKey alternatePositiveKey =
      keyValue positiveKey alternatePositiveKey
        - keyValue negativeKey alternateNegativeKey
    keyValue primaryKey alternateKey =
      if isKeyDown primaryKey || isKeyDown alternateKey then 1 else 0
