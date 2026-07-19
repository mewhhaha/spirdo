{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (unless)
import Slop (Key(..), V4(..), m44MulV4)

import Examples.Game.Camera (gameViewProjection)
import Examples.Game.Input (interactiveFrameSeconds, movementFromKeys)
import Examples.Game.Logic
  ( GameInput(..)
  , GroundPosition(..)
  , MoveDirection(..)
  , activeCrystals
  , advanceGame
  , gamePlayerPosition
  , gamePlayerHeading
  , gameScore
  , initialGame
  )

main :: IO ()
main = do
  movementAliasesDoNotStack
  interactiveFramesHaveABoundedDuration
  movingForOneSecondAdvancesFiveUnits
  diagonalMovementKeepsTheSameSpeed
  movingRightPointsThePlayerRight
  neutralInputPreservesThePlayerHeading
  movementStopsAtTheArenaBoundary
  slidingAlongAWallPreservesMovementSpeed
  reachingACrystalCollectsItOnce
  crossingACrystalDuringALongFrameCollectsIt
  orbitingCrystalIsCollectedBetweenMatchingEndpoints
  cameraKeepsThePlayerInsideTheClipVolume
  cameraHandlesAZeroSizedRenderTarget
  resetRestoresTheInitialGame
  invalidFrameDurationDoesNotChangeTheGame
  putStrLn "game logic tests passed"

movementAliasesDoNotStack :: IO ()
movementAliasesDoNotStack = do
  let direction = movementFromKeys (`elem` [KeyW, KeyUp, KeyS])
  unless (direction == MoveDirection 0 0) $
    fail ("movement aliases did not cancel the opposite direction: " <> show direction)

interactiveFramesHaveABoundedDuration :: IO ()
interactiveFramesHaveABoundedDuration = do
  assertApprox "interactive frame duration cap" 0.25 (interactiveFrameSeconds 4)
  assertApprox "ordinary interactive frame duration" 0.1 (interactiveFrameSeconds 0.1)

movingForOneSecondAdvancesFiveUnits :: IO ()
movingForOneSecondAdvancesFiveUnits = do
  let state = advanceGame 1 (moving 1 0) initialGame
      position = gamePlayerPosition state
  assertApprox "one-second x movement" 5 position.x
  assertApprox "one-second x movement leaves z unchanged" 0 position.z

diagonalMovementKeepsTheSameSpeed :: IO ()
diagonalMovementKeepsTheSameSpeed = do
  let state = advanceGame 1 (moving 1 1) initialGame
      position = gamePlayerPosition state
      distance = sqrt (position.x * position.x + position.z * position.z)
  assertApprox "diagonal movement distance" 5 distance

movingRightPointsThePlayerRight :: IO ()
movingRightPointsThePlayerRight = do
  let state = advanceGame 0.1 (moving 1 0) initialGame
  assertApprox "rightward player heading" (-pi / 2) (gamePlayerHeading state)

neutralInputPreservesThePlayerHeading :: IO ()
neutralInputPreservesThePlayerHeading = do
  let movingState = advanceGame 0.1 (moving 1 0) initialGame
      neutralState = advanceGame 0.1 (moving 0 0) movingState
  assertApprox
    "neutral input preserves player heading"
    (gamePlayerHeading movingState)
    (gamePlayerHeading neutralState)

movementStopsAtTheArenaBoundary :: IO ()
movementStopsAtTheArenaBoundary = do
  let state = advanceGame 10 (moving 1 0) initialGame
      position = gamePlayerPosition state
  assertApprox "arena boundary" 8 position.x

slidingAlongAWallPreservesMovementSpeed :: IO ()
slidingAlongAWallPreservesMovementSpeed = do
  let atWall = advanceGame 2 (moving 1 0) initialGame
      state = advanceGame 1 (moving 1 1) atWall
      position = gamePlayerPosition state
  assertApprox "wall slide remains on boundary" 8 position.x
  assertApprox "wall slide preserves tangent speed" 5 position.z
  assertApprox "wall slide faces its tangent direction" pi (abs (gamePlayerHeading state))

reachingACrystalCollectsItOnce :: IO ()
reachingACrystalCollectsItOnce = do
  let state = advanceGame 1 (moving (-3.8) (-3.5)) initialGame
      advancedState = advanceGame 0.1 (moving 0 0) state
  unless (gameScore state == 1) $
    fail "reaching a crystal did not increase the score"
  unless (length (activeCrystals state) == 4) $
    fail "a collected crystal remained active"
  unless (gameScore advancedState == 1) $
    fail "an already collected crystal increased the score again"

crossingACrystalDuringALongFrameCollectsIt :: IO ()
crossingACrystalDuringALongFrameCollectsIt = do
  let state = advanceGame 1.2 (moving (-3.8) (-3.5)) initialGame
  unless (gameScore state == 1) $
    fail "crossing a crystal during a long frame did not collect it"

orbitingCrystalIsCollectedBetweenMatchingEndpoints :: IO ()
orbitingCrystalIsCollectedBetweenMatchingEndpoints = do
  let movedLeft = advanceGame 1.2 (moving (-1) 0) initialGame
      movedToOrbit = advanceGame 0.7 (moving 0 (-1)) movedLeft
      oneOrbit = 2 * pi / 0.8
      waitedOneOrbit = advanceGame oneOrbit (moving 0 0) movedToOrbit
  unless (gameScore movedToOrbit == 0) $
    fail
      ( "positioning the player for the orbit regression collected a crystal early: "
          <> show (gameScore movedLeft, gameScore movedToOrbit)
      )
  unless (gameScore waitedOneOrbit == 1) $
    fail "a crystal crossing the player during a full orbit was not collected"

cameraKeepsThePlayerInsideTheClipVolume :: IO ()
cameraKeepsThePlayerInsideTheClipVolume = do
  let viewProjection = gameViewProjection (960, 540) (gamePlayerPosition initialGame)
      V4 clipX clipY clipZ clipW = m44MulV4 viewProjection (V4 0 0.48 0 1)
      insideHorizontal = abs clipX <= clipW && abs clipY <= clipW
      insideDepth = clipZ >= 0 && clipZ <= clipW
  unless (clipW > 0 && insideHorizontal && insideDepth) $
    fail ("player is outside the clip volume: " <> show (clipX, clipY, clipZ, clipW))

cameraHandlesAZeroSizedRenderTarget :: IO ()
cameraHandlesAZeroSizedRenderTarget = do
  let viewProjection = gameViewProjection (0, 0) (gamePlayerPosition initialGame)
      V4 clipX clipY clipZ clipW = m44MulV4 viewProjection (V4 0 0.48 0 1)
      clipCoordinates = [clipX, clipY, clipZ, clipW]
  unless (all isFinite clipCoordinates) $
    fail ("zero-sized render target produced non-finite clip coordinates: " <> show clipCoordinates)

resetRestoresTheInitialGame :: IO ()
resetRestoresTheInitialGame = do
  let moved = advanceGame 1 (moving 1 0) initialGame
      resetState = advanceGame 0 (GameInput (MoveDirection 0 0) True) moved
  unless (resetState == initialGame) $
    fail "reset did not restore the initial game"

invalidFrameDurationDoesNotChangeTheGame :: IO ()
invalidFrameDurationDoesNotChangeTheGame = do
  let state = advanceGame (0 / 0) (moving 1 0) initialGame
  unless (state == initialGame) $
    fail "NaN frame duration changed the game"
  unless (length (activeCrystals state) == 5) $
    fail "invalid frame duration changed active crystals"

moving :: Float -> Float -> GameInput
moving x z =
  GameInput
    { move = MoveDirection x z
    , reset = False
    }

assertApprox :: String -> Float -> Float -> IO ()
assertApprox label expected actual =
  unless (abs (expected - actual) < 0.0001) $
    fail
      ( label
          <> ": expected "
          <> show expected
          <> ", got "
          <> show actual
      )

isFinite :: Float -> Bool
isFinite value = not (isNaN value || isInfinite value)
