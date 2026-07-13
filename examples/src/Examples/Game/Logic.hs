{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Pure state transitions for the crystal collection example.
module Examples.Game.Logic
  ( GroundPosition(..)
  , MoveDirection(..)
  , GameInput(..)
  , CrystalId
  , Crystal(..)
  , GameState
  , arenaHalfExtent
  , initialGame
  , advanceGame
  , gamePlayerPosition
  , gameScore
  , activeCrystals
  ) where

data GroundPosition = GroundPosition
  { x :: !Float
  , z :: !Float
  } deriving (Eq, Show)

data MoveDirection = MoveDirection
  { x :: !Float
  , z :: !Float
  } deriving (Eq, Show)

data GameInput = GameInput
  { move :: !MoveDirection
  , reset :: !Bool
  } deriving (Eq, Show)

newtype CrystalId = CrystalId Int
  deriving (Eq, Show)

data Crystal = Crystal
  { id :: !CrystalId
  , pos :: !GroundPosition
  , rotation :: !Float
  } deriving (Eq, Show)

data GameState = GameState
  { player :: !GroundPosition
  , elapsed :: !Float
  , score :: !Int
  , collected :: ![CrystalId]
  } deriving (Eq, Show)

data CrystalSeed = CrystalSeed
  { id :: !CrystalId
  , center :: !GroundPosition
  , orbitRadius :: !Float
  , phase :: !Float
  }

arenaHalfExtent :: Float
arenaHalfExtent = 8

playerSpeed :: Float
playerSpeed = 5

collectionRadius :: Float
collectionRadius = 0.9

crystalAngularSpeed :: Float
crystalAngularSpeed = 0.8

crystalSeeds :: [CrystalSeed]
crystalSeeds =
  [ CrystalSeed (CrystalId 0) (GroundPosition (-4.5) (-3.5)) 0.7 0.0
  , CrystalSeed (CrystalId 1) (GroundPosition (-1.5) 3.8) 0.9 1.2
  , CrystalSeed (CrystalId 2) (GroundPosition 2.8 (-2.8)) 0.8 2.4
  , CrystalSeed (CrystalId 3) (GroundPosition 4.7 3.1) 0.6 3.6
  , CrystalSeed (CrystalId 4) (GroundPosition 0.2 2.6) 1.0 4.8
  ]

initialGame :: GameState
initialGame =
  GameState
    { player = GroundPosition 0 0
    , elapsed = 0
    , score = 0
    , collected = []
    }

-- | Advances one frame. Invalid frame durations leave the game unchanged.
advanceGame :: Float -> GameInput -> GameState -> GameState
advanceGame frameSeconds input state
  | input.reset = initialGame
  | not (isValidFrameSeconds frameSeconds) = state
  | otherwise =
      GameState
        { player = nextPlayerPosition
        , elapsed = nextElapsed
        , score = state.score + length newlyCollected
        , collected = state.collected <> newlyCollected
        }
  where
    nextElapsed = state.elapsed + frameSeconds
    nextPlayerPosition = movePlayer frameSeconds input.move state.player
    newlyCollected =
      fmap (.id)
        (filter (isWithinCollectionRange nextPlayerPosition) (activeCrystalsAt nextElapsed state.collected))

gamePlayerPosition :: GameState -> GroundPosition
gamePlayerPosition = (.player)

gameScore :: GameState -> Int
gameScore = (.score)

activeCrystals :: GameState -> [Crystal]
activeCrystals state = activeCrystalsAt state.elapsed state.collected

activeCrystalsAt :: Float -> [CrystalId] -> [Crystal]
activeCrystalsAt gameTime collectedIds =
  filter (not . isCollected collectedIds . (.id)) (fmap (crystalAt gameTime) crystalSeeds)

crystalAt :: Float -> CrystalSeed -> Crystal
crystalAt gameTime seed =
  Crystal
    { id = seed.id
    , pos =
        GroundPosition
          { x = seed.center.x + seed.orbitRadius * cos angle
          , z = seed.center.z + seed.orbitRadius * sin angle
          }
    , rotation = angle * 2
    }
  where
    angle = seed.phase + gameTime * crystalAngularSpeed

movePlayer :: Float -> MoveDirection -> GroundPosition -> GroundPosition
movePlayer frameSeconds direction position
  | not (isFinite direction.x && isFinite direction.z) = position
  | directionMagnitude == 0 = position
  | otherwise =
      GroundPosition
        { x = clampArena (position.x + distance * direction.x / directionMagnitude)
        , z = clampArena (position.z + distance * direction.z / directionMagnitude)
        }
  where
    directionMagnitude = sqrt (direction.x * direction.x + direction.z * direction.z)
    distance = playerSpeed * frameSeconds

isWithinCollectionRange :: GroundPosition -> Crystal -> Bool
isWithinCollectionRange playerPosition crystal =
  deltaX * deltaX + deltaZ * deltaZ <= collectionRadius * collectionRadius
  where
    deltaX = playerPosition.x - crystal.pos.x
    deltaZ = playerPosition.z - crystal.pos.z

isCollected :: [CrystalId] -> CrystalId -> Bool
isCollected collectedIds crystalId = crystalId `elem` collectedIds

clampArena :: Float -> Float
clampArena coordinate = max (-arenaHalfExtent) (min arenaHalfExtent coordinate)

isValidFrameSeconds :: Float -> Bool
isValidFrameSeconds seconds = seconds >= 0 && isFinite seconds

isFinite :: Float -> Bool
isFinite value = not (isNaN value || isInfinite value)
