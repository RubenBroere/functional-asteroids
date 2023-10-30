{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data where

import Data.Set (Set)
import Graphics.Gloss.Interface.IO.Game (Key)
import Graphics.Gloss

type Score    = Int
type Time     = Float

class Drawable a where
    draw :: a -> Picture

class Physics a where
    applyForce :: Float -> a -> a
    updatePosition :: Float -> a -> a


data GameData = GameData
    { pressedKeys :: Set Key 
    , worldSize  :: (Int, Int)
    , score       :: Score
    , player      :: Player
    , asteroids   :: [Asteroid]
    , ufos        :: [Ufo]
    , bullets     :: [Bullet]
    , elapsedTime :: Time
    , gameState   :: GameState
    }

data Player = Player
    { lives            :: Int
    , angle            :: Float
    , playerKinematics :: KinematicInfo
    }

data KinematicInfo = KinematicInfo
    { velocity :: Vector
    , position :: Vector
    }

data GameState = Game | Paused | GameOver
    deriving (Eq)

data Asteroid = Asteroid
    { size               :: AsteroidSize
    , asteroidKinematics :: KinematicInfo
    }

data AsteroidSize = Small | Medium | Large
    deriving (Eq, Ord, Enum)

data Ufo = Ufo
    { aimTarget     :: AimTarget
    , ufoKinematics :: KinematicInfo
    }

data AimTarget = TargetForward | TargetPlayer
    deriving (Eq)

data Bullet = Bullet
    { lifeTime         :: Time
    , bulletKinematics :: KinematicInfo
    }
