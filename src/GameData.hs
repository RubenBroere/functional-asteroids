module GameData (GameData (..), makeGameData) where

import Kinematics
import Player
import Data.Set (Set, empty)
import Graphics.Gloss.Interface.IO.Game (Key)

type Score    = Int
type Time     = Float

data GameData = GameData
    { pressedKeys :: Set Key 
    , score       :: Score
    , player      :: Player
    , asteroids   :: [Asteroid]
    , ufos        :: [Ufo]
    , bullets     :: [Bullet]
    , elapsedTime :: Time
    , gameState   :: GameState
    }

makeGameData :: GameData
makeGameData = GameData
    { pressedKeys = empty 
    , score = 0
    , player = makePlayer 
    , asteroids = []
    , ufos = []
    , bullets = []
    , elapsedTime = 0
    , gameState = Paused 
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
