module World (World(..), makeWorld, updateGame) where

import Data
import System.Random

import Player
import Types
import InputHandler
import Asteroid
import Bullet

data World = World
    { pressedKeys :: PressedKeys 
    , worldSize  :: (Int, Int)
    , stdGen      :: StdGen
    , score       :: Score
    , player      :: Player
    , asteroids   :: [Asteroid]
    , ufos        :: [Ufo]
    , bullets     :: [Bullet]
    , elapsedTime :: Time
    , gameState   :: GameState
    }

makeWorld :: World
makeWorld = World
    { pressedKeys = makeKeys 
    , worldSize = (1280, 720)
    , stdGen = mkStdGen 42
    , score = 0
    , player = makePlayer 
    , asteroids = []
    , ufos = []
    , bullets = []
    , elapsedTime = 0
    , gameState = Paused 
    }

updateGame :: Time -> World -> World
updateGame dt world = undefined

updatePlayer :: Time -> World -> World
updatePlayer dt world = world { player=updatePosition dt $ player world }

updateAsteroids :: Time -> World -> World
updateAsteroids dt world = world { asteroids=Asteroid.updateAsteroids dt $ asteroids world }

