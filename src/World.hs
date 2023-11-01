{-# LANGUAGE NamedFieldPuns #-}
module World (World(..), GameState(..), makeWorld, updateWorld) where

import System.Random
import Graphics.Gloss

import Player
import Types
import InputHandler
import Asteroid
import Bullet
import Ufo

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

data GameState = Game | Paused | GameOver
    deriving (Eq)

instance Drawable World where
    draw w = Pictures [draw $ player w, Pictures $ map draw $ asteroids w, Pictures $ map draw $ bullets w]

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

updateWorld :: Time -> World -> World
updateWorld _ w@World{ gameState=Paused } = w 
updateWorld _ w@World{ gameState=GameOver } = w 
updateWorld dt world = updateComponents dt world [updatePlayer, updateAsteroids, updateBullets, updateElapsedTime, updateGameState]

updateComponents :: Time -> World -> [Time -> World -> World] -> World
updateComponents dt = foldl (\x y -> y dt x)

updatePlayer :: Time -> World -> World
updatePlayer dt world = world { player=updatePosition dt $ player world }

updateAsteroids :: Time -> World -> World
updateAsteroids dt world = world { asteroids=map (updateAsteroid dt) $ asteroids world }

updateElapsedTime :: Time -> World -> World
updateElapsedTime dt world@World{ elapsedTime } = world{ elapsedTime=elapsedTime + dt }

updateBullets :: Float -> World -> World
updateBullets dt w@World{ bullets } = w{ bullets=filter (not . isExpired) $ map (updateBullet dt) bullets }

updateGameState :: Float -> World -> World
updateGameState _ w@World{ player }
    | getLives player == 0 = w { gameState=GameOver }
    | otherwise = w

