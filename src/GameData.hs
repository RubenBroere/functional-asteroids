module GameData  where

import Data
import Data.Set (empty)
import Player

makeGameData :: GameData
makeGameData = GameData
    { pressedKeys = empty 
    , worldSize = (1280, 720)
    , score = 0
    , player = makePlayer 
    , asteroids = []
    , ufos = []
    , bullets = []
    , elapsedTime = 0
    , gameState = Paused 
    }

isPaused :: GameData -> Bool
isPaused gd = gameState gd == Paused
