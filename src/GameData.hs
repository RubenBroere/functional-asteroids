module GameData  where

import Data
import Data.Set (empty)
import Player

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
