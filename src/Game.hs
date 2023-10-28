module Game ( run ) where

import Graphics.Gloss
import GameData (GameData, makeGameData, player)
import InputHandler (handleInput, isKeyPressed)
import Renderer (drawGame)
import Player

run :: IO ()
run = play screen black 60 makeGameData drawGame handleInput updateGame
    where
        screen = InWindow "Functional Asteroid" (1280, 720) (0, 0)

updateGame :: Float -> GameData -> GameData
updateGame _ gd | isKeyPressed 'w' gd = gd { player = acceleratePlayer (player gd) }
                | otherwise = gd
