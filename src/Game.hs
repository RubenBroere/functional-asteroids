{-# LANGUAGE NamedFieldPuns #-}
module Game ( run ) where

import Graphics.Gloss
import Data
import GameData (makeGameData) 
import InputHandler (handleInput)
import Renderer (drawGame)
import Player

run :: IO ()
run = play screen black 60 makeGameData drawGame handleInput updateGame
    where
        screen = InWindow "Functional Asteroid" (1280, 720) (0, 0)

updateGame :: Float -> GameData -> GameData
updateGame dt gd = updatePlayer dt gd 
