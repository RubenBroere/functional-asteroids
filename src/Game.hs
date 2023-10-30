{-# LANGUAGE NamedFieldPuns #-}
module Game ( run ) where

import Graphics.Gloss
import Data
import GameData
import InputHandler
import Renderer (drawGame)
import Player
import Bullet
import Graphics.Gloss.Interface.IO.Game

run :: IO ()
run = play screen black 120 makeGameData drawGame handleEvents updateGame
    where
        gameData = makeGameData
        screenSize = worldSize gameData
        screen = InWindow "Functional Asteroid" screenSize (0, 0)

updateGame :: Float -> GameData -> GameData
updateGame _ gd@GameData{ gameState=Paused } = gd 
updateGame dt gd = updateComponents dt $ updatePlayer dt gd 


handleEvents :: Event -> GameData -> GameData
handleEvents (EventResize size) gd = gd { worldSize = size }
handleEvents event gd = handleInput event gd

updateComponents :: Float -> GameData -> GameData
updateComponents dt gd@GameData{ bullets } = gd { bullets=updateBullets dt bullets }
