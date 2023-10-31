{-# LANGUAGE NamedFieldPuns #-}
module Game ( run ) where

import Graphics.Gloss
import Data
import InputHandler
import Renderer (drawGame)
import Player
import Bullet
import Graphics.Gloss.Interface.IO.Game
import World

run :: IO ()
run = play screen black 120 makeWorld drawGame handleEvents Game.updateGame
    where
        gameData = makeWorld
        screenSize = worldSize gameData
        screen = InWindow "Functional Asteroid" screenSize (0, 0)

updateGame :: Float -> World -> World
updateGame _ w@World{ gameState=Paused } = w 
updateGame _ w@World{ gameState=GameOver } = w 
updateGame dt w = updateComponents dt w 

-- Input handling

handleEvents :: Event -> World -> World
handleEvents (EventResize size) w = w { worldSize = size }
handleEvents event@(EventKey k _ _ _) w@World{ pressedKeys } = handleKeyUps k $ w{ pressedKeys=handleInput event pressedKeys }
handleEvents _ w = w

handleKeyUps :: Key -> World -> World
handleKeyUps (SpecialKey KeySpace) w = w { gameState=Game }
handleKeyUps _ w = w

updateComponents :: Float -> World -> World
updateComponents _ w@World{ player }
    | getLives player == 0 = w { gameState=GameOver }
    | otherwise = w
updateComponents dt w@World{ bullets, elapsedTime } = w { bullets=updateBullets dt bullets, elapsedTime=elapsedTime + dt }
