{-# LANGUAGE NamedFieldPuns #-}
module Game ( run ) where

import Graphics.Gloss
import InputHandler
import Graphics.Gloss.Interface.IO.Game
import World
import Types

run :: IO ()
run = play screen black 120 makeWorld draw handleEvents Game.updateGame
    where
        gameData = makeWorld
        screenSize = worldSize gameData
        screen = InWindow "Functional Asteroid" screenSize (0, 0)

updateGame :: Float -> World -> World
updateGame = updateWorld

-- Input handling

handleEvents :: Event -> World -> World
handleEvents (EventResize size) w = w { worldSize = size }
handleEvents event@(EventKey k _ _ _) w@World{ pressedKeys } = handleKeyUps k $ w{ pressedKeys=handleInput event pressedKeys }
handleEvents _ w = w

handleKeyUps :: Key -> World -> World
handleKeyUps (SpecialKey KeySpace) w = w { gameState=Game }
handleKeyUps _ w = w

