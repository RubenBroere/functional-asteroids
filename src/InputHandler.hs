{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module InputHandler where

import Graphics.Gloss.Interface.IO.Game
import Data.Set (insert, delete, member)
import Data

handleInput :: Event -> GameData -> GameData
handleInput (EventKey k Down _ _) gd@GameData{ pressedKeys } = gd { pressedKeys = insert k pressedKeys }
handleInput (EventKey k Up _ _) gd@GameData{ pressedKeys } = handleKeyUp k gd { pressedKeys = delete k pressedKeys }
handleInput _ gd = gd -- Ignore non-keypresses for simplicity

isAnyPressed :: [Key] -> GameData -> Bool
isAnyPressed keys gd = any (`member` pressedKeys gd) keys

handleKeyUp :: Key -> GameData -> GameData
handleKeyUp (Char 'p') gd@GameData{ gameState=Game } = gd { gameState=Paused }
handleKeyUp (SpecialKey KeySpace) gd = gd { gameState=Game }
handleKeyUp _ gd = gd

pressForward :: GameData -> Bool
pressForward = isAnyPressed [Char 'w', SpecialKey KeyUp]

pressLeft :: GameData -> Bool
pressLeft = isAnyPressed [Char 'a', SpecialKey KeyLeft]

pressRight :: GameData -> Bool
pressRight = isAnyPressed [Char 'd', SpecialKey KeyRight]

pressShoot :: GameData -> Bool
pressShoot = isAnyPressed [SpecialKey KeySpace]
