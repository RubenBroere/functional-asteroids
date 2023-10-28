{-# LANGUAGE NamedFieldPuns #-}

module InputHandler (handleInput, isKeyPressed) where

import Graphics.Gloss.Interface.IO.Game
import Data.Set (insert, delete, member) 
import GameData

handleInput :: Event -> GameData -> GameData
handleInput (EventKey k Down _ _) gd@GameData{ pressedKeys } = gd { pressedKeys = insert k pressedKeys }
handleInput (EventKey k Up _ _) gd@GameData{ pressedKeys } = gd { pressedKeys = delete k pressedKeys }
handleInput _ gd = gd -- Ignore non-keypresses for simplicity

isKeyPressed :: Char -> GameData -> Bool
isKeyPressed key gd = member (Char key) (pressedKeys gd)
