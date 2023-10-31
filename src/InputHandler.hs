module InputHandler (PressedKeys, makeKeys, handleInput, isAnyPressed, pressForward, pressLeft, pressRight, pressShoot) where

import Graphics.Gloss.Interface.IO.Game
import Data.Set (insert, delete, member, Set)

-- Hiding implementation
newtype PressedKeys = Keys (Set Key)

makeKeys :: PressedKeys
makeKeys = Keys mempty

handleInput :: Event -> PressedKeys -> PressedKeys
handleInput (EventKey k Down _ _) (Keys set) = Keys (insert k set)
handleInput (EventKey k Up _ _) (Keys set) = Keys (delete k set)
handleInput _ keys = keys -- Ignore non-keypresses for simplicity

isAnyPressed :: [Key] -> PressedKeys -> Bool
isAnyPressed keys (Keys set) = any (`member` set) keys

--handleKeyUp :: Key -> Set Key -> Set Key
--handleKeyUp (Char 'p') pressedKeys = gd { gameState=Paused }
--handleKeyUp (SpecialKey KeySpace) gd = gd { gameState=Game }
--handleKeyUp (SpecialKey KeyEnter) gd = gd { asteroids=asteroid:asteroids gd, stdGen=newGen }
--    where
--        (asteroid, newGen) = generateAsteroid (worldSize gd) (stdGen gd)
-- handleKeyUp _ gd = gd

pressForward :: PressedKeys -> Bool
pressForward = isAnyPressed [Char 'w', SpecialKey KeyUp]

pressLeft :: PressedKeys -> Bool
pressLeft = isAnyPressed [Char 'a', SpecialKey KeyLeft]

pressRight :: PressedKeys -> Bool
pressRight = isAnyPressed [Char 'd', SpecialKey KeyRight]

pressShoot :: PressedKeys -> Bool
pressShoot = isAnyPressed [SpecialKey KeySpace]

