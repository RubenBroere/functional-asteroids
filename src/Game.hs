module Game ( run ) where

import Graphics.Gloss
import World
import Types

run :: IO ()
run = play screen black 120 (makeWorld screenSize) draw handleEvents updateWorld
    where
        screenSize = (1280, 720)
        screen = InWindow "Functional Asteroid" screenSize (0, 0)
