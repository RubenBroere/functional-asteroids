module Game ( run ) where

import Graphics.Gloss

run :: IO ()
run = display (InWindow "Functional Asteroids" (200, 200) (10, 10)) black (Circle 80)
