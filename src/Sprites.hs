module Sprites (playerSprite) where

import Graphics.Gloss

playerSprite :: Picture
playerSprite = Pictures [Line [(-15, -20), (0, 30)], Line [(15, -20), (0, 30)], Line [(10, -5), (-10, -5)]]
