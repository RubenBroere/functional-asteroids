module Sprites (playerSprite, ufoSprite) where

import Graphics.Gloss

playerSprite :: Picture
playerSprite = Pictures [Line [(-15, -20), (0, 30), (15, -20)], Line [(10, -5), (-10, -5)]]

ufoSprite :: Picture
ufoSprite = Pictures [Line [(-20, -5), (-15, 0), (15, 0), (20, -5), (15, -10), (-15, -10), (-20, -5)], Translate 0 7 $ Arc (-20) 200 10]
