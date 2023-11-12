module Renderer (translateBody, rotateRadian, topLeft, topRight, bottomLeft) where

import Graphics.Gloss

import Types

translateBody :: Body a => a -> Picture -> Picture
translateBody body = uncurry Translate (position body)

rotateRadian :: Float -> Picture -> Picture
rotateRadian theta = Rotate (theta * (180/pi))

topLeft :: (Int, Int) -> Picture -> Picture
topLeft (x, y) = Translate (-fromIntegral x/2) (fromIntegral y/2)

topRight :: (Int, Int) -> Picture -> Picture
topRight (x, y) = Translate (fromIntegral x/2) (fromIntegral y/2)

bottomLeft :: (Int, Int) -> Picture -> Picture
bottomLeft (x, y) = Translate (-fromIntegral x/2) (-fromIntegral y/2)
