module Renderer (translateBody, rotateRadian, positionTopLeft) where

import Graphics.Gloss

import Types

translateBody :: Body a => a -> Picture -> Picture
translateBody body = uncurry Translate (position body)

rotateRadian :: Float -> Picture -> Picture
rotateRadian theta = Rotate (theta * (180/pi))

positionTopLeft :: (Int, Int) -> Picture -> Picture
positionTopLeft (x, y) = Translate (-fromIntegral x/2) (fromIntegral y/2)

{-
drawGame :: World -> Picture
drawGame gd@World { player, bullets, asteroids } = Pictures [drawBanner $ gameState gd, drawHud gd, drawPlayer player, drawBullets bullets, drawAsteroids asteroids]

drawHud :: World -> Picture
drawHud gd = positionTopLeft (worldSize gd) $ Pictures [drawLives . getLives $ player gd]

drawBanner :: GameState -> Picture
drawBanner GameOver = Translate (-300) 100 $ Color white $ Text "Game over"
drawBanner Paused = Translate (-200) 100 $ Color white $ Text "Paused"
drawBanner _ = Blank

drawLives :: Int -> Picture
drawLives lives = Color white $ Pictures [Translate (15 + fromIntegral x*20) (-20) $ Scale 0.5 0.5 playerSprite | x <- [0..(lives-1)]]
-}
