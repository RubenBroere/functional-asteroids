{-# language NamedFieldPuns #-}

module Renderer (drawGame) where

import Graphics.Gloss

import Types
import Data
import Asteroid
import Bullet
import Player
import World

playerSprite :: Picture
playerSprite = Pictures [Line [(-15, -20), (0, 30)], Line [(15, -20), (0, 30)], Line [(10, -5), (-10, -5)]]

translateBody :: Body a => a -> Picture -> Picture
translateBody body = uncurry Translate (position body)

rotateRadian :: Float -> Picture -> Picture
rotateRadian theta = Rotate (theta * (180/pi))

positionTopLeft :: (Int, Int) -> Picture -> Picture
positionTopLeft (x, y) = Translate (-fromIntegral x/2) (fromIntegral y/2)

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

drawPlayer :: Player -> Picture
drawPlayer player = translateBody player $ rotateRadian (rotation player) $ Color spriteColor playerSprite
    where
        spriteColor | hasSpawnProtection player = greyN 0.4
                    | otherwise = white

drawBullets :: [Bullet] -> Picture
drawBullets = Pictures . map drawBullet

drawBullet :: Bullet -> Picture
drawBullet bullet = translateBody bullet $ Color white $ Circle 2

drawAsteroids :: [Asteroid] -> Picture
drawAsteroids = Pictures . map drawAsteroid

drawAsteroid :: Asteroid -> Picture
drawAsteroid asteroid = translateBody asteroid $ Color white $ Circle $ getAsteroidSize asteroid 

