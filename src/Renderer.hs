{-# language NamedFieldPuns #-}

module Renderer (drawGame) where

import Data
import Graphics.Gloss

playerSprite :: Picture
playerSprite = Color white $ Pictures [Line [(-15, -20), (0, 30)], Line [(15, -20), (0, 30)], Line [(10, -5), (-10, -5)]]

translateKin :: KinematicInfo -> Picture -> Picture
translateKin KinematicInfo{ position } = uncurry Translate position

rotateRadian :: Float -> Picture -> Picture
rotateRadian theta = Rotate (theta * (180/pi))

positionTopLeft :: (Int, Int) -> Picture -> Picture
positionTopLeft (x, y) = Translate (-fromIntegral x/2) (fromIntegral y/2)

drawGame :: GameData -> Picture
drawGame gd@GameData { player, bullets } = Pictures [drawBanner $ gameState gd, drawHud gd, drawPlayer player, drawBullets bullets]

drawHud :: GameData -> Picture
drawHud gd = positionTopLeft (worldSize gd) $ Pictures [drawLives . lives $ player gd]

drawBanner :: GameState -> Picture
drawBanner GameOver = Translate (-300) 100 $ Color white $ Text "Game over"
drawBanner Paused = Translate (-200) 100 $ Color white $ Text "Paused"
drawBanner _ = Blank 

drawLives :: Int -> Picture
drawLives lives = Pictures [Translate (15 + fromIntegral x*20) (-20) $ Scale 0.5 0.5 playerSprite | x <- [0..(lives-1)]]

drawPlayer :: Player -> Picture
drawPlayer Player{ playerKinematics=kin, angle } = translateKin kin $ rotateRadian angle playerSprite

drawBullets :: [Bullet] -> Picture
drawBullets = Pictures . map drawBullet

drawBullet :: Bullet -> Picture
drawBullet Bullet{ bulletKinematics=kin } = translateKin kin $ Color green $ Circle 2
