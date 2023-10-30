{-# LANGUAGE NamedFieldPuns #-}
module Bullet (makeBullet, updateBullet, updateBullets) where

import Data
import Kinematics

makeBullet :: Player -> Bullet
makeBullet Player{ angle, playerKinematics=KinematicInfo{ velocity, position } } =
    Bullet {lifeTime=30, bulletKinematics=KinematicInfo {velocity=calcVelocity angle velocity, position=calcPosition angle position}}

calcVelocity :: Float -> (Float, Float) -> (Float, Float)
calcVelocity angle (dx, dy) = (dx + baseVelocity * sin angle, dy + baseVelocity * cos angle)
    where
        baseVelocity = 1000

calcPosition :: Float -> (Float, Float) -> (Float, Float)
calcPosition angle (x, y) = (x + noseDistance * sin angle, y + noseDistance * cos angle)
    where
        noseDistance = 30

updateBullet :: Float -> Bullet -> Bullet
updateBullet dt bullet@Bullet{ bulletKinematics, lifeTime } = 
    bullet{ bulletKinematics=updateKinematics dt bulletKinematics, lifeTime=lifeTime-dt*degradationSpeed}
    where
        degradationSpeed = 20

updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets dt bullets = filter (\x -> lifeTime x > 0) $ map (updateBullet dt) bullets 
