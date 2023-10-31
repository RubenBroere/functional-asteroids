{-# LANGUAGE NamedFieldPuns #-}
module Bullet (Bullet, makeBullet, updateBullet, updateBullets) where

import Types
import Kinematics

data Bullet = Bullet
    { lifeTime         :: Time
    , bulletKinematics :: KinematicInfo
    }

instance Body Bullet where
    position = position . bulletKinematics
    velocity = velocity . bulletKinematics

makeBullet :: Vector -> Vector -> Float -> Bullet
makeBullet pos vel angle =
    Bullet {lifeTime=30, bulletKinematics=setVelocity bulletVel $ makeKinematics bulletPos} 
    where
        bulletVel = calcVelocity angle vel
        bulletPos = calcPosition angle pos

calcVelocity :: Float -> Vector -> Vector 
calcVelocity angle (dx, dy) = (dx + baseVelocity * sin angle, dy + baseVelocity * cos angle)
    where
        baseVelocity = 1000

calcPosition :: Float -> Vector -> Vector 
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
