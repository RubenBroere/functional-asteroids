{-# LANGUAGE NamedFieldPuns #-}
module Bullet (Bullet, makeBullet, updateBullet, isExpired) where

import Types
import Kinematics
import Graphics.Gloss (Picture (..))
import Graphics.Gloss.Data.Color
import Renderer

data Bullet = Bullet
    { lifeTime         :: Time
    , bulletKinematics :: KinematicInfo
    }

instance Body Bullet where
    position = position . bulletKinematics
    velocity = velocity . bulletKinematics

instance Drawable Bullet where
    draw b = translateBody b $ Color white $ Circle 2

makeBullet :: Vector -> Vector -> Float -> Bullet
makeBullet pos vel angle =
    Bullet {lifeTime=30, bulletKinematics=setVelocity bulletVel $ makeKinematics bulletPos} 
    where
        bulletVel = calcVelocity angle vel
        bulletPos = calcPosition angle pos

updateBullet :: Float -> Bullet -> Bullet
updateBullet dt bullet@Bullet{ bulletKinematics, lifeTime } = 
    bullet{ bulletKinematics=updateKinematics dt bulletKinematics, lifeTime=lifeTime-dt*degradationSpeed}
    where
        degradationSpeed = 20

isExpired :: Bullet -> Bool
isExpired = (< 0) . lifeTime

calcVelocity :: Float -> Vector -> Vector 
calcVelocity angle (dx, dy) = (dx + baseVelocity * sin angle, dy + baseVelocity * cos angle)
    where
        baseVelocity = 1000

calcPosition :: Float -> Vector -> Vector 
calcPosition angle (x, y) = (x + noseDistance * sin angle, y + noseDistance * cos angle)
    where
        noseDistance = 30

