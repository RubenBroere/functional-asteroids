{-# LANGUAGE NamedFieldPuns #-}
module Bullet (Bullet, makeBullet, isExpired, isPlayerBullet) where

import Types
import Kinematics
import Graphics.Gloss (Picture (..), circleSolid)
import Graphics.Gloss.Data.Color
import Renderer

data Bullet = Bullet
    { lifeTime         :: Time
    , kinematics :: KinematicInfo
    , isPlayer :: Bool
    }

instance Body Bullet where
    position = position . kinematics
    velocity = velocity . kinematics
    update = updateBullet
    size _ = 2

instance Drawable Bullet where
    draw b = translateBody b $ Color white $ circleSolid 2

makeBullet :: Bool -> Vector -> Vector -> Float -> Bullet
makeBullet player pos vel angle =
    Bullet {lifeTime=30, kinematics=setVelocity bulletVel $ makeKinematics bulletPos, isPlayer=player }
    where
        bulletVel = calcVelocity angle vel
        bulletPos = calcPosition angle pos

updateBullet :: Float -> Bullet -> Bullet
updateBullet dt bullet@Bullet{ kinematics, lifeTime } =
    bullet{ kinematics=updateKinematics dt kinematics, lifeTime=max (lifeTime-dt*degradationSpeed) 0}
    where
        degradationSpeed = 50

isExpired :: Bullet -> Bool
isExpired = (== 0) . lifeTime

isPlayerBullet :: Bullet -> Bool
isPlayerBullet = isPlayer

calcVelocity :: Float -> Vector -> Vector 
calcVelocity angle (dx, dy) = (dx + baseVelocity * sin angle, dy + baseVelocity * cos angle)
    where
        baseVelocity = 1000

calcPosition :: Float -> Vector -> Vector 
calcPosition angle (x, y) = (x + noseDistance * sin angle, y + noseDistance * cos angle)
    where
        noseDistance = 30

