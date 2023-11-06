{-# LANGUAGE NamedFieldPuns #-}
module Player 
    ( Player
    , makePlayer
    , handleBounds
    , handleDeath
    , hasSpawnProtection
    , rotatePlayer
    , acceleratePlayer
    , deceleratePlayer
    , degradeRespawnShield
    , getLives
    , rotation
    , reloading
    , shoot 
    ) where

import Types
import Kinematics
import Renderer
import Graphics.Gloss hiding(rotate) 
import Sprites

data Player = Player
    { lives            :: Int
    , angle            :: Float
    , playerKinematics :: KinematicInfo
    , respawnShield :: Time
    , reloadTime :: Time
    }

instance Drawable Player where
    draw player = translateBody player $ rotateRadian (angle player) $ Color spriteColor playerSprite
        where
            spriteColor | hasSpawnProtection player = greyN 0.4
                        | otherwise = white

instance Body Player where
    position = position . playerKinematics
    velocity = velocity . playerKinematics
    update = updatePosition
    size _ = 15

makePlayer :: Player
makePlayer = Player
    { lives=3
    , angle=0
    , playerKinematics=makeKinematics (0, 0)
    , respawnShield=10
    , reloadTime=0
    }

getLives :: Player -> Int
getLives = lives

rotation :: Player -> Float
rotation = angle

reloading :: Player -> Bool
reloading = (> 0) . reloadTime 

shoot :: Player -> Player
shoot p = p { reloadTime=50 }

-- Movement

rotatePlayer :: Float -> Time -> Player -> Player
rotatePlayer speed dt p = p{ angle=angle p + speed * dt }

handleBounds :: (Int, Int) -> Player -> Player
handleBounds (width, height) p@Player{ playerKinematics=kin }
    | rx < -hWidth = p{ playerKinematics=setPosition (x + fromIntegral width, y) kin }
    | rx > hWidth = p{ playerKinematics=setPosition (x - fromIntegral width, y) kin }
    | ry < -hHeight = p{ playerKinematics=setPosition (x, y + fromIntegral height) kin }
    | ry > hHeight = p{ playerKinematics=setPosition (x, y - fromIntegral height) kin }
    | otherwise = p 
    where
        (x, y) = position p
        (rx, ry) = (round x, round y)
        (hWidth, hHeight) = (width `div` 2, height `div` 2)

deceleratePlayer :: Time -> Player -> Player
deceleratePlayer dt player = player{ playerKinematics=decelerate deceleration $ playerKinematics player }
    where
        deceleration = dt * 200

acceleratePlayer :: Time -> Player -> Player
acceleratePlayer dt player@Player{ playerKinematics, angle } =
    player{ playerKinematics=setVelocity newVelocity playerKinematics }
    where
        newVelocity = (x + speed * dt * sin angle, y + speed * dt * cos angle) 
        (x, y) = velocity player
        speed = 500

updatePosition :: Time -> Player -> Player
updatePosition dt player@Player{ playerKinematics } = degradeRespawnShield dt $ player{ playerKinematics=newKinematics, reloadTime=max (reloadTime player - dt * 100) 0 }
    where
        newKinematics = clampVelocity maxVelocity $ updateKinematics dt playerKinematics
        maxVelocity = 300

-- Respawning 

handleDeath :: Player -> Player
handleDeath p   | hasSpawnProtection p = p
                | otherwise = makePlayer{ lives=lives p - 1 }

degradeRespawnShield :: Time -> Player -> Player
degradeRespawnShield dt player@Player{ respawnShield=s } = player{ respawnShield=max 0 $ s - dt * degrationSpeed } 
    where
        degrationSpeed = 10

hasSpawnProtection :: Player -> Bool
hasSpawnProtection = (> 0) . respawnShield

