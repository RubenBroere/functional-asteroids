{-# LANGUAGE NamedFieldPuns #-}
module Player (Player, makePlayer, handleBounds, handleDeath, hasSpawnProtection, rotate, updatePosition, acceleratePlayer, deceleratePlayer, degradeRespawnShield, rotation, getLives) where

import Types
import Kinematics

data Player = Player
    { lives            :: Int
    , angle            :: Float
    , playerKinematics :: KinematicInfo
    , respawnShield :: Time
    }

instance Body Player where
    position = position . playerKinematics
    velocity = velocity . playerKinematics

makePlayer :: Player
makePlayer = Player {lives=3, angle=0, playerKinematics=makeKinematics (0, 0), respawnShield=10}

{-
updatePlayer :: Time -> GameData -> GameData
updatePlayer dt gd = handleBounds . handleDeath . degradeRespawnShield dt $ updatePosition dt $ updateInput dt gd

updateInput :: Time -> GameData -> GameData
updateInput _ gd@GameData{ gameState=Paused } = gd

updateInput dt gd = foldr applyIfPressed gd inputActions
    where
        applyIfPressed :: (GameData -> Bool, Time -> GameData -> GameData) -> GameData -> GameData
        applyIfPressed (isPressed, action) accData
            | isPressed gd = action dt accData
            | otherwise = accData

inputActions :: [(GameData -> Bool, Time -> GameData -> GameData)]
inputActions = [
    (pressForward, forward),
    (pressRight, rotate pi),
    (pressLeft, rotate (-pi)),
    (pressShoot, shoot)
    ]
-}
-- shoot :: Time -> Player -> GameData
-- shoot _ gd@GameData{ player, bullets }  | any (\x -> lifeTime x > 25) bullets = gd
--                                         | otherwise = gd { bullets=newBullet : bullets }
--                                        where
--                                            newBullet = makeBullet player

rotation :: Player -> Float
rotation = angle

getLives :: Player -> Int
getLives = lives

-- Movement

rotate :: Float -> Time -> Player -> Player
rotate speed dt p = p{ angle=angle p + speed * dt }

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
updatePosition dt player@Player{ playerKinematics } = player{ playerKinematics=newKinematics }
    where
        newKinematics = clampVelocity maxVelocity $ updateKinematics dt playerKinematics
        maxVelocity = 300

-- Respawning 

handleDeath :: Player -> Player
handleDeath player  | hasSpawnProtection player = makePlayer{ lives=lives player - 1 }
                    | otherwise = player

degradeRespawnShield :: Time -> Player -> Player
degradeRespawnShield dt player@Player{ respawnShield=s }
    | s > 0 = player{ respawnShield = s - dt * degrationSpeed }
    | otherwise = player{ respawnShield = 0  }
    where
        degrationSpeed = 10

hasSpawnProtection :: Player -> Bool
hasSpawnProtection = (0 ==) . respawnShield

