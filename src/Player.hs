{-# LANGUAGE NamedFieldPuns #-}

module Player (makePlayer, updatePlayer) where

import Kinematics
import InputHandler
import Data hiding(updatePosition)
import Bullet

makePlayer :: Player
makePlayer = Player {lives=3, angle=0, playerKinematics=makeKinematics (0, 0)}

updatePlayer :: Time -> GameData -> GameData
updatePlayer dt gd = handleDeath . updatePosition dt $ updateInput dt gd


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

shoot :: Time -> GameData -> GameData
shoot _ gd@GameData{ player, bullets }  | any (\x -> lifeTime x > 25) bullets = gd
                                        | otherwise = gd { bullets=newBullet : bullets }
                                        where
                                            newBullet = makeBullet player

forward :: Time -> GameData -> GameData
forward dt gd = gd { player = changeVelocity dt (player gd) }

rotate :: Float -> Time -> GameData -> GameData
rotate speed dt gd@GameData{ player=p@Player{ angle } } = gd{ player=p{ angle=angle + speed * dt } }


handleDeath :: GameData -> GameData
handleDeath gd  | isColliding gd = gd { player=Player{ playerKinematics=makeKinematics (0, 0), angle=0, lives=lives (player gd) - 1 } }
                | otherwise = gd

isColliding :: GameData -> Bool
isColliding = isOutOfBounds


isOutOfBounds :: GameData -> Bool
isOutOfBounds gd = rx < -hWidth || rx >= hWidth || ry < -hHeight || ry >= hHeight
    where
        (x, y) = position . playerKinematics $ player gd
        (rx, ry) = (round x, round y)
        (width, height) = worldSize gd
        (hWidth, hHeight) = (width `div` 2, height `div` 2)

updatePosition :: Time -> GameData -> GameData
updatePosition dt gd@GameData{ player } = gd{ player=player{ playerKinematics=newKinematics } }
    where
        newKinematics = deceleratePlayer . clampVelocity maxVelocity $ updateKinematics dt (playerKinematics player)
        deceleratePlayer    | pressForward gd = id
                            | otherwise = decelerate (dt * deceleration)
        maxVelocity = 300
        deceleration = 200

changeVelocity :: Time -> Player -> Player
changeVelocity dt player@Player{ playerKinematics, angle } =
    player{ playerKinematics=playerKinematics{ velocity = calcVelocity dt angle (velocity playerKinematics) } }

calcVelocity :: Time -> Float -> (Float, Float) -> (Float, Float)
calcVelocity dt angle (x, y) = (x + speed * dt * sin angle, y + speed * dt * cos angle)
    where
        speed = 500
