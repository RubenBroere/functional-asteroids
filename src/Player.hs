{-# LANGUAGE NamedFieldPuns #-}

module Player (makePlayer, updatePlayer) where

import Kinematics
import InputHandler
import Data
import Graphics.Gloss.Interface.IO.Game (Key (SpecialKey, Char), SpecialKey (..))

makePlayer :: Player
makePlayer = Player {lives=3, angle=0, playerKinematics=makeKinematics (0, 0)}

updatePlayer :: Time -> GameData -> GameData
updatePlayer dt gd = updatePosition dt $ updateInput dt gd


updateInput :: Time -> GameData -> GameData
updateInput dt gd = foldr applyIfPressed gd inputActions
    where
        applyIfPressed :: ([Key], Time -> GameData -> GameData) -> GameData -> GameData
        applyIfPressed (keys, action) accData
            | any (`isKeyPressed` gd) keys = action dt accData
            | otherwise = accData

inputActions :: [([Key], Time -> GameData -> GameData)]
inputActions = [([Char 'w', SpecialKey KeyUp], forward), ([Char 'd', SpecialKey KeyRight], rotate pi), ([Char 'a', SpecialKey KeyLeft], rotate (-pi))]

forward :: Time -> GameData -> GameData
forward dt gd = gd { player = changeVelocity dt (player gd) }

rotate :: Float -> Time -> GameData -> GameData
rotate speed dt gd@GameData{ player=p@Player{ angle } } = gd{ player=p{ angle=angle + speed * dt } } 

updatePosition :: Time -> GameData -> GameData
updatePosition dt gd@GameData{ player } = gd{ player=player{ playerKinematics=newKinematics } }
    where
        newKinematics = deceleratePlayer . clampVelocity maxVelocity $ updateKinematics dt (playerKinematics player)
        deceleratePlayer    | any (`isKeyPressed` gd) [Char 'w', SpecialKey KeyUp] = id
                            | otherwise = decelerate (dt * deceleration)
        maxVelocity = 200
        deceleration = 100

changeVelocity :: Time -> Player -> Player
changeVelocity dt player@Player{ playerKinematics, angle } =
    player{ playerKinematics=playerKinematics{ velocity = calcVelocity dt angle (velocity playerKinematics) } }

calcVelocity :: Time -> Float -> (Float, Float) -> (Float, Float)
calcVelocity dt angle (x, y) = (x + speed * dt * sin angle, y + speed * dt * cos angle)
    where
        speed = 500
