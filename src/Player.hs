{-# LANGUAGE NamedFieldPuns #-}

module Player where

import Kinematics

data Player = Player
    { lives            :: Int
    , angle            :: Float
    , playerKinematics :: KinematicInfo
    }

makePlayer :: Player
makePlayer = Player {lives=3, angle=0, playerKinematics=makeKinematics (0, 0)}

acceleratePlayer :: Player -> Player
acceleratePlayer player@Player{ playerKinematics, angle } = player{ playerKinematics=calculateVelocity angle playerKinematics }

calculateVelocity :: Float -> KinematicInfo -> KinematicInfo
calculateVelocity angle kin@KinematicInfo{ position=(x, y) } = kin{ position=(x+1, y+1)} 
