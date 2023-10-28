module Kinematics (makeKinematics, updateKinematics, clampVelocity, decelerate) where

import Data

makeKinematics :: (Float, Float) -> KinematicInfo
makeKinematics pos = KinematicInfo {velocity=(0, 0), position=pos}

updateKinematics :: Time -> KinematicInfo -> KinematicInfo
updateKinematics dt kin@(KinematicInfo { position=(x, y), velocity=(dx, dy)}) = kin { position = updatedPosition } 
    where
        updatedPosition = (x + dt * dx, y + dt * dy) 

clampVelocity :: Float -> KinematicInfo -> KinematicInfo
clampVelocity maxSpeed kin@KinematicInfo{ velocity=(x, y) } = kin{ velocity=clampedVelocity } 
    where
        speed = sqrt (x**2 + y**2)

        clampedVelocity | speed > maxSpeed = (maxSpeed * x/speed, maxSpeed * y/speed)
                        | otherwise = velocity kin

decelerate :: Float -> KinematicInfo -> KinematicInfo
decelerate deceleration kin@KinematicInfo { velocity=(x, y) } = kin { velocity=newVelocity }
    where
        acceleration = sqrt (x**2 + y**2)
        targetAcceleration = acceleration - deceleration
        newVelocity | acceleration == 0 = (x, y)
                    | otherwise = (targetAcceleration * x/acceleration, targetAcceleration * y/acceleration)
