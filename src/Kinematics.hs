module Kinematics (KinematicInfo, makeKinematics, updateKinematics, clampVelocity, decelerate, setVelocity, setPosition) where

import Types

data KinematicInfo = KinematicInfo
    { velocityInfo :: Vector 
    , positionInfo :: Vector 
    }

instance Body KinematicInfo where
    velocity = velocityInfo
    position = positionInfo
    update = updateKinematics
    size _ = 0

makeKinematics :: Vector -> KinematicInfo
makeKinematics pos = KinematicInfo {velocityInfo=(0, 0), positionInfo=pos}

setVelocity :: Vector -> KinematicInfo -> KinematicInfo
setVelocity (x, y) kin = kin{ velocityInfo=(x, y) }  

setPosition :: Vector -> KinematicInfo -> KinematicInfo
setPosition (x, y) kin = kin{ positionInfo=(x, y) }

updateKinematics :: Time -> KinematicInfo -> KinematicInfo
updateKinematics dt kin@(KinematicInfo { positionInfo=(x, y), velocityInfo=(dx, dy)}) = kin { positionInfo = updatedPosition } 
    where
        updatedPosition = (x + dt * dx, y + dt * dy)

clampVelocity :: Float -> KinematicInfo -> KinematicInfo
clampVelocity maxSpeed kin@KinematicInfo{ velocityInfo=(x, y) } = kin{ velocityInfo=clampedVelocity } 
    where
        speed = sqrt (x**2 + y**2)

        clampedVelocity | speed > maxSpeed = (maxSpeed * x/speed, maxSpeed * y/speed)
                        | otherwise = velocity kin

decelerate :: Float -> KinematicInfo -> KinematicInfo
decelerate deceleration kin@KinematicInfo { velocityInfo=(x, y) } = kin { velocityInfo=newVelocity }
    where
        acceleration = sqrt (x**2 + y**2)
        targetAcceleration = acceleration - deceleration
        newVelocity | acceleration == 0 = (x, y)
                    | otherwise = (targetAcceleration * x/acceleration, targetAcceleration * y/acceleration)
