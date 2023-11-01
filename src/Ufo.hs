module Ufo (Ufo) where

import Kinematics

data Ufo = Ufo
    { aimTarget     :: AimTarget
    , ufoKinematics :: KinematicInfo
    }

data AimTarget = TargetForward | TargetPlayer
    deriving (Eq)

