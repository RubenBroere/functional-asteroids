{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data where

import Kinematics

data GameState = Game | Paused | GameOver
    deriving (Eq)


data Ufo = Ufo
    { aimTarget     :: AimTarget
    , ufoKinematics :: KinematicInfo
    }

data AimTarget = TargetForward | TargetPlayer
    deriving (Eq)

