{-# language NamedFieldPuns #-}

module Kinematics where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point.Arithmetic
import Prelude hiding ((+), (*))

data KinematicInfo = KinematicInfo
    { velocity :: Vector
    , position :: Vector
    }

makeKinematics :: Point -> KinematicInfo
makeKinematics pos = KinematicInfo {velocity=(0,0), position=pos}

updateKinematics :: Float -> KinematicInfo -> KinematicInfo
updateKinematics dt kin@(KinematicInfo { position, velocity }) = kin { position = position + dt * velocity } 
