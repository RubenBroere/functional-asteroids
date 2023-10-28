{-# language NamedFieldPuns #-}

module Renderer (drawGame) where

import Data
import Graphics.Gloss

translateKin :: KinematicInfo -> Picture -> Picture
translateKin KinematicInfo{ position } = uncurry Translate position

rotateRadian :: Float -> Picture -> Picture
rotateRadian theta = Rotate (theta * (180/pi) - 45)

drawGame :: GameData -> Picture
drawGame GameData { player } = drawPlayer player

drawPlayer :: Player -> Picture
drawPlayer Player{ playerKinematics=kin, angle } = translateKin kin $ rotateRadian angle $ Color white $ Pictures [Circle 40, Line [(0, 0), (20, 20)]]
