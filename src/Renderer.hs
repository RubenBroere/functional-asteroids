{-# language NamedFieldPuns #-}

module Renderer (drawGame) where

import GameData
import Player
import Graphics.Gloss
import Kinematics

translateKin :: KinematicInfo -> Picture -> Picture
translateKin KinematicInfo{ position } = uncurry Translate position

drawGame :: GameData -> Picture
drawGame GameData { player } = drawPlayer player

drawPlayer :: Player -> Picture
drawPlayer Player{ playerKinematics=kin } = translateKin kin (Color white (Circle 80))
