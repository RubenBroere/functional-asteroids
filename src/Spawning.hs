module Spawning (genKinAtBorder, randomPosition) where

import System.Random (StdGen, Random (randomR))

import Types
import Kinematics

genKinAtBorder :: (Int, Int) -> StdGen -> (KinematicInfo, StdGen)
genKinAtBorder (width, height) gen0 = (kinematic, gen4)
    where
        kinematic = setVelocity (speed * dx, speed *dy) $ makeKinematics (x, y)
        (theta, gen1) = randomR (0, 2*pi) gen0
        dx = 10 * sin theta
        dy = 10 * cos theta
        ((px, py), gen2) = randomPosition (width, height) gen1
        (x, y) = traverseTillBorder (width, height) (px, py) (-dx, -dy)
        (speed, gen4) = randomR (10.0, 15.0) gen2

-- Walk line till we've reached a border
traverseTillBorder :: (Int, Int) -> Vector -> Vector -> Vector
traverseTillBorder s p@(x, y) d@(dx, dy)
    | not $ isBounds s newPos = p 
    | otherwise = traverseTillBorder s newPos d
    where
        newPos = (x + dx, y + dy) 

isBounds :: (Int, Int) -> Vector -> Bool
isBounds (width, height) (x, y) = x > (-hWidth - pad) && x < (hWidth + pad) && y > (-hHeight - pad) && y < (hHeight + pad)
    where
        pad = 50
        (hWidth, hHeight) = (fromIntegral $ width `div` 2, fromIntegral $ height `div` 2)

randomPosition :: (Int, Int) -> StdGen -> (Vector, StdGen)
randomPosition (width, height) gen1 = ((px, py), gen3)
    where
        (px, gen2) = randomR (-hWidth, hWidth) gen1
        (py, gen3) = randomR (-hHeight, hHeight) gen2
        (hWidth, hHeight) = (fromIntegral $ width `div` 2, fromIntegral $ height `div` 2)
