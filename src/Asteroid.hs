module Asteroid (Asteroid, AsteroidSize, generateAsteroid, updateAsteroid, getAsteroidSize) where

import System.Random

import Kinematics
import Types

data Asteroid = Asteroid
    { size               :: AsteroidSize
    , asteroidKinematics :: KinematicInfo
    }

instance Body Asteroid where
    position = velocity . asteroidKinematics  
    velocity = velocity . asteroidKinematics

instance Drawable Asteroid where
    draw = undefined  

data AsteroidSize = Small | Medium | Large
    deriving (Eq, Ord, Enum)

getAsteroidSize :: Asteroid -> Float
getAsteroidSize Asteroid { size=Small } = 10 
getAsteroidSize Asteroid { size=Medium } = 20
getAsteroidSize Asteroid { size=Large } = 30 

generateAsteroid :: (Int, Int) -> StdGen -> (Asteroid, StdGen)
generateAsteroid (width, height) gen0 = (asteroid, gen5)
    where
        asteroid = Asteroid { size=toEnum s, asteroidKinematics=setVelocity (dx, dy) $ makeKinematics (x, y) }
        (x, gen1) = randomR (-hWidth, hWidth) gen0
        (y, gen2) = randomR (-hHeight, hHeight) gen1
        (dx, gen3) = randomR (0.0, 100.0) gen2
        (dy, gen4) = randomR (0.0, 100.0) gen3
        (s, gen5) = randomR (0, 2) gen4
        (hWidth, hHeight) = (fromIntegral $ width `div` 2, fromIntegral $ height `div` 2)

updateAsteroid :: Time -> Asteroid -> Asteroid
updateAsteroid dt asteroid@Asteroid{ asteroidKinematics=kin } = asteroid { asteroidKinematics=updateKinematics dt kin } 
