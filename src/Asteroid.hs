module Asteroid (Asteroid, AsteroidSize, generateAsteroid, isGarbage, split, canSpawnMore) where

import System.Random hiding (split)

import Kinematics
import Types
import Renderer
import Spawning

import Graphics.Gloss hiding (Vector)

data Asteroid = Asteroid
    { asteroidSize       :: AsteroidSize
    , kinematics :: KinematicInfo
    }

instance Body Asteroid where
    position = position . kinematics
    velocity = velocity . kinematics
    size = getAsteroidSize
    update dt a = a{ kinematics=updateKinematics dt (kinematics a) } 


instance Drawable Asteroid where
    draw asteroid = translateBody asteroid $ Color white $ Circle $ getAsteroidSize asteroid

data AsteroidSize = Small | Medium | Large
    deriving (Eq, Ord, Enum)

getAsteroidSize :: Asteroid -> Float
getAsteroidSize Asteroid { asteroidSize=Small } = 15
getAsteroidSize Asteroid { asteroidSize=Medium } = 30
getAsteroidSize Asteroid { asteroidSize=Large } = 50

generateAsteroid :: (Int, Int) -> StdGen -> (Asteroid, StdGen)
generateAsteroid worldSize gen0 = (asteroid, gen1)
    where
        asteroid = Asteroid { asteroidSize=Large, kinematics=kin }
        (kin, gen1) = genKinAtBorder worldSize gen0 

isGarbage :: (Int, Int) -> Asteroid -> Bool
isGarbage screen a@Asteroid{ asteroidSize=Large } = not $ isInBounds screen a
isGarbage _ _ = False 

canSpawnMore :: [Asteroid] -> Bool
canSpawnMore = (< maxLargeSpawns) . length . filter ((== Large) . asteroidSize)
    where
        maxLargeSpawns = 2

isInBounds :: (Int, Int) -> Asteroid -> Bool
isInBounds screen a = isBounds screen (position a) 

isBounds :: (Int, Int) -> Vector -> Bool
isBounds (width, height) (x, y) = x > (-hWidth - pad) && x < (hWidth + pad) && y > (-hHeight - pad) && y < (hHeight + pad)
    where
        pad = 50
        (hWidth, hHeight) = (fromIntegral $ width `div` 2, fromIntegral $ height `div` 2)

split :: Asteroid -> StdGen -> Maybe (Asteroid, Asteroid, StdGen)
split Asteroid{ asteroidSize=Small } _ = Nothing  
split a0 gen0 = Just (a1, a2, gen2) 
    where
        (a1, gen1) = splitOf a0 gen0
        (a2, gen2) = splitOf a0 gen1 

splitOf :: Asteroid -> StdGen -> (Asteroid, StdGen)
splitOf a0 gen0 = (a1, gen1) 
    where
        a1 = shrinkAsteroid $ a0{ kinematics=kin }
        kin = setVelocity (dx, dy) (kinematics a0)
        (theta, gen1) = randomR (-2.0, 2.0) gen0
        (x, y) = velocity a0
        (dx, dy) = (x * cos theta - y * sin theta, x * sin theta + y * cos theta)

shrinkAsteroid :: Asteroid -> Asteroid
shrinkAsteroid a@Asteroid{ asteroidSize=Large } = a{ asteroidSize=Medium }
shrinkAsteroid a@Asteroid{ asteroidSize=Medium } = a{ asteroidSize=Small }
shrinkAsteroid a = a 

