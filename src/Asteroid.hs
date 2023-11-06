module Asteroid (Asteroid, AsteroidSize, generateAsteroid, isGarbage, split, canSpawnMore) where

import System.Random hiding (split)

import Kinematics
import Types
import Renderer
import Graphics.Gloss hiding (Vector)

data Asteroid = Asteroid
    { asteroidSize       :: AsteroidSize
    , kinematics :: KinematicInfo
    }

instance Body Asteroid where
    position = position . kinematics
    velocity = velocity . kinematics
    size = getAsteroidSize
    update = updateAsteroid

instance Drawable Asteroid where
    draw asteroid = translateBody asteroid $ Color white $ Circle $ getAsteroidSize asteroid

data AsteroidSize = Small | Medium | Large
    deriving (Eq, Ord, Enum)

getAsteroidSize :: Asteroid -> Float
getAsteroidSize Asteroid { asteroidSize=Small } = 15
getAsteroidSize Asteroid { asteroidSize=Medium } = 30
getAsteroidSize Asteroid { asteroidSize=Large } = 50

generateAsteroid :: (Int, Int) -> StdGen -> (Asteroid, StdGen)
generateAsteroid (width, height) gen0 = (asteroid, gen4)
    where
        asteroid = Asteroid { asteroidSize=Large, kinematics=setVelocity (speed * dx, speed *dy) $ makeKinematics (x, y) }
        (theta, gen1) = randomR (0, 2*pi) gen0
        dx = 10 * sin theta
        dy = 10 * cos theta
        (px, gen2) = randomR (-hWidth, hWidth) gen1
        (py, gen3) = randomR (-hHeight, hHeight) gen2
        (x, y) = traverseTillBorder (width, height) (px, py) (-dx, -dy)
        (speed, gen4) = randomR (10.0, 15.0) gen3
        (hWidth, hHeight) = (fromIntegral $ width `div` 2, fromIntegral $ height `div` 2)

-- Walk line till we've reached a border
traverseTillBorder :: (Int, Int) -> Vector -> Vector -> Vector
traverseTillBorder s p@(x, y) d@(dx, dy)
    | not $ isBounds s newPos = p 
    | otherwise = traverseTillBorder s newPos d
    where
        newPos = (x + dx, y + dy) 

updateAsteroid :: Time -> Asteroid -> Asteroid
updateAsteroid dt asteroid@Asteroid{ kinematics=kin } = asteroid { kinematics=updateKinematics dt kin } 

isGarbage :: (Int, Int) -> Asteroid -> Bool
isGarbage screen a@Asteroid{ asteroidSize=Large } = not $ isInBounds screen a
isGarbage _ _ = False 

canSpawnMore :: [Asteroid] -> Bool
canSpawnMore = (<= maxLargeSpawns) . length . filter ((== Large) . asteroidSize)
    where
        maxLargeSpawns = 5

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

