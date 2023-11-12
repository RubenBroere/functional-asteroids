module Ufo (Ufo, AimTarget(..), generateUfo, moveToTargets, ufoShoot, ufoIsReloading, isSmart) where

import Kinematics
import Types
import Renderer
import Spawning

import Graphics.Gloss hiding(Vector)
import System.Random (StdGen)
import Sprites

data Ufo = Ufo
    { aimTarget     :: AimTarget
    , kinematics :: KinematicInfo
    , reloading :: Time
    , targetLocation :: Vector
    }

data AimTarget = Random | TargetPlayer
    deriving (Eq)

instance Body Ufo where
    position = position . kinematics
    velocity = velocity . kinematics
    update dt ufo = ufo{ kinematics=update dt $ kinematics ufo, reloading=max 0 $ reloading ufo - dt }
    size _ = 20

instance Drawable Ufo where
    draw ufo@Ufo{ aimTarget=TargetPlayer } = translateBody ufo $ Color blue ufoSprite
    draw ufo = translateBody ufo $ Color yellow ufoSprite

generateUfo :: (Int, Int) -> StdGen -> AimTarget -> (Ufo, StdGen)
generateUfo screenSize rnd target = (Ufo{ aimTarget=target, kinematics=kin, reloading=0, targetLocation=location }, rnd2)
    where
        (kin, rnd1) = genKinAtBorder screenSize rnd
        (location, rnd2) = randomPosition screenSize rnd1

moveToTarget :: (Int, Int) -> StdGen -> Ufo -> (Ufo, StdGen)
moveToTarget screenSize rnd ufo
    | d < 10 = (ufo{ targetLocation=newTarget }, rnd')
    | otherwise = (ufo{ kinematics=setVelocity v $ kinematics ufo }, rnd)
    where
        (x, y) = position ufo
        (tx, ty) = targetLocation ufo
        (dx, dy) = (tx-x, ty-y)
        d = sqrt (dx**2 + dy**2)
        (ux, uy) = (dx / d, dy / d)
        v = (ux * speed, uy * speed)
        (newTarget, rnd') = randomPosition screenSize rnd
        speed = 200

moveToTargets :: (Int, Int) -> StdGen -> [Ufo] -> ([Ufo], StdGen)
moveToTargets worldSize rnd = foldr (\ufo (ufos', rnd') ->
    let (ufo', rnd'') = moveToTarget worldSize rnd' ufo in (ufo':ufos', rnd'')) ([], rnd)

ufoShoot :: Ufo -> Ufo
ufoShoot u = u{ reloading=1 }

ufoIsReloading :: Ufo -> Bool
ufoIsReloading u = reloading u > 0

isSmart :: Ufo -> Bool
isSmart = (TargetPlayer==) . aimTarget
