module Types (Time, Score, Vector, Point, Body(..), Drawable(..), GameState(..), isCollision, isCollisions) where
import Graphics.Gloss (Picture (Pictures))

type Time = Float
type Score = Int
type Vector = (Float, Float)
type Point = (Int, Int)

data GameState = Game | Paused | GameOver
    deriving (Eq, Show)

class Body a where
    position :: a -> Vector
    velocity :: a -> Vector
    size :: a -> Float
    update :: Time -> a -> a

    updateAll :: Time -> [a] -> [a]
    updateAll dt = map (update dt)

-- Simple collision detection
isCollision :: (Body a, Body b) => a -> b -> Bool
isCollision a b = d < s
    where
        (x0, y0) = position a
        (x1, y1) = position b
        d = sqrt $ (x0 - x1)**2 + (y0 - y1)**2
        s = size a + size b

isCollisions :: (Body a, Body b) => a -> [b] -> Bool
isCollisions a = any (isCollision a)

class Drawable a where
    draw :: a -> Picture

    drawAll :: [a] -> Picture
    drawAll xs = Pictures $ map draw xs

