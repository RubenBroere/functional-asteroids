module Types (Time, Score, Vector, Point, Body(..), Drawable(..)) where
import Graphics.Gloss (Picture)

type Time = Float
type Score = Int
type Vector = (Float, Float)
type Point = (Int, Int)

class Body a where
    position :: a -> Vector
    velocity :: a -> Vector

class Drawable a where
    draw :: a -> Picture

