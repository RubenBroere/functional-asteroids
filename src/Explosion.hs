module Explosion (Explosion, makeExplosion) where

import Types
import Renderer
import Graphics.Gloss hiding(Vector)

data Explosion = Explosion
    { time :: Time
    , position :: Vector
    , eSize :: Float
    }

instance Body Explosion where
    position = Explosion.position
    velocity _ = (0, 0)
    update dt e = e{ time=time e - dt }
    updateAll dt = filter (\x -> 0 < time x) . map (update dt)
    size = eSize

instance Drawable Explosion where
    draw e = translateBody e $ Color white $ Circle ((duration - time e) * 500 + eSize e)

makeExplosion :: Float -> Vector -> Explosion
makeExplosion expSize pos = Explosion {time=duration, Explosion.position=pos, eSize=expSize}

duration :: Float
duration = 0.1
