module Particle where

import Graphics.Gloss
import Linear (V2(..))

type Coord = V2 Double
data Particle = Particle { color :: Color
                         , position :: Coord
                         , velocity :: Coord
                         , mass :: Double
}

type Water = [Particle]

initialState :: Water
initialState = [
    Particle {Particle.color=blue, position=(V2 a b), velocity=(V2 (-3) 4), mass=5.0} 
    | a <- [0, 50..500]
    , b <- [0, 50..500]
    ]

