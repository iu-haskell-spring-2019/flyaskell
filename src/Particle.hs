module Particle where

import Graphics.Gloss
import Linear (V2(..))

type Coord = V2 Double
data Particle = Particle { color :: Color
                         , position :: Coord
                         , velocity :: Coord
                         , mass :: Double
} deriving Show

type Water = [Particle]

initialState :: Water
initialState = [
    Particle {Particle.color=blue, position=(V2 a b), velocity=(V2 0 0), mass=5.0} 
    | a <- [0, 50..500]
    , b <- [0, 50..500]
    ]

