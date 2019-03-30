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
    Particle {Particle.color=white, position=(V2 a b), velocity=(V2 0 0), mass=5.0} 
    | a <- [0, 1..4]
    , b <- [0, 1..4]
    ]
-- initialState = [Particle {Particle.color = white, position=(V2 0 0), velocity=(V2 0 0), mass=5.0},
--   Particle {Particle.color = white, position=(V2 0 4), velocity=(V2 0 0), mass=5.0}]
