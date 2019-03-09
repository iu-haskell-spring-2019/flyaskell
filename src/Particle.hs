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
initialState = [Particle {Particle.color=blue, position=(V2 5.5 6.2), velocity=(V2 5.5 6.2), mass=5.0}]
