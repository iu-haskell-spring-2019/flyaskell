module Particle where

import Graphics.Gloss

type Coord = (Double, Double)
data Particle = Particle { color :: Color
                         , position :: Coord
                         , velocity :: Coord
                         , mass :: Double
}

type Water = [Particle]

initialState :: Water
initialState = [Particle {Particle.color=blue, position=(1.1, 2.6), velocity=(5.5, 6.2), mass=5.0}]
