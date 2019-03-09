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
initialState = []
