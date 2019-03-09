module Particle where
data Color = Green | Red | Blue
type Coord = (Double, Double)
data Particle = Particle { color :: Color
                         , position :: Coord
                         , velocity :: Coord
                         , mass :: Double
}
