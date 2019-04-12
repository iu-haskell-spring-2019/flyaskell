module Particle where

import           Graphics.Gloss hiding (color)
import           Linear         (V2 (..))

type Coord  = V2 Double

data Particle = Particle
  { color    :: Color
  , position :: Coord
  , velocity :: Coord -- FIXME: this is not a coordinate
  , mass     :: Double
  } deriving (Show)

type Water = [Particle]

mkParticle :: Coord -> Particle
mkParticle coord = Particle
  { color     = white
  , position  = coord
  , velocity  = V2 0 0
  , mass      = 1.0
  }

initialState :: Water
initialState = [ mkParticle (V2 x y) | x <- [0..10] , y <- [0..10] ]
