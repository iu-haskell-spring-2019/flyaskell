module Lib (run) where

import Functions (advance)
import Particle
import Graphics.Gloss
import Linear

window :: Display
window = InWindow "Water" (1600, 900) (10, 10)

background :: Color
background = black

drawParticle :: Particle -> Picture
drawParticle part = Graphics.Gloss.color (Particle.color part) (translate x y (thickCircle 5 10))
  where
    i :: Double
    j :: Double
    V2 i j = position part
    x :: Float
    y :: Float
    (x, y) = (realToFrac i,  realToFrac j)

drawAll :: Water -> Picture
drawAll water = foldl g blank water
 where
   g :: Picture -> Particle -> Picture
   g pic part = pic <> drawParticle part

run :: IO ()
run = simulate window background 30 initialState drawAll adv
  where
    adv _ _ = advance
