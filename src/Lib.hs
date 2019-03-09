module Lib (run) where

import Functions (advance)
import Particle
import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (1600, 900) (10, 10)

background :: Color
background = white

drawParticle :: Particle -> Picture
drawParticle part = translate x y (circle 2)
  where
    i :: Double
    j :: Double
    (i, j) = position part
    x :: Float
    y :: Float
    (x, y) = (realToFrac i, realToFrac j)

drawAll :: Water -> Picture
drawAll list = foldl g blank list
  where
    g :: Picture -> Particle -> Picture
    g pic part = pic <> drawParticle part

drawing :: Float -> Picture
drawing t = drawAll initialState

run :: IO ()
run = animate window background drawing
