module Lib (run) where

import Functions (advance)
import Particle
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate (ViewPort)
import Linear (V2(..))

window :: Display
window = InWindow "Nice Window" (1600, 900) (10, 10)

background :: Color
background = black

drawParticle :: Particle -> Picture
drawParticle part = Graphics.Gloss.color white (translate x y (circle 2))
  where
    i :: Double
    j :: Double
    V2 i j = position part
    x :: Float
    y :: Float
    (x, y) = (realToFrac i, realToFrac j)

drawAll :: Water -> Picture
drawAll list = foldl g blank list
  where
    g :: Picture -> Particle -> Picture
    g pic part = pic <> drawParticle part

adv :: ViewPort -> Float -> Water -> Water
adv _ _ = advance

run :: IO ()
run = simulate window background 10 initialState drawAll adv
