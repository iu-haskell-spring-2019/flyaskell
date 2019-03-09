module Lib (run) where

import Functions (advance)
import Particle
import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (1600, 900) (10, 10)

background :: Color
background = white

drawing :: Float -> Picture
drawing a = circle (a * 100)

run :: IO ()
run = animate window background drawing
