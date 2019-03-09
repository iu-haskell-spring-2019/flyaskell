{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where
import Functions (advance)
import Particle
import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

run :: IO ()
run = display window background drawing
