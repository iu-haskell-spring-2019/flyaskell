{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where
import Functions
import Particle
import SDL
import Linear (V4(..))
import Control.Monad (unless)

run :: IO ()
run = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer initialState

appLoop :: Renderer -> Water -> IO ()
appLoop renderer water = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  render renderer water
  unless qPressed (appLoop renderer (advance water))





type Water = [Particle]

h :: Double
h = 5

calcDensity :: Water -> (Double -> Double -> Double) -> Coord -> Double
calcDensity [] func coord = 0
calcDensity (p:ps) func coord = (calcDensity ps func coord) +
  ((mass p) * (func (dist (position p) coord) h))


initialState :: Water
initialState = []

advance :: Water -> Water
advance water = water

render :: Renderer -> Water -> IO ()
render renderer particles = do
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
