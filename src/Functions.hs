module Functions (advance) where

import Particle
import Linear (V2(..), distance)

calcPressure :: Double -> Double -> Double
calcPressure p p0 = k * (p - p0)
  where
    k = 0.3
    p0 = 0

h :: Double
h = 5

wPoly :: Double -> Double -> Double
wPoly r h
  | r <= h = 315 / 64 / pi / h**9 * (h**2 - r**2)**3
  | otherwise = 0

gradWPoly :: Double -> Double -> Double
gradWPoly r h
  | r <= h = -315 / 64 / pi / h**9 * 6 * r * (h**2 - r**2) ** 2
  | otherwise = 0

hessWPoly :: Double -> Double -> Double
hessWPoly r h
  | r <= h = 315 / 64 / pi / h**9 * 6 * (h**2 - r**2) * (4 * r**2 - (h**2 - r**2))
  | otherwise = 0

advance :: Water -> Water
advance water = map g water
  where
    g :: Particle -> Particle
    g part = part { position=position part + velocity part
                  , velocity=velocity part + (V2 0 (-9.8))
                  }

calcDensity :: Water -> (Double -> Double -> Double) -> Coord -> Double
calcDensity [] func coord = 0
calcDensity (p:ps) func coord = (calcDensity ps func coord) +
  ((mass p) * (func (distance (position p) coord) h))
