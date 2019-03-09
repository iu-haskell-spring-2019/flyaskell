module Functions (advance) where

import Particle
import Linear

calcPressure :: Double -> Double
calcPressure p = k * (p - p0)
  where
    k = 0.3
    p0 = 0

calcPressurePoint :: Particle -> Water -> Double
calcPressurePoint part water = calcPressure (calcDensityPoint part water)

-- p
unitVector :: Particle -> Particle -> Coord
unitVector start end
  | position start == position end = V2 0 0
  | otherwise = ((position end) - (position start)) ^/ len
    where
      len :: Double
      len = distance (position start) (position end)
-- force from part2 on part1
calcPressureForceBetweenPoints :: Particle -> Particle -> Water -> Coord
calcPressureForceBetweenPoints part1 part2 water = (unitVector part2 part1) ^*
  (pressure1+pressure2) ^/ (2 * density1) ^* (wPoly len h)
  where
    len = distance (position part1) (position part2)
    pressure1 = calcPressurePoint part1 water
    pressure2 = calcPressurePoint part2 water
    density1 = calcDensityPoint part1 water

calcPressureForcePoint :: Particle -> Water -> Coord
calcPressureForcePoint part water = sum (map g water)
  where
    g :: Particle -> Coord
    g help_part = calcPressureForceBetweenPoints part help_part water

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

lowerBoundCoord :: Coord -> Coord
lowerBoundCoord (V2 a b) = V2 (max a (-800)) (max b (-450))

upperBoundCoord :: Coord -> Coord
upperBoundCoord (V2 a b) = V2 (min a 800) (min b 450)

boundCoord :: Coord -> Coord
boundCoord = lowerBoundCoord . upperBoundCoord

advance :: Water -> Water
advance water = map g water
  where
    g :: Particle -> Particle
    g part = part { position=boundCoord (position part + velocity part)
                  , velocity=velocity part + (V2 0 (-9.8 / 15))
                  }

calcDensityPoint :: Particle -> Water -> Double
calcDensityPoint particle water = calcDensity water wPoly (position particle)

calcDensity :: Water -> (Double -> Double -> Double) -> Coord -> Double
calcDensity [] func coord = 0
calcDensity (p:ps) func coord = (calcDensity ps func coord) +
  ((mass p) * (func (distance (position p) coord) h))
