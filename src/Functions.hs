module Functions (advance) where

import Particle
import Linear

calcPressure :: Double -> Double
calcPressure p = k * (p - p0)
  where
    k = 0.3
    p0 = 0

calcPressurePoint :: Double -> Double
calcPressurePoint = calcPressure

-- p
unitVector :: Particle -> Particle -> Coord
unitVector start end
  | position start == position end = V2 0 0
  | otherwise = ((position end) - (position start)) ^/ len
    where
      len :: Double
      len = distance (position start) (position end)

-- force from part2 on part1
calcPressureForceBetweenPoints :: (Double, Particle) -> (Double, Particle) -> Coord
calcPressureForceBetweenPoints (dens1, part1) (dens2, part2) = (unitVector part2 part1) ^* (mass part2) ^*
  (pressure1 + pressure2) ^/ (2 * dens2) ^* (wPoly len h)
  where
    len = distance (position part1) (position part2)
    pressure1 = calcPressurePoint dens1
    pressure2 = calcPressurePoint dens2

calcPressureForcePoint :: [(Double, Particle)] -> (Double, Particle) -> Coord
calcPressureForcePoint densAndWater densPart = sum (map g densAndWater)
  where
    g :: (Double, Particle) -> Coord
    g helpDensPart = calcPressureForceBetweenPoints densPart helpDensPart

h :: Double
h = 2



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
advance water = map g densAndWater
  where
    densAndWater :: [(Double, Particle)]
    densAndWater = zip densities water
    densities :: [Double]
    densities = getDensity water
    force :: (Double, Particle) -> Coord
    force = calcPressureForcePoint densAndWater
    g :: (Double, Particle) -> Particle
    g (dens, part) = part { position=boundCoord (position part + velocity part)
      , velocity=velocity part + (V2 0 (-9.8 / 15)) + force (dens, part) ^/ mass part}


calcDensity :: Water -> (Double -> Double -> Double) -> Coord -> Double
calcDensity [] func coord = 0
calcDensity (p:ps) func coord = (calcDensity ps func coord) + ((mass p) * (func (distance (position p) coord) h))

calcDensityPoint :: Particle -> Water -> Double
calcDensityPoint particle water = calcDensity water wPoly (position particle)

getDensity :: Water -> [Double]
getDensity water = map g water
  where
    g :: Particle -> Double
    g part = calcDensityPoint part water
