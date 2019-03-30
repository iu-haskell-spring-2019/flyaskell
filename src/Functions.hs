module Functions where

import Particle
import Linear


type DensPart = (Double, Particle)
-- p
unitVector :: Coord -> Coord -> Coord
unitVector start end
  | start == end = V2 0 0
  | otherwise = (end - start) ^/ (distance start end)

calcPressurePoint :: Double -> Double
calcPressurePoint p = k * (p - p0)
  where
    k = 0.3
    p0 = 0.1


calcAbstractForcePoint :: (DensPart -> DensPart -> Coord) -> [DensPart] -> DensPart -> Coord
calcAbstractForcePoint forceCalculator densWater particle = sum (map g densWater)
  where
    g :: DensPart -> Coord
    g = forceCalculator particle



calcPressureForcePoint :: [DensPart] -> DensPart -> Coord
calcPressureForcePoint = calcAbstractForcePoint calcPressureForceBetweenPoints 

calcViscosityForcePoint :: [DensPart] -> DensPart -> Coord
calcViscosityForcePoint = calcAbstractForcePoint calcViscosityForceBetweenPoints

calcTensionForcePoint :: [DensPart] -> DensPart -> Coord
calcTensionForcePoint = calcAbstractForcePoint calcTensionForceBetweenPoints


-- force from part2 on part1
calcPressureForceBetweenPoints :: DensPart -> DensPart -> Coord
calcPressureForceBetweenPoints (dens1, part1) (dens2, part2) =
  unit ^* ((mass part2) * (pressure1 + pressure2) / (2 * dens2) * (gradWPoly len))
  where
    unit = unitVector (position part2) (position part1)
    len = distance (position part1) (position part2)
    pressure1 = calcPressurePoint dens1
    pressure2 = calcPressurePoint dens2

calcViscosityForceBetweenPoints :: DensPart -> (Double,Particle) -> Coord
calcViscosityForceBetweenPoints (dens1,part1) (dens2,part2) = ((velocity part1) - (velocity part2)) ^* (mass part2)
  ^* (mu / dens2) ^* (hessWPoly ( distance (position part1) (position part2)))


calcTensionForceBetweenPoints :: DensPart -> DensPart -> Coord
calcTensionForceBetweenPoints (dens1, part1) (dens2, part2) = unit ^* ((mass part2) * sigma / dens2 *
  (hessWPoly (distance (position part1) (position part2))))
  where
    unit = rotateDevanosto (unitVector (position part1) (position part2))




sigma :: Double
sigma = 0

h :: Double
h = 5

mu :: Double 
mu = 1

wPoly :: Double -> Double
wPoly r
  | r <= h = 315 / 64 / pi / h**9 * (h**2 - r**2)**3
  | otherwise = 0

gradWPoly :: Double -> Double
gradWPoly r
  | r <= h = -315 / 64 / pi / h**9 * 6 * r * (h**2 - r**2) ** 2
  | otherwise = 0

hessWPoly :: Double -> Double
hessWPoly r
  | r <= h = 315 / 64 / pi / h**9 * 6 * (h**2 - r**2) * (4 * r**2 - (h**2 - r**2))
  | otherwise = 0

applyHorizontalBound :: Particle -> Particle
applyHorizontalBound particle
  | py < -450 = particle { position = (V2 px (-450)), velocity = (V2 vx 0) }
  | py > 450 =  particle { position = (V2 px 450), velocity = (V2 vx 0) }
  | otherwise = particle
  where
    V2 px py = position particle
    V2 vx vy = velocity particle

applyVerticalBound :: Particle -> Particle
applyVerticalBound particle
  | px < -800 = particle { position = (V2 (-800) py), velocity = (V2 0 vy) }
  | px > 800 =  particle { position = (V2 800 py), velocity = (V2 0 vy) }
  | otherwise = particle
  where
    V2 px py = position particle
    V2 vx vy = velocity particle

applyBound = applyHorizontalBound . applyVerticalBound

rotateDevanosto :: Coord -> Coord
rotateDevanosto (V2 a b) = V2 b (-a)

advance :: Water -> Water
advance water = map g densAndWater
  where
    densAndWater :: [DensPart]
    densAndWater = zip densities water
    densities :: [Double]
    densities = getDensity water
    pressureForce :: DensPart -> Coord
    pressureForce = calcPressureForcePoint densAndWater
    viscosityForce :: DensPart -> Coord
    viscosityForce = calcViscosityForcePoint densAndWater
    tensionForce :: (Double,Particle) -> Coord
    tensionForce _ = V2 0 0
    g :: DensPart -> Particle
    g (dens, part) = applyBound part { position=position part + velocity part
      , velocity=velocity part + (V2 0 (-9.8 / 15)) + ((pressureForce (dens, part) + 
          viscosityForce (dens, part) + tensionForce (dens,part)) ^/ (mass part))}


calcDensity :: Water -> (Double -> Double) -> Coord -> Double
calcDensity [] func coord = 0
calcDensity (p:ps) func coord = (calcDensity ps func coord) + ((mass p) * (func (distance (position p) coord)))

calcDensityPoint :: Particle -> Water -> Double
calcDensityPoint particle water = calcDensity water wPoly (position particle)

getDensity :: Water -> [Double]
getDensity water = map g water
  where
    g :: Particle -> Double
    g part = calcDensityPoint part water
