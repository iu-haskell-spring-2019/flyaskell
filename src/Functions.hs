module Functions where
import Particle
calcPressure :: Double -> Double -> Double
calcPressure p p0 = k * (p - p0)
  where
    k = 0.3
    p0 = 0

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

dist :: Coord -> Coord -> Double
dist (x1,y1) (x2,y2) = ((x1 - x2)**2 + (y1 - y2)**2)**0.5
