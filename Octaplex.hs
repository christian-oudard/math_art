import Geometry
  ( Vec
  , vecToPoint
  , rotation4d
  , octaplex
  , octaplexEdges
  )
import ColorSpace
import Graphics.Gloss
import Data.Prizm.Color (LAB(..), mkLAB)

import System.Random.MWC (GenIO, createSystemRandom, uniformR)
import Data.Matrix ((!))
import Data.List (sortBy)
import Data.Function (on)

bg1, bg2, lineColor :: Color
bg1 = toGloss $ mkLAB 5 (-5) (-15)
bg2 = toGloss $ mkLAB 10 (-5) (-20)
lineColor = toGloss $ mkLAB 70 5 7
dotColor = toGloss $ mkLAB 80 5 7

main = do
  g <- createSystemRandom
  t0 <- uniformR (0.0, 10**10) g :: IO Double
  animate FullScreen bg1 (frame t0)

frame t0 t = frame' t0 (realToFrac t)
frame' :: Double -> Double -> Picture
frame' t0 t = Scale 300 300 $ Pictures
  [ Color lineColor $ drawLines lines
  , Color dotColor $ drawPoints rotatedPoints
  ]
  where
    points = octaplex
    rotatedPoints = map (rotation *) points
    lines = [ (rotatedPoints !! i, rotatedPoints !! j) | (i, j) <- octaplexEdges ]
    rotation = rotation4d (t0 + 0.3 * t)

drawLines :: [(Vec, Vec)] -> Picture
drawLines lines = Pictures [ Line [vecToPoint a, vecToPoint b] | (a, b) <- lines ]

drawPoints :: [Vec] -> Picture
drawPoints points = Pictures $ map drawPoint points

drawPoint :: Vec -> Picture
drawPoint p = Translate x y $ circleSolid 0.005
  where (x, y) = vecToPoint p

pointColor4d :: Vec -> LAB
pointColor4d p = mkLAB (80 + 10*z) (7*w) (6*w)
  where
    z = p ! (3, 1)
    w = p ! (4, 1)

sortZ = sortBy (compare `on` key)
  where
    key :: Vec -> Double
    key v = v ! (3,1)
