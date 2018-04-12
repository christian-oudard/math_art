import Geometry
import ColorSpace
import Graphics.Gloss
import Data.Prizm.Color (LAB(..), mkLAB)

import Data.Matrix ((!))
import Data.List (sortBy)
import Data.Function (on)

bg1, bg2, lineColor :: Color
bg1 = toGloss $ mkLAB 5 (-5) (-15)
bg2 = toGloss $ mkLAB 10 (-5) (-20)
lineColor = toGloss $ mkLAB 70 5 7

main = do
  animate (InWindow "Octaplex" (1600, 900) (0,0))
    bg1
    frame


frame t = frame' (realToFrac t)
frame' :: Double -> Picture
frame' t = Scale 200 200 $ Pictures
  [ Color bg2 $ circleSolid 0.05
  , Color bg2 $ ThickCircle 2 0.05
  , Color lineColor $ drawLines lines
  , drawPoints $ rotatedPoints
  ]
  where
    points = octaplex
    rotatedPoints = map (rotation *) points
    lines = [ (rotatedPoints !! i, rotatedPoints !! j) | (i, j) <- octaplexEdges ]
    rotation = rotation4d t

drawLines :: [(Vec, Vec)] -> Picture
drawLines lines = Pictures [ Line [vecToPoint a, vecToPoint b] | (a, b) <- lines ]

drawPoints :: [Vec] -> Picture
drawPoints points = Pictures $ map drawPoint points

drawPoint :: Vec -> Picture
drawPoint p = Translate x y $ Color lineColor $ circleSolid 0.008
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
