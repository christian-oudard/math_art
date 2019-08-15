module Main where

import Geometry
import ColorSpace ()
import Graphics.Gloss
import Data.Prizm.Color.CIE (LAB(..), mkLAB)
import Data.Matrix ((!))
import Data.List (sortBy)
import Data.Function (on)
import Data.Convertible

bg1, bg2, lineColor :: Color
bg1 = convert $ mkLAB 5 (-5) (-15)
bg2 = convert $ mkLAB 10 (-5) (-20)
lineColor = convert $ mkLAB 70 5 7

main = do
  animate (InWindow "" (1600, 900) (0,0))
    bg1
    frame

frame t = frame' (realToFrac t)
frame' :: Double -> Picture
frame' t = Scale 200 200 $ Pictures $
  [ Pictures [ color lineColor $ Translate x y $ circleSolid 0.03 | (x,y) <- map vecToPoint rotatedPoints ]
  , Pictures [ color lineColor $ Line [vecToPoint a, vecToPoint b] | (a, b) <- lines ]
  ]
  where
    points = hyperCubePoints 4
    rotatedPoints = map (rotation *) points
    lines = [ (rotatedPoints !! i, rotatedPoints !! j) | (i, j) <- hyperCubeEdges 4 ]
    rotation = rotation4d (t/3)

pointColor4d :: Vec -> LAB
pointColor4d p = mkLAB (80 + 10*z) (7*w) (6*w)
  where
    z = p ! (3, 1)
    w = p ! (4, 1)

sortZ = sortBy (compare `on` key)
  where
    key :: Vec -> Double
    key v = v ! (3,1)
