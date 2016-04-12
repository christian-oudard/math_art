import FourDee
import ColorSpace
import Graphics.Gloss
import Data.Prizm.Color (CIELAB(..))
import Data.Matrix ((!))
import Data.List (sortBy)
import Data.Function (on)

main = do
  animate (InWindow "" (1600, 900) (0,0))
    black
    frame

frame t = frame' (realToFrac t)
frame' :: Double -> Picture
frame' t = Scale 200 200 $ Pictures
  [ drawLines lines
  , drawPoints $ sortZ rotatedPoints
  ]
  where
    points = hyperCubePoints 4
    rotatedPoints = map (rotation *) points
    lines = [ (rotatedPoints !! i, rotatedPoints !! j) | (i, j) <- hyperCubeEdges 4 ]
    rotation = rotation4d t

drawLines :: [(Vec, Vec)] -> Picture
drawLines lines = Pictures [ Color color $ Line [vecToPoint a, vecToPoint b] | (a, b) <- lines ]
  where color = toGloss $ CIELAB 80 0 0

drawPoints :: [Vec] -> Picture
drawPoints points = Pictures $ map drawPoint points

drawPoint :: Vec -> Picture
drawPoint p = Translate x y $ Color color $ circleSolid (0.05 + 0.005 * z')
  where
    (x, y) = vecToPoint p
    z' = realToFrac $ p ! (3, 1)
    -- color = toGloss $ CIELAB 90 0 0
    color = toGloss $ pointColor4d p

pointColor4d :: Vec -> CIELAB Double
pointColor4d p = CIELAB (80 + 10*z) (7*w) (6*w)
  where
    z = p ! (3, 1)
    w = p ! (4, 1)

sortZ = sortBy (compare `on` key)
  where
    key :: Vec -> Double
    key v = v ! (3,1)
