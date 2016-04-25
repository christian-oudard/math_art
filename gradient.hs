import Geometry
import ColorSpace
import Data.Prizm.Color (CIELAB(..))
import Graphics.Gloss
import Data.Matrix ((!))

main = do
  display
    (InWindow "" (round screenWidth, round screenHeight) (0,0))
    (toGloss gray)
    drawing
  return ()

-- red' = CIELAB 50 50 0
red' = rgb 255 0 0
blue' = CIELAB 30 0 (-50)

uniformStops n = [0..n]

drawing :: Picture
drawing = scaling $ Pictures $ [ drawSquare (colorAtIndex i) i | i <- [-7..7] ]
  -- [ Color (toGloss red') $ Polygon $ pointsToPath $ squareAt $ vec[0,1]

colorAtIndex = linearGradient red' blue' . indexToStop
indexToStop i = (i - (-7)) / (7 -(-7))

drawSquare color i =  Color (toGloss color) $ polygon
  where
    polygon = Polygon $ pointsToPath $ squareAt $ vec[i,0]

interpolateVec :: Vec -> Vec -> Double -> Vec
interpolateVec start end s = start + fmap (*s) (end - start)

linearGradient :: CIELAB Double -> CIELAB Double -> Double -> CIELAB Double
linearGradient start end s = 
  let start' = colorToVec start
      end' = colorToVec end
   in vecToColor $ interpolateVec start' end' s

colorToVec :: CIELAB Double -> Vec
colorToVec (CIELAB l a b) = vec [l, a, b]

vecToColor :: Vec -> CIELAB Double
vecToColor v = CIELAB (v!(1,1)) (v!(2,1)) (v!(3,1))

-- Move to GlossHelper.hs?
pointsToPath :: [Vec] -> [Point]
pointsToPath vecs = [
  (realToFrac x, realToFrac y) |
  v <- vecs, let x = v!(1,1), let y = v!(2,1)
  ]

squares = [ squareAt (vec[p,0]) | p <- [-7..7] ]
squareAt :: Vec -> [Vec]
squareAt p = map (p+) $
  [ vec [-1/2,-1/2]
  , vec [-1/2,1/2]
  , vec [1/2,1/2]
  , vec [1/2,-1/2]
  ]

-- Constants
screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 100
scaling = Scale pxPerUnit pxPerUnit
