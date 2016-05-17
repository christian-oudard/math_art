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

greenIcecream = hexColor 0x7ecf92
yellowIcecream = hexColor 0xd6a642
purpleIcecream = hexColor 0x4b3c8a

uniformStops n = [0..n]

drawing :: Picture
drawing = scaling $ Pictures $ [ drawSquare (colorAtIndex i) i | i <- [-7..7] ]
  -- [ Color (toGloss red') $ Polygon $ pointsToPath $ squareAt $ vec[0,1]

grad = rainbowGradient greenIcecream yellowIcecream purpleIcecream

colorAtIndex :: Double -> CIELAB Double
colorAtIndex = grad . f
  where f i = (i - (-7)) / (7 -(-7))

drawSquare color i =  Color (toGloss color) $ polygon
  where
    polygon = Polygon $ pointsToPath $ squareAt $ vec[i,0]

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
