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

purple = hexColor 0x6c1c64
orange = hexColor 0xc7482b

greenIcecream = hexColor 0x7ecf92
yellowIcecream = hexColor 0xd6a642
purpleIcecream = hexColor 0x4b3c8a

uniformStops n = [0..n]

drawing :: Picture
drawing =
  scaling $
  Translate 0.0 (-7.0) $ Pictures $ [ rainbowArc c i | (i, c) <- zip positions colors ]
  where
    grad = hueGradient 60 40 0 (0.8 * tau)
    colors = gradStops (length positions) grad
    positions = [0..6]

icecreamRainbow = Pictures
  [ rainbowArc yellowIcecream 0
  , rainbowArc greenIcecream 1
  , rainbowArc purpleIcecream 2
  ]

rainbowArc color n = Color (toGloss color) $
  thickArc 0 180 (7.0 + n) 1.0

icecreamGrad = linearGradient greenIcecream purpleIcecream

-- drawing = scaling $ Pictures $ [ drawSquare c i | (i, c) <- zip positions colors ]
--   where
--     colors = gradStops (length positions) icecreamGrad
--     positions = [-4 .. 4]


drawSquare color i =  Color (toGloss color) $ polygon
  where
    polygon = Polygon $ pointsToPath $ squareAt $ vec[i,0]

-- Move to GlossHelper.hs?
pointsToPath :: [Vec] -> [Point]
pointsToPath vecs = [
  (realToFrac x, realToFrac y) |
  v <- vecs, let x = v!(1,1), let y = v!(2,1)
  ]

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
pxPerUnit = 50
scaling = Scale pxPerUnit pxPerUnit
