import ColorSpace
import Optimization
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)

import Data.Convertible
import Data.Matrix ((!))
import Data.Function (on)
import Data.Prizm.Color (ColorCoord(..), mkRGB)
import Data.Prizm.Color.CIE (LAB(unLAB), mkLAB, LCH)
import Geometry

import Data.List (sortBy)

-- Constants
pxPerUnit = 6
scaling = Scale pxPerUnit pxPerUnit

main :: IO ()
main = do
  screenSize <- getScreenSize
  animate
    (InWindow "" screenSize (0,0))
    (convert $ grayN 50)
    (frame allDots)

allDots :: [LAB]
-- allDots = edgeColors ++ axisL ++ dotGrid 25 ++ dotGrid 50 ++ dotGrid 75
allDots = edgeColors ++ axisL ++ biggestRainbow' 25 ++ biggestRainbow' 50 ++ biggestRainbow' 75
-- allDots = edgeColors ++ dotGrid 75 ++ dotGrid 25

axisL :: [LAB]
axisL = gradStops 16 $ linearGradient (mkLAB 0 0 0) (mkLAB 100 0 0)

axisAB, axisA, axisB :: Double -> [LAB]
axisAB l = axisA l ++ axisB l
axisA l = gradStops 32 $ linearGradient minA maxA
  where
    minA = boundaryColor (grayN l) $ mkLAB l (-100) 0
    maxA = boundaryColor (grayN l) $ mkLAB l 100 0

axisB l = gradStops 32 $ linearGradient minB maxB
  where
    minB = boundaryColor (grayN l) $ mkLAB l 0 (-100)
    maxB = boundaryColor (grayN l) $ mkLAB l 0 100

edgeColors :: [LAB]
edgeColors =
  rgbGrad r0b0g0 r0b0g1 ++
  rgbGrad r0b0g0 r0b1g0 ++
  rgbGrad r0b0g0 r1b0g0 ++
  rgbGrad r0b0g1 r0b1g1 ++
  rgbGrad r0b0g1 r1b0g1 ++
  rgbGrad r0b1g0 r0b1g1 ++
  rgbGrad r0b1g0 r1b1g0 ++
  rgbGrad r1b0g0 r1b0g1 ++
  rgbGrad r1b0g0 r1b1g0 ++
  rgbGrad r1b1g0 r1b1g1 ++
  rgbGrad r1b0g1 r1b1g1 ++
  rgbGrad r0b1g1 r1b1g1
  where
    rgbGrad lo hi = map (convert . grad lo hi) $ linSpace 0 1 16
      where grad (r1,g1,b1) (r2,g2,b2) s = mkRGB (round $ lerp r1 r2 s) (round $ lerp g1 g2 s) (round $ lerp b1 b2 s)
    r0b0g0 = (0,0,0)
    r0b0g1 = (0,0,255)
    r0b1g0 = (0,255,0)
    r0b1g1 = (0,255,255)
    r1b0g0 = (255,0,0)
    r1b0g1 = (255,0,255)
    r1b1g0 = (255,255,0)
    r1b1g1 = (255,255,255)



-- Drawing

frame colors t' = scaling $ Pictures $ map (uncurry drawDot) (sortZ drawData)
  where
    t = realToFrac t'
    initialPositions = map colorPosition colors
    currentPositions = map (rotation3d t *) initialPositions
    drawData = zip colors currentPositions

sortZ = sortBy (compare `on` key)
  where
    key :: (LAB, Vec) -> Double
    key (c, v) = v ! (3,1)

drawDot :: LAB -> Vec -> Picture
drawDot c v = Color (convert c) $ Translate x y $ circleSolid 2
  where
    (x, y) = vecToPoint v

colorPosition :: LAB -> Vec
colorPosition (unLAB -> ColorCoord(l, a, b)) = vec [a, b, l - 50]

