import ColorSpace
import Graphics.Gloss

import Data.Convertible
import Data.Matrix ((!))
import Data.Function (on)
import Data.Prizm.Color (ColorCoord(..), mkRGB)
import Data.Prizm.Color.CIE (LAB(unLAB), mkLAB, LCH)
import Geometry

import Data.List (sortBy)


main :: IO ()
main = animate
  (InWindow "" (round screenWidth, round screenHeight) (0,0))
  (convert $ grayN 20)
  (frame allDots)

allDots :: [LAB]
allDots = edgeColors ++ axisL ++ axisAB 25 ++ axisAB 50 ++ axisAB 75 ++ rainbowDots

rainbowDots :: [LAB]
rainbowDots = filter inBounds $ map convert $ circleRainbow 50 28

circleRainbow :: Double -> Double -> [LCH]
circleRainbow l chroma = gradStops 64 $ hueGradient l chroma 0 360

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
    rgbGrad lo hi = map (convert . grad lo hi) $ linSpace 16
      where grad (r1,g1,b1) (r2,g2,b2) s = mkRGB (round $ lerp r1 r2 s) (round $ lerp g1 g2 s) (round $ lerp b1 b2 s)
    r0b0g0 = (0,0,0)
    r0b0g1 = (0,0,255)
    r0b1g0 = (0,255,0)
    r0b1g1 = (0,255,255)
    r1b0g0 = (255,0,0)
    r1b0g1 = (255,0,255)
    r1b1g0 = (255,255,0)
    r1b1g1 = (255,255,255)



-- Constants
screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 3
scaling = Scale pxPerUnit pxPerUnit

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
drawDot c v = Color (convert c) $ Translate x y $ circleSolid 3
  where
    (x, y) = vecToPoint v

colorPosition :: LAB -> Vec
colorPosition (unLAB -> ColorCoord(l, a, b)) = vec [a, b, l - 50]

