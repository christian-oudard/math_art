module Main where

import Data.Complex
import Geometry (radToDeg)
import ColorSpace ()
import Graphics.Gloss
  ( display
  , animate
  , Picture(Pictures, Scale, Translate, Rotate, Line, Color)
  , Display(InWindow)
  , Color
  , color
  , Point
  , makeColor
  , circleSolid
  )
import Data.Convertible (Convertible(..), convert)
import Data.Convertible.Utils (convertVia)
import Data.Prizm.Color (ColorCoord(..), RGB(unRGB))
import Data.Prizm.Color.CIE (LCH, mkLCH, LAB)

bgColor, fieldColor, lineColor :: Color
bgColor = convert $ mkLCH 7 5 (5/16 * tau)
fieldColor = convert $ mkLCH 20 35 (5/64 * tau)
lineColor = convert $ mkLCH 26 29 (3/64 * tau)


main = do
  animate
    (InWindow "" (round screenWidth, round screenHeight) (0,0))
    bgColor
    picture

screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 400
scaling = Scale pxPerUnit pxPerUnit 

-- Drawing
picture t' = scaling $ Pictures $
  [ color fieldColor $ circleSolid 1
  -- , color lineColor $ line $ map toDisk [-100, 100]
  -- , color lineColor $ line $ map toDisk $ circle 1
  , rotate (1.05*t) $ color lineColor $ line $ map toDisk $ spiral (tau / 54)
  , rotate (0.28*t) $ color lineColor $ line $ map toDisk $ spiral (tau / 18)
  , rotate (0.56*tau + 0.28*t) $ color lineColor $ line $ map toDisk $ spiral (tau / 18)
  ]
  where t = -0.4* realToFrac t'

line points = Line $ map toScreen points
rotate angle = Rotate $ realToFrac $ radToDeg $ realToFrac (-angle)

-- Coordinates
toScreen :: Complex Double -> Point
toScreen z = (realToFrac $ realPart z, realToFrac $ imagPart z)

toDisk z = mkPolar (tanh (r/2)) theta
  where (r, theta) = polar z
fromDisk z = mkPolar (2 * atanh r) theta
  where (r, theta) = polar z

circle :: Double -> [Complex Double]
circle r = points
  where
    points = [(r:+0) * exp (0:+a)| a <- angleSteps 0 1 (2^6)]

angleSteps :: Double -> Double -> Integer -> [Double]
angleSteps minTurns maxTurns stepsPerTurn =
  [lo + (fromIntegral i) * step | i <- [0 .. (ceiling steps)]]
  where
    lo = minTurns * tau
    hi = maxTurns * tau
    range = hi - lo
    steps = fromIntegral stepsPerTurn * range
    step = range / steps

spiral :: Double -> [Complex Double]
spiral slant = [f t | t <- angleSteps (-8) 5 (2^7)]
  where f = logSpiral 1 slant

logSpiral :: Double -> Double -> (Double -> Complex Double)
logSpiral radius slant t = (radius:+0) * exp (mkPolar t (tau / 4 - slant))

-- Miscellaneous
tau = 2*pi
