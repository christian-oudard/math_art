module ColorSpace
  ( toGloss
  , toGlossAlpha
  , gray
  , rgb
  , hexColor
  , labL
  , labA
  , labB
  , labHue
  , labChroma
  , gradStops
  , linearGradient
  , hueGradient
  -- , circleGradient
  )
where

import Data.Prizm.Color (RGB(..), CIELAB(..))
import Data.Prizm.Color.CIE.LAB (toRGB, fromRGB)
import qualified Data.Matrix as M
import Geometry
import Graphics.Gloss
  ( Color
  , makeColor
  )

-- Lightness from 0 to 
-- A and B within the range of about 0-100
degToRad d = tau * d / 360
radToDeg r = 360 * r / tau

rgb :: Integer -> Integer -> Integer -> CIELAB Double
rgb r g b = fromRGB $ RGB r g b

gray :: CIELAB Double
gray = CIELAB 50 0 0

toGloss :: CIELAB Double -> Color
toGloss color = toGlossAlpha color 1
toGlossAlpha color alpha = makeColor r g b alpha
  where
    (RGB r' g' b') = toRGB color
    r = realToFrac r' / 255
    g = realToFrac g' / 255
    b = realToFrac b' / 255

labL, labA, labB :: CIELAB Double -> Double
labL (CIELAB l _ _) = realToFrac l
labA (CIELAB _ c _) = realToFrac c
labB (CIELAB _ _ h) = realToFrac h
labHue c = atan2 (labA c) (labB c)
labChroma c = sqrt $ (labA c)^2 + (labB c)^2

hexColor :: Integer -> CIELAB Double
hexColor n = rgb r g b
  where
    (n', b) = n `divMod` 0x100
    (r, g) = n' `divMod` 0x100

polarColor :: Double -> Double -> Double -> CIELAB Double
polarColor l chroma hue = CIELAB l a b
  where
    a = chroma * cos hue
    b = chroma * sin hue

gradStops :: Int -> (Double -> CIELAB Double) -> [CIELAB Double]
gradStops n grad = map (grad . f) [0..n-1]
  where
    f i = (i' / (n' - 1))
      where
        i' = realToFrac i
        n' = realToFrac n

linearGradient :: CIELAB Double -> CIELAB Double -> (Double -> CIELAB Double)
linearGradient start end s = vecToColor $ vlerp start' end' s
  where
    start' = colorToVec start
    end' = colorToVec end

hueGradient:: Double -> Double -> Double -> Double -> (Double -> CIELAB Double)
hueGradient l chroma startHue endHue s = polarColor l chroma hue
  where
    hue = lerp startHue endHue s

-- circleGradient :: CIELAB Double -> CIELAB Double -> CIELAB Double ->  (Double -> CIELAB Double)
-- circleGradient a b c s = vecToColor $ circle s
--   where
--     [a', b', c'] = map colorToVec [a, b, c]
--     circle = circle3p a b c

colorToVec :: CIELAB Double -> Vec
colorToVec (CIELAB l a b) = vec [l, a, b]

vecToColor :: Vec -> CIELAB Double
vecToColor v = CIELAB (getX v) (getY v) (getZ v)

-- Tests
-- labL $ polarColor 50 45 25 == 50.0
-- labChroma $ polarColor 50 45 25 == 25.0
-- labHue $ polarColor 50 (tau/8) 25 == tau/8
