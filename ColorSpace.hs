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
  , linearGradient
  -- , rainbowGradient
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

toGloss :: CIELAB Double -> Color
toGloss color = toGlossAlpha color 1
toGlossAlpha color alpha = makeColor r g b alpha
  where
    (RGB r' g' b') = toRGB color
    r = realToFrac r' / 255
    g = realToFrac g' / 255
    b = realToFrac b' / 255

gray :: CIELAB Double
gray = CIELAB 50 0 0

rgb :: Integer -> Integer -> Integer -> CIELAB Double
rgb r g b = fromRGB $ RGB r g b

hexColor :: Integer -> CIELAB Double
hexColor n = rgb r g b
  where
    (n', b) = n `divMod` 0x100
    (r, g) = n' `divMod` 0x100

labL, labA, labB :: CIELAB Double -> Double
labL (CIELAB l _ _) = realToFrac l
labA (CIELAB _ c _) = realToFrac c
labB (CIELAB _ _ h) = realToFrac h
labHue c = atan2 (labA c) (labB c)
labChroma c = sqrt $ (labA c)^2 + (labB c)^2

degToRad d = tau * d / 360
radToDeg r = 360 * r / tau

linearGradient :: CIELAB Double -> CIELAB Double -> Double -> CIELAB Double
linearGradient start end s = vecToColor $ vlerp start' end' s
  where
    start' = colorToVec start
    end' = colorToVec end

-- rainbowGradient :: CIELAB Double -> CIELAB Double -> CIELAB Double ->  (Double -> CIELAB Double)
-- rainbowGradient a b c s = vecToColor $ circle s
--   where
--     [a', b', c'] = map colorToVec [a, b, c]
--     circle = circle3p a b c

colorToVec :: CIELAB Double -> Vec
colorToVec (CIELAB l a b) = vec [l, a, b]

vecToColor :: Vec -> CIELAB Double
vecToColor v = CIELAB (getX v) (getY v) (getZ v)
