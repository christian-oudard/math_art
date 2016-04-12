module ColorSpace
  ( toGloss
  , toGlossAlpha
  , gray
  , rgb
  , labL
  , labA
  , labB
  , labHue
  , labChroma
  )
where

import Data.Prizm.Color (RGB(..), CIELAB(..))
import Data.Prizm.Color.CIE.LAB (toRGB, fromRGB)
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

labL, labA, labB :: CIELAB Double -> Double
labL (CIELAB l _ _) = realToFrac l
labA (CIELAB _ c _) = realToFrac c
labB (CIELAB _ _ h) = realToFrac h
labHue c = atan2 (labA c) (labB c)
labChroma c = sqrt $ (labA c)^2 + (labB c)^2

tau = 2*pi
degToRad d = tau * d / 360
radToDeg r = 360 * r / tau

