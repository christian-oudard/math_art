module ColorSpace
  ( toGloss
  , toGlossAlpha
  , gray
  , rgb
  , lightness
  , chroma
  , hue
  , blueyellow
  , greenred
  )
where

import Data.Prizm.Color (RGB(..), CIELCH(..))
import Data.Prizm.Color.CIE.LCH (toRGB, fromRGB)
import Graphics.Gloss
  ( Color
  , makeColor
  )

toGloss :: CIELCH Double -> Color
toGloss color = toGlossAlpha color 1
toGlossAlpha color alpha = makeColor r g b alpha
  where
    (RGB r' g' b') = toRGB color
    r = realToFrac r' / 255
    g = realToFrac g' / 255
    b = realToFrac b' / 255

gray :: CIELCH Double
gray = CIELCH 50 0 0

rgb :: Integer -> Integer -> Integer -> CIELCH Double
rgb r g b = fromRGB $ RGB r g b

lightness, chroma, hue, blueyellow, greenred :: CIELCH Double -> Double
lightness (CIELCH l _ _) = realToFrac l / 100
chroma (CIELCH _ c _) = realToFrac c
hue (CIELCH _ _ h) = degToRad $ realToFrac h
blueyellow c = chroma c * sin (hue c)
greenred c = chroma c * cos (hue c)

tau = 2*pi
degToRad d = tau * d / 360
radToDeg r = 360 * r / tau

