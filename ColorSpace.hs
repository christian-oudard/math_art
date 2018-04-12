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

import Data.Convertible
import Data.Prizm.Color (RGB(..), LAB(..), ColorCoord(..), mkRGB, mkLAB)
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

rgb :: Integer -> Integer -> Integer -> LAB
rgb r g b = convert $ mkRGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

gray :: LAB
gray = mkLAB 50 0 0

lab2rgb :: LAB -> RGB
lab2rgb = convert

toGloss :: LAB -> Color
toGloss color = toGlossAlpha color 1
toGlossAlpha color alpha = makeColor r g b alpha
  where
    ColorCoord (r', g', b') = unRGB $ lab2rgb $ color
    r = realToFrac r' / 255
    g = realToFrac g' / 255
    b = realToFrac b' / 255

labL, labA, labB :: LAB -> Double
labL color  = realToFrac l
  where ColorCoord (l, _, _) = unLAB color
labA color  = realToFrac a
  where ColorCoord (_, a, _) = unLAB color
labB color  = realToFrac b
  where ColorCoord (_, _, b) = unLAB color
labHue c = atan2 (labA c) (labB c)
labChroma c = sqrt $ (labA c)^2 + (labB c)^2

hexColor :: Integer -> LAB
hexColor n = rgb r g b
  where
    (n', b) = n `divMod` 0x100
    (r, g) = n' `divMod` 0x100

polarColor :: Double -> Double -> Double -> LAB
polarColor l chroma hue = mkLAB l a b
  where
    a = chroma * cos hue
    b = chroma * sin hue

gradStops :: Int -> (Double -> LAB) -> [LAB]
gradStops n grad = map (grad . f) [0..n-1]
  where
    f i = (i' / (n' - 1))
      where
        i' = realToFrac i
        n' = realToFrac n

linearGradient :: LAB -> LAB -> (Double -> LAB)
linearGradient start end s = vecToColor $ vlerp start' end' s
  where
    start' = colorToVec start
    end' = colorToVec end

hueGradient:: Double -> Double -> Double -> Double -> (Double -> LAB)
hueGradient l chroma startHue endHue s = polarColor l chroma hue
  where
    hue = lerp startHue endHue s

-- circleGradient :: LAB -> LAB -> LAB ->  (Double -> LAB)
-- circleGradient a b c s = vecToColor $ circle s
--   where
--     [a', b', c'] = map colorToVec [a, b, c]
--     circle = circle3p a b c

colorToVec :: LAB -> Vec
colorToVec color = vec [l, a, b]
  where (ColorCoord (l, a, b)) = unLAB color

vecToColor :: Vec -> LAB
vecToColor v = mkLAB (getX v) (getY v) (getZ v)

-- Tests
-- labL $ polarColor 50 45 25 == 50.0
-- labChroma $ polarColor 50 45 25 == 25.0
-- labHue $ polarColor 50 (tau/8) 25 == tau/8
