module ColorSpace
  ( gray
  , rgb
  , hexColor
  , gradStops
  , linearGradient
  , hueGradient
  -- , circleGradient
  )
where

import Data.Convertible
import Data.Prizm.Color
  ( RGB(..)
  , LAB(..)
  , LCH(..)
  , ColorCoord(..)
  , mkRGB
  , mkLAB
  )
import qualified Data.Matrix as M
import Geometry
import Graphics.Gloss
  ( Color
  , makeColor
  )

degToRad d = tau * d / 360
radToDeg r = 360 * r / tau

rgb :: Integer -> Integer -> Integer -> LAB
rgb r g b = convert $ mkRGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

gray :: LAB
gray = mkLAB 50 0 0

hexColor :: Int -> RGB
hexColor n = mkRGB r g b
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


instance Convertible RGB Color where
  safeConvert (unRGB -> ColorCoord(r, g, b)) = Right $ makeColor r' g' b' 1.0
    where
      r' = realToFrac r / 255
      g' = realToFrac g / 255
      b' = realToFrac b / 255


instance Convertible LAB Color where
  safeConvert = convertVia (undefined :: RGB)


instance Convertible LCH Color where
  safeConvert = convertVia (undefined :: RGB)
