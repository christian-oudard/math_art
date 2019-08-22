module ColorSpace
  ( grayN
  , hexColor
  , gradStops
  , linearGradient
  , hueGradient
  , circleRainbow
  , rgbR
  , rgbG
  , rgbB
  , labL
  , labA
  , labB
  , lchChroma
  , lchHue
  , inBounds
  , linSpace
  , genColor
  , genColors
  , vecToColor
  , colorToVec
  )
where

import System.Random.MWC (GenIO, uniformR)
import Control.Monad (replicateM)
import Data.Convertible
import Data.Prizm.Color
  ( RGB(..)
  , LAB(..)
  , LCH(..)
  , ColorCoord(..)
  , mkRGB
  , mkLAB
  , mkLCH
  )
import Geometry
import Graphics.Gloss
  ( Color
  , makeColor
  )

grayN :: Double -> LAB
grayN l = mkLAB l 0 0

hexColor :: Int -> RGB
hexColor n = mkRGB r g b
  where
    (n', b) = n `divMod` 0x100
    (r, g) = n' `divMod` 0x100

rgbR, rgbG, rgbB :: Convertible c RGB => c -> Double
rgbR (unRGB . convert -> ColorCoord(r, _, _)) = realToFrac r
rgbG (unRGB . convert -> ColorCoord(_, g, _)) = realToFrac g
rgbB (unRGB . convert -> ColorCoord(_, _, b)) = realToFrac b

labL, labA, labB :: Convertible c LAB => c -> Double
labL (unLAB . convert -> ColorCoord(l, _, _)) = realToFrac l
labA (unLAB . convert -> ColorCoord(_, a, _)) = realToFrac a
labB (unLAB . convert -> ColorCoord(_, _, b)) = realToFrac b

lchChroma, lchHue :: Convertible c LCH => c -> Double
lchChroma (unLCH . convert -> ColorCoord(_, chroma, _)) = realToFrac chroma
lchHue (unLCH . convert -> ColorCoord(_, _, hue)) = realToFrac hue


inBounds :: Convertible c RGB => c -> Bool
inBounds c =
  0 < rgbR c && rgbR c < 255 &&
  0 < rgbG c && rgbG c < 255 &&
  0 < rgbB c && rgbB c < 255

gradStops :: Int -> (Double -> a) -> [a]
gradStops n grad = map grad $ linSpace 0 1 n

linSpace :: Double -> Double -> Int -> [Double]
linSpace start stop n = map f [0..n-1]
  where
    f i = lerp start stop $ i' / (n' - 1)
      where
        i' = realToFrac i
        n' = realToFrac n

linearGradient :: LAB -> LAB -> (Double -> LAB)
linearGradient start end s = vecToColor $ vlerp start' end' s
  where
    start' = colorToVec start
    end' = colorToVec end

hueGradient:: Double -> Double -> Double -> Double -> (Double -> LCH)
hueGradient l chroma startHue endHue s = mkLCH l chroma hue
  where
    hue = lerp startHue endHue s

circleRainbow :: LAB -> Double -> [LAB]
circleRainbow center chroma = map (addColor center) stops
  where
    stops :: [LAB]
    stops = map convert $ gradStops 64 $ hueGradient 0 chroma 0 360

colorToVec :: LAB -> Vec
colorToVec color = vec [l, a, b]
  where (ColorCoord (l, a, b)) = unLAB color

vecToColor :: Vec -> LAB
vecToColor v = mkLAB (getX v) (getY v) (getZ v)

addColor :: LAB -> LAB -> LAB
addColor a b = vecToColor $ colorToVec a + colorToVec b

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

instance Convertible RGB RGB where
  safeConvert = Right

instance Convertible LAB LAB where
  safeConvert = Right

instance Convertible LCH LCH where
  safeConvert = Right


-- Color Generation
genColors :: Int -> GenIO -> IO [RGB]
genColors n gen = replicateM n $ genColor gen

genColor :: GenIO -> IO RGB
genColor gen = do
  r <- genComponent gen
  g <- genComponent gen
  b <- genComponent gen
  return $ mkRGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
  where
    genComponent :: GenIO -> IO Int
    genComponent = uniformR (0,255)

