import Graphics.Gloss
  ( display
  , Picture(Pictures, Scale, Translate, Line, Color)
  , Display(InWindow)
  , Color
  , makeColor
  , circleSolid
  )

import Data.List (sortBy)
import Data.Function (on)
import Data.Prizm.Color (RGB(..), CIELCH(..))
import Data.Prizm.Color.CIE.LCH (toRGB, fromRGB)
import System.Random.MWC (GenIO, createSystemRandom, uniformR)

main = do
  gen <- createSystemRandom
  colors <- genColors 10000 gen
  display
    (InWindow "" (round screenWidth, round screenHeight) (0,0))
    (toGloss gray)
    (colorStar $ sortLightness colors)
  return ()

-- Constants
screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 3
scaling = Scale pxPerUnit pxPerUnit 

-- Colors
gray = CIELCH 50 0 0

rgb :: Integer -> Integer -> Integer -> CIELCH Double
rgb r g b = fromRGB $ RGB r g b

lightness (CIELCH l _ _) = realToFrac l / 100
chroma (CIELCH _ c _) = realToFrac c
hue (CIELCH _ _ h) = degToRad $ realToFrac h
blueyellow c = chroma c * sin (hue c)
greenred c = chroma c * cos (hue c)

genColors n gen = sequence $ replicate n $ genColor gen
genColor gen = do
  r <- genComponent gen
  g <- genComponent gen
  b <- genComponent gen
  return $ rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
genComponent :: GenIO -> IO Int
genComponent g = uniformR (0,255) g

toGloss :: CIELCH Double -> Color
toGloss color = toGlossAlpha color 1
toGlossAlpha color alpha = makeColor r g b alpha
  where
    (RGB r' g' b') = toRGB color
    r = realToFrac r' / 255
    g = realToFrac g' / 255
    b = realToFrac b' / 255

tau = 2*pi
degToRad d = tau * d / 360
radToDeg r = 360 * r / tau

-- Drawing

colorStar :: [CIELCH Double] -> Picture
colorStar colors = scaling $ Pictures $ map colorDot colors

colorDot c = Color (toGlossAlpha c 0.8) $ Translate x y $ circleSolid 6
  where (x, y) = colorPositionPolar c

colorLine c = Color (toGlossAlpha c 0.6) $ Line [(0, 0), (x, y)]
  where (x, y) = polar (hue c) (chroma c)

colorPositionPolar c = polar (hue c) (chroma c)
colorPositionSide c = (-greenred c, -100 + 200 * lightness c)

sortLightness = sortBy cmp
  where cmp = compare `on` (\c -> (lightness c, chroma c))
sortBlueYellow = sortBy cmp
  where cmp = compare `on` (\c -> -blueyellow c)

filterColorsLightness = filter pred
  where
    pred c =
      ( lightness c > 0.3
      && lightness c < 0.8
      )

filterColorsRing = filter pred
  where
    pred c =
      ( lightness c > 0.6
      && lightness c < 0.7
      && chroma c > 20
      && chroma c < 43
      )

polar :: Float -> Float -> (Float, Float)
polar theta r = (r * cos theta, r * sin theta)
