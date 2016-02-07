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
  colors <- genColors 3000 gen
  let sortedColors = sortBy cmpColor colors
  display
    (InWindow "" (round screenWidth, round screenHeight) (0,0))
    (toScreen black)
    (colorStar sortedColors)
  return ()

cmpColor = compare `on` (\c -> (lightness c, - chroma c))

genColors n gen = sequence $ replicate n $ genColor gen
genColor gen = do
  r <- genComponent gen
  g <- genComponent gen
  b <- genComponent gen
  return $ rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)

genComponent :: GenIO -> IO Int
genComponent g = uniformR (0,255) g

-- Constants
screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 2
scaling = Scale pxPerUnit pxPerUnit 

-- Colors
black = rgb 0 0 0
gray = CIELCH 50 0 0
white = rgb 255 255 255
red = rgb 255 0 0
green = rgb 0 255 0
blue = rgb 0 0 255
cyan = rgb 0 255 255
magenta = rgb 255 0 255
yellow = rgb 255 255 0

rgb :: Integer -> Integer -> Integer -> CIELCH Double
rgb r g b = fromRGB $ RGB r g b

toScreen :: CIELCH Double -> Color
toScreen color = toScreenAlpha color 1
toScreenAlpha color alpha = makeColor r g b alpha
  where
    (RGB r' g' b') = toRGB color
    r = realToFrac r' / 255
    g = realToFrac g' / 255
    b = realToFrac b' / 255


lightness (CIELCH l _ _) = realToFrac l
chroma (CIELCH _ c _) = realToFrac c
hue (CIELCH _ _ h) = degToRad $ realToFrac h

tau = 2*pi
degToRad d = tau * d / 360
radToDeg r = 360 * r / tau

colorStar :: [CIELCH Double] -> Picture
colorStar colors = scaling $ Pictures $ map colorDot colors

colorDot c = Color (toScreenAlpha c 0.3) $ Translate x y $ circleSolid 6
  where (x, y) = polar (hue c) (chroma c)

colorLine c = Color (toScreenAlpha c 0.6) $ Line [(x/2, y/2), (x, y)]
  where (x, y) = polar (hue c) (chroma c)

origin :: (Float, Float)
origin = (0, 0)

polar :: Float -> Float -> (Float, Float)
polar theta r = (r * cos theta, r * sin theta)
