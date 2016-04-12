import ColorSpace
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
    (colorCloud $ sortLightness colors)
  return ()

-- Constants
screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 3
scaling = Scale pxPerUnit pxPerUnit 

-- Colors
genColors n gen = sequence $ replicate n $ genColor gen
genColor gen = do
  r <- genComponent gen
  g <- genComponent gen
  b <- genComponent gen
  return $ rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
genComponent :: GenIO -> IO Int
genComponent g = uniformR (0,255) g

-- Drawing

colorCloud :: [CIELCH Double] -> Picture
colorCloud colors = scaling $ Pictures $ map colorDot colors

colorDot c = Color (toGlossAlpha c 0.8) $ Translate x y $ circleSolid 1.5
  where (x, y) = colorPositionPolar c

colorLine c = Color (toGlossAlpha c 0.6) $ Line [(0, 0), (x, y)]
  where (x, y) = polar (realToFrac $ hue c) (realToFrac $ chroma c)

colorPositionPolar c = polar (realToFrac $ hue c) (realToFrac $ chroma c)
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
