import ColorSpace
import Graphics.Gloss

import Data.Matrix ((!))
import Data.List (sortBy)
import Data.Function (on)
import Data.Prizm.Color (CIELAB(..))
import System.Random.MWC (GenIO, createSystemRandom, uniformR)

import FourDee

main = do
  gen <- createSystemRandom
  colors <- genColors 900 gen
  animate
    (InWindow "" (round screenWidth, round screenHeight) (0,0))
    (toGloss gray)
    (frame colors)
  return ()

-- Constants
screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 3
scaling = Scale pxPerUnit pxPerUnit

-- Color Generation
genColors n gen = sequence $ replicate n $ genColor gen
genColor gen = do
  r <- genComponent gen
  g <- genComponent gen
  b <- genComponent gen
  return $ rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
genComponent :: GenIO -> IO Int
genComponent g = uniformR (0,255) g

-- Drawing

frame colors t' = scaling $ Pictures $ map (uncurry drawDot) (sortZ drawData)
  where
    t = realToFrac t'
    initialPositions = map colorPosition colors
    currentPositions = map (rotation3d t *) initialPositions
    drawData = zip colors currentPositions

sortZ = sortBy (compare `on` key)
  where
    key :: (CIELAB Double, Vec) -> Double
    key (c, v) = v ! (3,1)

drawDot :: CIELAB Double -> Vec -> Picture
drawDot c v = Color (toGloss c) $ Translate x y $ circleSolid 3
  where
    (x, y) = vecToPoint v

colorPosition c = vec [labA c, labB c, labL c - 50]

