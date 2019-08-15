import ColorSpace
import Graphics.Gloss

import Data.Matrix ((!))
import Data.List (sortBy)
import Data.Function (on)
import Data.Prizm.Color.CIE (LAB(..))
import System.Random.MWC (GenIO, createSystemRandom, uniformR)
import Geometry

main = do
  -- gen <- createSystemRandom
  -- randomColors <- genColors 300 gen
  -- let gradientColors = map (rainbowGradient gray 35) [0, (1/40) .. 1]
  animate
    (InWindow "" (round screenWidth, round screenHeight) (0,0))
    (toGloss gray)
    (frame colors)
  return ()

pairwise, pairwiseCircular :: [a] -> [(a,a)]
pairwise xs = zip xs (tail xs)
pairwiseCircular xs = (last xs, head xs) : pairwise xs

gradientPolygon :: [LAB] -> (Double -> LAB)
gradientPolygon colors s = (gradients !! i) s'
  where
    gradients = [ linearGradient a b | (a,b) <- pairwiseCircular colors ]
    len = length colors
    (i, s') = splitIndex len s

splitIndex :: Int -> Double -> (Int, Double)
-- Take a segment from 0 to 1, and split it into n segments from
-- 0 to 1. Return an appropriate index into the set of n segments.
splitIndex n s = 
  let
    n' = fromIntegral n
    wrapped = mod' s 1
    (segmentNumber, s') = divMod' wrapped (1/n')
    s'' = s' * n'
  in 
    (segmentNumber, s'')


div' :: (Real a, Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))
mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where f = div' n d
divMod' :: (Real a, Integral b) => a -> a -> (b,a)
divMod' n d = (div' n d, mod' n d)

rgbGradient (r1,g1,b1) (r2,g2,b2) s = rgb (round $ lerp r1 r2 s) (round $ lerp g1 g2 s) (round $ lerp b1 b2 s)

gr = hexColor 0x7ecf92
ye = hexColor 0xd6a642
br = hexColor 0x513520
pu = hexColor 0x4b3c8a
pn = hexColor 0xf07687

grad = gradientPolygon [ye, gr, pu, pn]
iceCreamColors = map (grad . (/40)) [0..40]
r0b0g0 = (0,0,0)
r0b0g1 = (0,0,255)
r0b1g0 = (0,255,0)
r0b1g1 = (0,255,255)
r1b0g0 = (255,0,0)
r1b0g1 = (255,0,255)
r1b1g0 = (255,255,0)
r1b1g1 = (255,255,255)

rgbGrad lo hi = map (rgbGradient lo hi) $ map (/20) [0..20]

edgeColors =
  rgbGrad r0b0g0 r0b0g1 ++
  rgbGrad r0b0g0 r0b1g0 ++
  rgbGrad r0b0g0 r1b0g0 ++
  rgbGrad r0b0g1 r0b1g1 ++
  rgbGrad r0b0g1 r1b0g1 ++
  rgbGrad r0b1g0 r0b1g1 ++
  rgbGrad r0b1g0 r1b1g0 ++
  rgbGrad r1b0g0 r1b0g1 ++
  rgbGrad r1b0g0 r1b1g0 ++
  rgbGrad r1b1g0 r1b1g1 ++
  rgbGrad r1b0g1 r1b1g1 ++
  rgbGrad r0b1g1 r1b1g1

colors = iceCreamColors ++ edgeColors


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
    key :: (LAB, Vec) -> Double
    key (c, v) = v ! (3,1)

drawDot :: LAB -> Vec -> Picture
drawDot c v = Color (toGloss c) $ Translate x y $ circleSolid 3
  where
    (x, y) = vecToPoint v

colorPosition c = vec [labA c, labB c, labL c - 50]

