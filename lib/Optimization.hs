module Optimization
  ( biggestRainbow
  , biggestRainbow'
  , biggestRainbowAt
  , boundaryColor
  , dotGrid
  )
  where

import ColorSpace
  ( grayN
  , hueGradient
  , linearGradient
  , gradStops
  , inBounds
  , vecToColor
  , colorToVec
  , circleRainbow
  , linSpace
  )

import Data.Convertible
import Data.Prizm.Color.CIE ( LAB, mkLAB )

import Numeric.NLOPT
import Numeric.LinearAlgebra ( Vector, R, dot, fromList, toList )

import Data.Ord ( comparing )
import Data.List ( maximumBy )

-- Three-dimension problem, (A, B, Chroma).
-- -100 < A < 100
-- -100 < B < 100
-- 0 < Chroma < 100
biggestRainbow lightness = circleRainbow (mkLAB lightness a' b') chroma'
  where
    [a', b', chroma'] = toList (optimum lightness)

    x0 :: Vector R
    x0 = fromList [0, 0, 5]

    initialStep :: Maybe InitialStep
    initialStep = Just $ InitialStep $ fromList [5, 5, 5]

    lowerbounds, upperbounds :: Bounds
    lowerbounds = LowerBounds $ fromList [-100, -100, 0]
    upperbounds = UpperBounds $ fromList [100, 100, 100]
    stop :: NonEmpty StoppingCondition
    stop = ObjectiveRelativeTolerance 1e-5 :| []

    -- Find the largest rainbow that is in-bounds with the given lightness
    objective :: Double -> Vector R -> Double
    objective lightness x = negate $ 10*inBoundsScore + chromaScore
      where
        [a, b, chroma] = toList x
        rainbow = circleRainbow (mkLAB lightness a b) chroma
        n = length rainbow
        numInBounds = length $ filter inBounds rainbow
        inBoundsScore = fromIntegral numInBounds / fromIntegral n
        chromaScore = chroma / 100.0

    optimum lightness =
      case minimizeLocal problem x0 of
        Right sol -> solutionParams sol
        Left _ -> error "Optimizer error."
      where
        problem = LocalProblem 3 stop algorithm
        algorithm = NELDERMEAD (objective lightness) [lowerbounds, upperbounds] initialStep

-- Three-dimension problem, (A, B, Chroma).
-- -100 < A < 100
-- -100 < B < 100
-- 0 < Chroma < 100
biggestRainbow lightness = circleRainbow (mkLAB lightness a' b') chroma'
  where
    [a', b', chroma'] = toList (optimum lightness)

    x0 :: Vector R
    x0 = fromList [0, 0, 5]

    initialStep :: Maybe InitialStep
    initialStep = Just $ InitialStep $ fromList [5, 5, 5]

    lowerbounds, upperbounds :: Bounds
    lowerbounds = LowerBounds $ fromList [-100, -100, 0]
    upperbounds = UpperBounds $ fromList [100, 100, 100]
    stop :: NonEmpty StoppingCondition
    stop = ObjectiveRelativeTolerance 1e-5 :| []

    -- Find the largest rainbow that is in-bounds with the given lightness
    objective :: Double -> Vector R -> Double
    objective lightness x = negate $ 10*inBoundsScore + chromaScore
      where
        [a, b, chroma] = toList x
        rainbow = circleRainbow (mkLAB lightness a b) chroma
        n = length rainbow
        numInBounds = length $ filter inBounds rainbow
        inBoundsScore = fromIntegral numInBounds / fromIntegral n
        chromaScore = chroma / 100.0

    optimum lightness =
      case minimizeLocal problem x0 of
        Right sol -> solutionParams sol
        Left _ -> error "Optimizer error."
      where
        problem = LocalProblem 3 stop algorithm
        algorithm = NELDERMEAD (objective lightness) [lowerbounds, upperbounds] initialStep


bisect :: Double -> Double -> Double -> (Double -> Bool) -> Double
bisect lo hi eps test
  | (mid - lo) < eps  = lo
  | otherwise  =
    if test mid
    then bisect mid hi eps test
    else bisect lo mid eps test
  where
    mid = (hi + lo) / 2

boundaryColor :: LAB -> LAB -> LAB
boundaryColor start target = head $ dropWhile inBounds $ gradStops 100 $ linearGradient start target

boundaryColor start target = grad $ bisect 0 1 1e-2 test
  where
    grad = linearGradient start target
    test s = inBounds $ grad s

biggestRainbowAt center = circleRainbow center (optimumChroma center)

optimumChroma center = bisect 0 100 1e-2 test
  where test chroma = all inBounds $ circleRainbow center chroma

dotGrid :: Double -> [LAB]
dotGrid l = filter inBounds [ mkLAB l a b | a <- xs, b <- xs ]
  where
    xs = linSpace (-100) 100 40

biggestRainbow' :: Double -> [LAB]
biggestRainbow' l = biggestRainbowAt $ maximumBy (comparing optimumChroma) $ dotGrid l

