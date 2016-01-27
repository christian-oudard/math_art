-- Multiple function graphs
-- Color generating

import Graphics.Gloss
import System.Random.MWC
import System.Random.MWC.Distributions

main = do
  g <- createSystemRandom
  f <- makeNoiseFunc g
  display
    (InWindow "" (round screenWidth, round screenHeight) (0,0))
    black
    (picture f)

-- Constants
screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 100
scaling = Scale pxPerUnit pxPerUnit 

leftBound = (-screenWidth / 2) / pxPerUnit
rightBound = (screenWidth / 2) / pxPerUnit

-- Drawing
grey = greyN 0.5

picture :: (Float -> Float) -> Picture
picture f = scaling $ Pictures
  [ Color grey $ Line $ plot $ const 1
  , Color grey $ Line $ plot $ const 0
  , Color grey $ Line $ plot $ const (-1)
  , Color white $ Line $ plot $ f
  , Color green $ Line $ plot $ diff f
  ]

plot f = [(x, f x) | x <- screenMesh $ 1/100]

screenMesh :: Float -> [Float]
screenMesh step = mesh leftBound rightBound step

mesh l' r' step = [step * (fromIntegral i) | i <- [l..r]]
  where
    l = ceiling $ l' / step
    r = floor $ r' / step

-- Functions
makeNoiseFunc :: GenIO -> IO (Float -> Float)
makeNoiseFunc g = do
  coeffs <- oscillatorCoeffs 100 g
  return $ oscillatorSum coeffs

oscillatorCoeffs :: Int -> GenIO -> IO [(Float, Float, Float)]
oscillatorCoeffs n g = sequence . replicate n $ loop
  where
    loop = do
      -- Frequencies are log-uniform distributed.
      freq' <- uniformR (log 1, log 5) g :: IO Double
      let freq = exp freq'
      -- Amplitude is based on uniform power distribution, where power is
      -- proportional to frequency squared times amplitude squared.
      -- freq^2 * amp^2 = uniform(1, 2)
      -- amp^2 = uniform(1 / freq^2, 2 / freq^2)
      amp' <- uniformR (1 / freq^2, 2 / freq^2) g :: IO Double
      let amp = sqrt amp' / 15
      -- phase is uniform around the circle
      phase <- uniform g :: IO Double

      return (realToFrac freq, realToFrac amp, realToFrac phase)

oscillatorSum :: [(Float, Float, Float)] -> (Float -> Float)
oscillatorSum coeffs = \t -> sum [oscillator p a f t | (p, a, f) <- coeffs]

oscillator freq amp phase t = amp * sin (freq * (t - phase*tau))

oscPower freq amp = (freq^2 * amp^2) / 2


diff :: (Float -> Float) -> (Float -> Float)
-- Secant method for derivative.
diff f x = (f (x + e) - f x) / e
  where e = 1/1000


-- Misc
tau = 2*pi

