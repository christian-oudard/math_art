-- What can we learn about complicated functions?
-- maxima and minima
-- amplitude range
-- frequency distribution
-- derivatives and antiderivatives
-- convolution?
-- one octave noise
-- scale power independently of function shape
-- energy(t) = amplitude(t) ^ 2

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import System.Random.MWC
import System.Random.MWC.Distributions

main = do
  g <- createSystemRandom
  noiseFunc <- makeNoiseFunc g

  play (InWindow "" (1600, 900) (0,0))
    black
    60
    (noiseFunc, 0)
    makePicture
    handleEvent
    stepWorld

type State = ((Float -> Float), Float)

initialState :: State
initialState = (id, 0)

stepWorld :: Float -> State -> State
stepWorld time (f, t) = (f, t + time)

handleEvent :: Event -> State -> State
handleEvent event state = state

makePicture :: State -> Picture
makePicture (f, t) = Scale 200 200 $ Pictures
  [ Color white $ Circle rad
  , Color blue $ Line [(x - t, f x) | x <- rangeAt t 3 0.05]
  ]
  where
    rad = (1 + f t)

makeNoiseFunc :: GenIO -> IO (Float -> Float)
makeNoiseFunc g = do
  coeffs <- oscillatorCoeffs 100 g
  return $ oscillatorSum coeffs

oscillatorCoeffs :: Int -> GenIO -> IO [(Float, Float, Float)]
oscillatorCoeffs n g = sequence . replicate n $ loop
  where
    loop = do
      -- Frequencies are log-uniform distributed.
      freq' <- uniformR (log 0.2, log 2.5) g :: IO Double
      let freq = exp freq'
      -- Amplitude is based on uniform power distribution, where power is
      -- proportional to frequency squared times amplitude squared.
      -- freq^2 * amp^2 = uniform(1, 2)
      -- amp^2 = uniform(1 / freq^2, 2 / freq^2)
      amp' <- uniformR (1 / freq^2, 2 / freq^2) g :: IO Double
      let amp = sqrt amp' / 150
      -- phase is uniform around the circle
      phase <- uniform g :: IO Double

      return (realToFrac freq, realToFrac amp, realToFrac phase)

oscPower freq amp = (freq^2 * amp^2) / 2

oscillatorSum :: [(Float, Float, Float)] -> (Float -> Float)
oscillatorSum coeffs = \t -> sum [oscillator p a f t | (p, a, f) <- coeffs]

oscillator freq amp phase t = amp * sinT (freq * (t - phase*tau))

diffract t = cosT (t) * bump 1 t
example t = 
  ( 0.2 * sinT (t/5) * bump 6 (t-20)
  + 0.02 * sinT (0.7*t) * bump 4 (t-25)
  + 0.1 * bump 20 (t-10)
  )

rangeAt :: Float -> Float -> Float -> [Float]
rangeAt t0 sigma step = [t0-sigma, t0-sigma+step .. t0+sigma]

bump sigma t = exp (-(t)^2 / (2 * sigma ^ 2))

timeLoop :: Float -> Float -> Float
timeLoop length t = t `mod'` length

tau = 2*pi
sinT t = sin (tau * t)
cosT t = cos (tau * t)
tanT t = tan (tau * t)

-- divMod for Reals taken from Data.Fixed
div' :: (Real a,Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))
divMod' :: (Real a,Integral b) => a -> a -> (b,a)
divMod' n d = (f,n - (fromIntegral f) * d) where
    f = div' n d
mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where
    f = div' n d

aa t = 1/2 * sigmoid t - 3/4 * sigmoid (t-3) + 1/2 * sinT(t)
sigmoid t = 1 / (1 + exp (-t))
