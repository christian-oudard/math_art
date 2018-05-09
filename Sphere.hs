import Graphics.Gloss
import Control.Monad (replicateM)
import System.Random.MWC (GenIO, createSystemRandom, uniformR)
import System.Random.MWC.Distributions (standard)
import Geometry
import ColorSpace
import Data.Prizm.Color (LAB, mkLAB)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Vector as V
import qualified Data.Matrix as M

screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 300
scaling = Scale pxPerUnit pxPerUnit 

main = do
  g <- createSystemRandom
  points <- replicateM 2000 $ uniformSphere 3 g
  animate
    (InWindow "" (round screenWidth, round screenHeight) (0,0))
    black
    (picture points)

picture points t = scaling $ Pictures $ map (uncurry drawPoint) $ coloredPoints'
  where
    t' = realToFrac t
    colors = map pointColor points
    points' = map (rotation*) $ points
    coloredPoints = zip colors points'
    coloredPoints' = sortZ coloredPoints
    rotation = rotation3d t'

drawPoint :: LAB -> Vec -> Picture
drawPoint c p = Color (toGloss c) $ Translate x' y' $ circleSolid 0.02
  where (x',y') = vecToPoint p

pointColor :: Vec -> LAB
pointColor p = mkLAB (45 + z*15) (x*40) (y*40)
  where [x, y, z] = vecToList p

sortZ :: [(LAB, Vec)] -> [(LAB, Vec)]
sortZ = sortBy (compare `on` (\(c,p) -> getZ p))

gaussBall :: Int -> GenIO -> IO Vec
gaussBall n g = do
  rs <- replicateM n $ standard g
  return $ vec rs

uniformSphere :: Int -> GenIO -> IO Vec
uniformSphere n g = do
   v <- gaussBall n g
   return $ vnorm v

-- uniformBall :: Int -> IO Vec
