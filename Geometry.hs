module Geometry
  ( Vec
  , vec
  , vmag
  , vnorm
  , getX
  , getY
  , getZ
  , getW
  , vecToList
  , vecToPoint
  , hyperCubePoints
  , hyperCubeEdges
  , octaplex
  , octaplexEdges
  , rot2d
  , rot3d
  , rot4d
  , rotNd
  , rotation3d
  , rotation4d
  , embedMatrix
  , switchAxes
  , tau
  ) where

import Data.Matrix as M
import qualified Data.Vector as V

type Vec = Matrix Double

vec :: [Double] -> Vec
vec = colVector . V.fromList

vmag :: Vec -> Double
vmag v = sqrt $ sum $ fmap (^2) v

getX, getY, getZ, getW :: Vec -> Double
getX v = get_ 1 v
getY v = get_ 2 v
getZ v = get_ 3 v
getW v = get_ 4 v
get_ n v = v ! (n,1)

vecToList :: Vec -> [Double]
vecToList = V.toList . M.getCol 1

vnorm :: Vec -> Vec
vnorm v = if m > 0 then fmap (/m) v else v
  where m = vmag v

vecToPoint :: Vec -> (Float, Float)
vecToPoint = (\[x,y] -> (x,y)) . map realToFrac . take 2 . vecToList

rot2d theta = M.transpose $ M.fromLists [
    [cos theta, sin theta], -- new X axis
    [-sin theta, cos theta] -- new Y axis
  ]

rot3d theta = embedMatrix (rot2d theta) (identity 3)
rot4d theta = embedMatrix (rot2d theta) (identity 4)
rotNd n theta = embedMatrix (rot2d theta) (identity n)

rotation3d t' = let t = t'*(4/5) in (
  (rot3d $ t * tau/61)
  * (switchAxes 2 3 $ rot3d $ t * tau/73) 
  * (switchAxes 1 2 $ switchAxes 2 3 $ rot3d $ t * tau/97) 
  )

rotation4d t' = let t = t'*(1/2)+100 in (
  -- (rot4d $ t * tau/61)
  (switchAxes 2 3 $ rot4d $ t * tau/73) 
  * (switchAxes 2 4 $ rot4d $ t * tau/83) 
  * (switchAxes 1 2 $ switchAxes 2 3 $ rot4d $ t * tau/97) 
  * (switchAxes 1 2 $ switchAxes 2 4 $ rot4d $ t * tau/103) 
  * (switchAxes 1 3 $ switchAxes 2 4 $ rot4d $ t * tau/117) 
  )

switchAxes a1 a2 = switchRows a1 a2 . switchCols a1 a2

hyperCubePoints :: Int -> [Vec]
hyperCubePoints n = map vec $ crossProduct $ replicate n [1, -1]

hyperCubeEdges :: Int -> [(Int, Int)]
hyperCubeEdges = snd . hyperCubeGraph

hyperCubeGraph :: Int -> ([Int], [(Int, Int)])
hyperCubeGraph 1 = ([0, 1], [(0, 1)])
hyperCubeGraph n = (prevNodes ++ newNodes, prevEdges ++ newEdges ++ crossEdges)
  where
    (prevNodes, prevEdges) = hyperCubeGraph (n-1)
    newNodes = shiftNodes prevNodes
    newEdges = shiftEdges prevEdges
    crossEdges = zip prevNodes newNodes
    shiftNodes ps = map (+dist) ps
    shiftEdges es = map (\(a,b) -> (a+dist,b+dist)) es
    dist = 2^(n-1)

crossProduct :: [[a]] -> [[a]]
crossProduct (axis:[]) = [ [v] | v <- axis ]
crossProduct (axis:rest) = [ v:r | v <- axis, r <- crossProduct rest ]

octaplex = map vec $
  [ [1, 1, 1, 1]
  , [1, 1, 1, -1]
  , [1, 1, -1, 1]
  , [1, 1, -1, -1]
  , [1, -1, 1, 1]
  , [1, -1, 1, -1]
  , [1, -1, -1, 1]
  , [1, -1, -1, -1]
  , [-1, 1, 1, 1]
  , [-1, 1, 1, -1]
  , [-1, 1, -1, 1]
  , [-1, 1, -1, -1]
  , [-1, -1, 1, 1]
  , [-1, -1, 1, -1]
  , [-1, -1, -1, 1]
  , [-1, -1, -1, -1]
  , [2, 0, 0, 0]
  , [-2, 0, 0, 0]
  , [0, 2, 0, 0]
  , [0, -2, 0, 0]
  , [0, 0, 2, 0]
  , [0, 0, -2, 0]
  , [0, 0, 0, 2]
  , [0, 0, 0, -2]
  ]

octaplexEdges :: [(Int, Int)]
octaplexEdges = (
  hyperCubeEdges 4 ++
  zip (repeat 16) [0..7]
  ++ zip (repeat 17) [8..15]
  ++ zip (repeat 18) ([0..3] ++ [8..11])
  ++ zip (repeat 19) ([4..7] ++ [12..15])
  ++ zip (repeat 20) [0,1,4,5,8,9,12,13]
  ++ zip (repeat 21) [2,3,6,7,10,11,14,15]
  ++ zip (repeat 22) [0,2..14]
  ++ zip (repeat 23) [1,3..15]
  )

embedMatrix :: Matrix a -> Matrix a -> Matrix a
-- Put matrix a on top of matrix b, where a is smaller.
embedMatrix a b = matrix (nrows b) (ncols b) (\(x, y) ->
  if x <= nrows a && y <= ncols a
  then a ! (x, y)
  else b ! (x, y)
  )

tau = 2*pi
