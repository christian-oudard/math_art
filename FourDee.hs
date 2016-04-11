module FourDee
  ( Vec
  , vec
  , vecToCoord
  , hyperCube
  , flatten2d
  , rot2d
  , rot3d
  , rot4d
  , rotNd
  , embedMatrix
  , switchAxes
  , tau
  ) where

import Data.Matrix as M
import qualified Data.Vector as V

type Vec = Matrix Double

vec :: [Double] -> Vec
vec = colVector . V.fromList

vecToCoord :: Vec -> (Float, Float)
vecToCoord v = (realToFrac x, realToFrac y)
  where
    x = v ! (1,1)
    y = v ! (2,1)

rot2d theta = M.transpose $ M.fromLists [
    [cos theta, sin theta], -- new X axis
    [-sin theta, cos theta] -- new Y axis
  ]

rot3d theta = embedMatrix (rot2d theta) (identity 3)
rot4d theta = embedMatrix (rot2d theta) (identity 4)
rotNd n theta = embedMatrix (rot2d theta) (identity n)

switchAxes a1 a2 = switchRows a1 a2 . switchCols a1 a2

hyperCube :: Int -> [Vec]
hyperCube n = map vec $ crossProduct $ replicate n [1, -1]  

flatten2d = submatrix 1 2 1 1

-- TODO: a more explicit cross product implementation
crossProduct :: [[a]] -> [[a]]
crossProduct = sequence

embedMatrix :: Matrix a -> Matrix a -> Matrix a
-- Put matrix a on top of matrix b, where a is smaller.
embedMatrix a b = matrix (nrows b) (ncols b) (\(x, y) ->
  if x <= nrows a && y <= ncols a
  then a ! (x, y)
  else b ! (x, y)
  )

tau = 2*pi
