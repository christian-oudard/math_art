module Geometry
  ( Vec
  , vec
  , vdot
  , vmag2
  , vmag
  , vnorm
  , vperp2d
  , vcross3d
  , vmul
  , vdiv
  , vmidpoint
  , lerp
  , vlerp
  , getX
  , getY
  , getZ
  , vecToList
  , vecToPoint
  , hyperCubePoints
  , hyperCubeEdges
  , octaplex
  , octaplexEdges
  , cartesianProduct
  , rot2d
  , rot3dx
  , rot3dy
  , rot3dz
  , rot4d
  , rotation3d
  , rotation4d
  , embedMatrix
  , switchAxes
  , tau
  , circle2d
  , degToRad
  , radToDeg
  ) where

import Data.Matrix as M
import qualified Data.Vector as V

type Vec = Matrix Double -- 1-column matrix

vec :: [Double] -> Vec
vec = colVector . V.fromList

getX, getY, getZ :: Vec -> Double
getX v = get_ 1 v
getY v = get_ 2 v
getZ v = get_ 3 v
get_ n v = v ! (n,1)

vdot :: Vec -> Vec -> Double
vdot a b = V.sum $ getCol 1 $ elementwise (*) a b

vmag2 :: Vec -> Double
vmag2 v = vdot v v

vmag :: Vec -> Double
vmag = sqrt . vmag2

vnorm :: Vec -> Vec
vnorm v = if m > 0 then vdiv m v else v
  where m = vmag v

vperp2d :: Vec -> Vec
vperp2d v = vec [-y, x]
  where
    x = getX v
    y = getY v

vcross3d :: Vec -> Vec -> Vec
vcross3d v1 v2 = vec [x,y,z]
  where
    [x1, y1, z1] = V.toList $ getCol 1 v1
    [x2, y2, z2] = V.toList $ getCol 1 v2
    x = y1*z2 - y2*z1
    y = z1*x2 - z2*x1
    z = x1*y2 - x2*y1

vmul :: Double -> Vec -> Vec
vmul c = fmap (*c)

vdiv :: Double -> Vec -> Vec
vdiv c = fmap (/c)

vmidpoint :: Vec -> Vec -> Vec
vmidpoint a b = vdiv 2 (a+b)


lerp :: Double -> Double -> (Double -> Double)
lerp start end s = start + s * (end - start)

vlerp :: Vec -> Vec -> (Double -> Vec)
-- Linear interpolation on vectors.
vlerp start end s = start + (vmul s (end - start))

data Line = Line Vec Vec -- point, tangent
  deriving (Show)

instance Eq Line where
  a == b  = ax0 == bx0 && at == bt
    where
      (Line ax0 at) = canonicalLine a
      (Line bx0 bt) = canonicalLine b

closestOnLine :: Vec -> Line -> Vec
closestOnLine p line@(Line x0 t) = lineAt line sMin 
  where
    sMin = -(v `vdot` t) / (vmag2 t)
    v = x0 - p

lineAt :: Line -> Double -> Vec
lineAt (Line x0 t) = vlerp x0 (x0 + t)

canonicalLine :: Line -> Line
canonicalLine line@(Line point tangent) = Line point' tangent'
  where
    tangent' = vnorm tangent
    point' = closestOnLine origin line
    origin = zero (nrows point) 1

vecToList :: Vec -> [Double]
vecToList = V.toList . M.getCol 1

vecToPoint :: Vec -> (Float, Float)
vecToPoint = (\[x,y] -> (x,y)) . map realToFrac . take 2 . vecToList

rot2d theta = M.transpose $ M.fromLists [
    [cos theta, sin theta], -- new X axis
    [-sin theta, cos theta] -- new Y axis
  ]

rot3dx alpha = switchAxes 2 3 $ switchAxes 3 1 $ rot3dz alpha
rot3dy beta = switchAxes 1 3 $ switchAxes 3 2 $ rot3dz beta
rot3dz gamma = embedMatrix (rot2d gamma) (identity 3)

rotation3d t' = let t = t'*(4/5) in (
  (rot3dx $ t * tau/61)
  * (rot3dy $ t * tau/73) 
  * (rot3dz $ t * tau/97) 
  )

rot4d theta = embedMatrix (rot2d theta) (identity 4)

rotation4d t' = let t = t' in (
  (switchAxes 2 3 $ rot4d $ t * tau/73)
  * (switchAxes 2 4 $ rot4d $ -t * tau/83)
  * (switchAxes 1 2 $ switchAxes 2 4 $ rot4d $ t * tau/97)
  * (switchAxes 1 2 $ switchAxes 2 3 $ rot4d $ -t * tau/103)
  * (switchAxes 1 3 $ switchAxes 2 4 $ rot4d $ t * tau/117)
  )

switchAxes a1 a2 = switchRows a1 a2 . switchCols a1 a2

hyperCubePoints :: Int -> [Vec]
hyperCubePoints n = map vec $ cartesianProduct $ replicate n [1, -1]

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
    shiftNodes = map (+dist)
    shiftEdges = map (\(a,b) -> (a+dist, b+dist))
    dist = 2^(n-1)

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [axis] = [ [v] | v <- axis ]
cartesianProduct (axis:rest) = [ v:r | v <- axis, r <- cartesianProduct rest ]

octaplex = map vec
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
octaplexEdges =
  hyperCubeEdges 4 ++
  zip (repeat 16) [0..7]
  ++ zip (repeat 17) [8..15]
  ++ zip (repeat 18) ([0..3] ++ [8..11])
  ++ zip (repeat 19) ([4..7] ++ [12..15])
  ++ zip (repeat 20) [0,1,4,5,8,9,12,13]
  ++ zip (repeat 21) [2,3,6,7,10,11,14,15]
  ++ zip (repeat 22) [0,2..14]
  ++ zip (repeat 23) [1,3..15]

embedMatrix :: Matrix a -> Matrix a -> Matrix a
-- Put matrix a on top of matrix b, where a is smaller.
embedMatrix a b = matrix (nrows b) (ncols b) (\(x, y) ->
  if x <= nrows a && y <= ncols a
  then a ! (x, y)
  else b ! (x, y)
  )

tau = 2*pi

circle2d :: Vec -> Double -> (Double -> Vec)
circle2d center radius s = center + (rot2d (tau*s) * radiusVector)
  where
    radiusVector = vec [radius, 0]

-- triangleCenter3d :: Vec -> Vec -> Vec -> Vec
-- triangleCenter3d p q r = 
--   let
--     a = q - p
--     b = r - p
--     m1 = vmidpoint p q
--     m2 = vmidpoint p r
--     n = vnorm $ vcross3d a b
--     a' = rotateAxis3 n
--   in
--     zero 3 1

-- rotateAxis3 :: Vec -> Double -> (Vec -> Vec)
-- rotateAxis3 axis theta =  

degToRad d = tau * d / 360
radToDeg r = 360 * r / tau

