import Graphics.Gloss
import Geometry
import qualified Data.Set as S
import Debug.Trace (trace)

main = do
  animate
    (InWindow "" (round screenWidth, round screenHeight) (0,0))
    black
    frame

-- Constants
screenWidth, screenHeight, pxPerUnit :: Float
screenWidth = 1600
screenHeight = 900
pxPerUnit = 20
scaling = Scale pxPerUnit pxPerUnit 

-- Drawing
frame t = scaling $ Pictures $
  [ Rotate (3*t) $ Color blue $ Translate x y $ circleSolid 0.25 | (x, y) <- coords ] ++
  [ Rotate (-1.1*t) $ Color red $ Translate x y $ circleSolid 0.25 | (x, y) <- coords]
    where coords = map vecToPoint latticeCircle 

latticeCircle :: [Vec]
latticeCircle = map hexToCartesian $ floodFill hexNeighbors (circleTest 20.2 . hexToCartesian) (0,0) 

floodFill :: (Eq a, Ord a) => (a -> [a]) -> (a -> Bool) -> a -> [a]
floodFill expand test x = S.toList $ go x S.empty
  where
    -- go :: a -> S.Set a -> S.Set a
    go x accum
      | test x == False  = accum
      | S.member x accum  = accum
      | otherwise  = foldl (\prev n -> prev . go n ) id neighbors $ (S.insert x accum) 
      where
        neighbors = expand x

circleTest :: Double -> Vec -> Bool
circleTest r v = vmag v < r

type HexCoord = (Int, Int)

hexNeighbors :: HexCoord -> [HexCoord]
hexNeighbors (x, y) = [(x-1,y),(x-1,y+1),(x,y+1),(x+1,y),(x+1,y-1),(x,y-1)]

hexToCartesian :: HexCoord -> Vec
hexToCartesian (x,y) = fmap (x'*) axisX + fmap (y'*) axisY
  where
    x' = fromIntegral x
    y' = fromIntegral y
    axisX = vec [1, 0]
    axisY = vec [1/2, sqrt 3 / 2]

