import Data.List (tails)
import Data.Maybe (fromMaybe)

import System.Random.MWC
import System.Random.MWC.Distributions (normal)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.MST

import Graphics.Gloss

dotColor = makeColor (1/5) (8/15) (4/5) 1.0
lineColor = makeColor (3/5) (2/5) (1/5) 1.0
dotRadius = 4.6

picWidth = 1000 :: Double
picHeight = 700 :: Double
centerX = picWidth / 2
centerY = picHeight / 2

main = do
  g <- createSystemRandom
  dots <- genDotsNormal g 7

  let graph = completeGraphFromPoints dots
      -- weightedGraph = addEdgeLengths graph
      -- spanningTree = msTree weightedGraph

  let drawing = drawGraph graph
   in display (InWindow "" (round picWidth, round picHeight) (100, 100)) black $ drawing

defaultGraph :: IO (Gr Point ())
defaultGraph = do
  g <- createSystemRandom
  dots <- genDotsNormal g 10
  return (completeGraphFromPoints dots)

noEdges :: [UEdge]
noEdges = []

edgeToUEdge :: Edge -> UEdge
edgeToUEdge (n1, n2) = (n1, n2, ())

allEdges :: Int -> [UEdge]
allEdges n = map edgeToUEdge $ pairs [1..n]

completeGraphFromPoints :: [Point] -> Gr Point ()
completeGraphFromPoints pts = mkGraph (zip [1..] pts) (allEdges (length pts))

addEdgeLengths :: Gr Point () -> Gr Point Float
addEdgeLengths g = gmap f g
  where f (p,v,l,s) = (map (addLengthLabel g v) p, v, l, map (addLengthLabel g v) s)

addLengthLabel :: Gr Point () -> Node -> ((), Node) -> (Float, Node)
addLengthLabel g v ((), x) = (dist (get v) (get x), x)
  where get x = fromMaybe (error "") (lab g x)

dist (xa, ya) (xb, yb) = sqrt $ (xb - xa)^2 + (yb - ya)^2

-- trimToMST :: Real b => Gr a b -> Gr a b
-- trimToMST g =
--   let tree = msTree g
--       g' = mkGraph

-- pathToEdges :: LPath a -> [LEdge a]
-- pathToEdges 

pairs :: [a] -> [(a,a)]
pairs s = [(x,y)|x:xt <- tails s, y <- xt]

genDotsGrid :: Double -> Double -> Double -> [Point]
genDotsGrid w h cellSize =
  [
    (realToFrac (x*cellSize), realToFrac (y*cellSize))
    | x <- [0..(w / cellSize)]
    , y <- [0..(h / cellSize)]
  ]


genDotsNormal :: GenIO -> Int -> IO [Point]
genDotsNormal g n = sequence . replicate n $ loop
  where
    loop = do
      x <- normal 0 150 g
      y <- normal 0 150 g
      return (realToFrac x, realToFrac y)

drawGraph :: Gr Point a -> Picture
drawGraph g = Pictures $
  [Color lineColor $ Line [a, b] | (a, b) <- lines]
  ++ [Translate x y $ Color dotColor $ circleSolid dotRadius | (x, y) <- pts]
    where
      pts = map (\(v, l) -> l) $ labNodes g
      lines = endpointLabels g

endpointLabels :: Gr a b -> [(a, a)]
endpointLabels g = map (\(a, b) -> (get a, get b)) (edges g)
   where get x = fromMaybe (error "") (lab g x)
