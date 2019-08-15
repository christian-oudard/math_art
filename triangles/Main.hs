import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

black' = makeColor 0.1 0.12 0.1 0
green' = makeColor 0.0 0.4 0.3 0.6
blue' = makeColor 0.12 0.1 0.7 1.0 

main = do
   play (InWindow "" (1600, 900) (0,0))
       black'
       40
       initialState
       makePicture
       handleEvent
       stepWorld

-- time
type State = Float

initialState :: State
initialState = 0

stepWorld :: Float -> State -> State
stepWorld time t = t + time
 
scaleAt t = Scale (1+s) (1+s/3)
  where s = (1/30) * sin (turn * t/15)

translateAt t = Translate s (s/3)
  where s = (1/5) * sin (turn * t/19)

makePicture :: State -> Picture
makePicture t = Scale 110 110 $ Pictures
  [ translateAt t $ scaleAt t $ rot (t*turn / 240) $ blueTriangles
  , translateAt (t*1.08) $ scaleAt (t*1.03) $ rot (-t*turn / 360) $ greenTriangles
  ]

triangleGrid :: Float -> [Vector]
triangleGrid n =
  [ mulSV x xAxis + mulSV y yAxis | x <- [-n..n], y <- [-n..n] ]

greenTriangles = Color green' $ Pictures
  [trans v $ rot (turn/6) triangle | v <- triangleGrid 20 ]

blueTriangles = Color blue' $ Pictures
  [ trans v $ triangle | v <- triangleGrid 20]

origin = (0, 0) :: Vector
xAxis = (1, 0) :: Vector
yAxis = (1/2, sqrt 3 / 2) :: Vector

trans :: Vector -> Picture -> Picture
trans (x, y) = Translate x y

rot :: Float -> Picture -> Picture
rot a = rotate ((360 / turn) * a)

turn = 2*pi

triangle = Polygon [origin, xAxis, yAxis]

handleEvent :: Event -> State -> State
handleEvent event state = state
