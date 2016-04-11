import FourDee
import Graphics.Gloss

main = do
  animate (InWindow "" (1600, 900) (0,0))
    black
    frame

frame t = frame' (realToFrac t)
frame' :: Double -> Picture
frame' t = Scale 100 100 $ Color white $ drawPoints $ geometry
  where
    geometry = map flatten2d $ map (rotation *) $ hyperCube 4
    rotation = rotation4d t

rotation4d t = (
  (rot4d $ t * tau/60)
  * (switchAxes 2 3 $ rot4d $ t * tau/73) 
  * (switchAxes 1 3 $ rot4d $ t * tau/97) 
  * (switchAxes 2 4 $ rot4d $ t * tau/53) 
  * (switchAxes 1 4 $ rot4d $ t * tau/117) 
  )


drawPoints :: [Vec] -> Picture
drawPoints points = Pictures $ map (\(x, y) ->Translate x y $ filledCircle 0.1) $ map vecToCoord points
    
filledCircle :: Float -> Picture
filledCircle r = ThickCircle (r/2) r
