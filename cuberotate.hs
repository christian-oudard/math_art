import FourDee
import Graphics.Gloss

main = do
  animate (InWindow "" (1600, 900) (0,0))
    black
    frame

frame t = frame' (realToFrac t)
frame' :: Double -> Picture
frame' t = Scale 200 200 $ drawPoints $ geometry
  where
    geometry = map (rotation *) $ octaplex
    rotation = rotation4d t

drawPoints :: [Vec] -> Picture
drawPoints points = Pictures $ map drawPoint points

drawPoint p =
  let (x, y) = vecToCoord p
   in Translate x y $ Color white $ circleSolid 0.03
