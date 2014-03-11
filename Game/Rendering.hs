module Game.Rendering where

import Graphics.UI.GLUT hiding (Point)
import Game.Shape

renderShape :: Shape a => a -> IO ()
renderShape s =
  renderPrimitive Polygon $
    mapM_ (vertex . pointToVertex3) (getPoints s)

pointToVertex3 :: Point -> Vertex3 GLdouble
pointToVertex3 (Point x y) = Vertex3 (gd x) (gd y) 0

gd :: Double -> GLdouble
gd = realToFrac
