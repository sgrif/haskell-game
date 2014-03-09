module Game.Shape where

class Shape a where
  getPoints :: a -> [Point]

data Rectangle = Rectangle {
                 width :: Double,
                 height :: Double,
                 center :: Point
                 } deriving (Show, Eq)

makeSquare :: Double -> Point -> Rectangle
makeSquare size = Rectangle size size

instance Shape Rectangle where
  getPoints (Rectangle w h (Point cx cy)) =
    let hh = h / 2
        hw = w / 2
    in [
      Point (cx + hw) (cy + hh),
      Point (cx - hw) (cy + hh),
      Point (cx - hw) (cy - hh),
      Point (cx + hw) (cy - hh)
    ]

data Point = Point Double Double deriving (Show, Eq)
type Velocity = (Double, Double)

movePoint :: Point -> Velocity -> Point
movePoint (Point x y) (vx, vy) = Point (x + vx) (y + vy)
