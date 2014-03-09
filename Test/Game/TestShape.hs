module Game.TestShape where

import Game.Shape
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary Rectangle where
  arbitrary = do
    w <- arbitrary
    h <- arbitrary
    p <- arbitrary
    return $ Rectangle w h p

instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Point x y

tests :: [Test]
tests =
  [testGroup "Game.Shape" [
    testProperty "getPoints Rectangle" test_getPoints_Rectangle,
    testProperty "moveShape Rectangle" test_moveShape_Rectangle
  ]]

test_getPoints_Rectangle :: Rectangle -> Bool
test_getPoints_Rectangle r =
  let (Rectangle w h (Point cx cy)) = r
      hw = w / 2
      hh = h / 2
      p1 = Point (cx + hw) (cy + hh)
      p2 = Point (cx - hw) (cy + hh)
      p3 = Point (cx - hw) (cy - hh)
      p4 = Point (cx + hw) (cy - hh)
  in getPoints r == [p1, p2, p3, p4]

test_moveShape_Rectangle :: Rectangle -> Velocity -> Bool
test_moveShape_Rectangle r (vx, vy) =
  let (Rectangle w h (Point cx cy)) = r
  in moveShape (vx, vy) r == (Rectangle w h $ Point (cx + vx) (cy + vy))
