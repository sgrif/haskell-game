module Game.Game where

import Control.Category hiding ((.))
import Control.Arrow (arr)
import Control.Applicative
import Data.Traversable (sequenceA)
import Data.Monoid (Sum(..), getSum)
import Graphics.UI.GLUT (Key(..))

import Coroutine

import Game.Shape
import Game.Keyboard
import Game.Controls

type Game = Coroutine Keyboard [Rectangle]

ballSize :: Double
ballSize = 0.1

ballInitPos :: Point
ballInitPos = Point 0 0

ballPos :: Coroutine Velocity Point
ballPos = scanC movePoint ballInitPos

ballVel :: Coroutine Keyboard Velocity
ballVel = (,) <$> ballVelX <*> ballVelY

ballVelX :: Coroutine Keyboard Double
ballVelX = arr $ sumOfKeys [(left, -0.01), (right, 0.01)]

ballVelY :: Coroutine Keyboard Double
ballVelY = arr $ sumOfKeys [(down, -0.01), (up, 0.01)]

sumOfKeys :: [(Key, Double)] -> Keyboard -> Double
sumOfKeys pairs =
  getSum . sumPressedKeys (map sumPair pairs)
    where sumPair (key, val) = (key, Sum val)

ball :: Coroutine Keyboard Rectangle
ball = fmap (makeSquare ballSize) $ ballVel >>> ballPos

game :: Game
game = sequenceA [ball]
