module Game.Game where

import Control.Category
import Control.Arrow (arr)
import Control.Applicative
import Data.Traversable

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
ballVelX =
  arr keyboardDir where
    keyboardDir kb
      | isKeyDown kb left  = -0.01
      | isKeyDown kb right = 0.01
      | otherwise          = 0

ballVelY :: Coroutine Keyboard Double
ballVelY =
  arr keyboardDir where
    keyboardDir kb
      | isKeyDown kb down = -0.01
      | isKeyDown kb up   = 0.01
      | otherwise         = 0

ball :: Coroutine Keyboard Rectangle
ball = fmap (makeSquare ballSize) $ ballVel >>> ballPos

game :: Game
game = sequenceA [ball]
