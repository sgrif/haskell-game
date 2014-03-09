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

type Delta = Double
type Input = (Keyboard, Delta)
type Game = Coroutine Input [Rectangle]

ballSize :: Double
ballSize = 0.1

ballInitPos :: Point
ballInitPos = Point 0 0

ballPos :: Coroutine Velocity Point
ballPos = scanC movePoint ballInitPos

ballVel :: Coroutine Input Velocity
ballVel = (,) <$> ballVelX <*> ballVelY

ballVelX :: Coroutine Input Double
ballVelX = arr $ sumOfKeysByDelta [(left, -1), (right, 1)]

ballVelY :: Coroutine Input Double
ballVelY = arr $ sumOfKeysByDelta [(down, -1), (up, 1)]

sumOfKeysByDelta :: [(Key, Double)] -> Input -> Double
sumOfKeysByDelta pairs (kb, delta) =
  sumOfKeys (map applyDelta pairs) kb
    where applyDelta (key, val) = (key, val * delta)

sumOfKeys :: [(Key, Double)] -> Keyboard -> Double
sumOfKeys pairs =
  getSum . sumPressedKeys (map sumPair pairs)
    where sumPair (key, val) = (key, Sum val)

ball :: Coroutine Input Rectangle
ball = fmap (makeSquare ballSize) $ ballVel >>> ballPos

game :: Game
game = sequenceA [ball]
