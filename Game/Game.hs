module Game.Game where

import Control.Category hiding ((.))
import Control.Arrow (arr)
import Control.Applicative
import Data.Traversable (sequenceA)
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

halfSize :: Double
halfSize = ballSize / 2

leftWall :: Double
leftWall = -1 + halfSize

rightWall :: Double
rightWall = 1 - halfSize

bottomWall :: Double
bottomWall = -1 + halfSize

topWall :: Double
topWall = 1 - halfSize

ballPos :: Coroutine Velocity Point
ballPos = scanC (\p -> movePoint p >>> boundBallPos) ballInitPos

boundBallPos :: Point -> Point
boundBallPos (Point x y) = Point boundedX boundedY where
  boundedX = min rightWall $ max leftWall x
  boundedY = min topWall $ max bottomWall y

ballVel :: Coroutine Input Velocity
ballVel = (,) <$> ballVelX <*> ballVelY

ballVelX :: Coroutine Input Double
ballVelX = arr $ sumOfKeysByDelta [(left, -1), (right, 1)]

ballVelY :: Coroutine Input Double
ballVelY = arr $ sumOfKeysByDelta [(down, -1), (up, 1)]

sumOfKeysByDelta :: [(Key, Double)] -> Input -> Double
sumOfKeysByDelta pairs (kb, delta) =
  sum $ valuesForPressedKeys kb (map applyDelta pairs)
    where applyDelta (key, val) = (key, val * delta)

ball :: Coroutine Input Rectangle
ball = fmap (makeSquare ballSize) $ ballVel >>> ballPos

game :: Game
game = sequenceA [ball]
