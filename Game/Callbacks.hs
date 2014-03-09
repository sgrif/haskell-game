module Game.Callbacks where

import Graphics.UI.GLUT
import Data.IORef

import Coroutine

import Game.DeltaClock
import Game.Keyboard
import Game.Game (Game)
import Game.Rendering

type ClockRef = IORef DeltaClock
type GameRef = IORef Game
type KeyboardRef = IORef Keyboard
type CallbackRefs = (ClockRef, KeyboardRef, GameRef)

display :: CallbackRefs -> DisplayCallback
display (cr, kb, gr) = do
  clear [ColorBuffer]
  keyboard <- get kb
  game <- get gr
  delta <- getDelta cr

  let (rects, nextGame) = runC game (keyboard, delta)

  writeIORef gr nextGame

  mapM_ renderShape rects
  flush

  postRedisplay Nothing

handleKeyboard :: CallbackRefs -> KeyboardMouseCallback
handleKeyboard (_, kb, _) k ks _ _ = modifyIORef kb (handleKeyEvent k ks)
