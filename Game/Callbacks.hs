module Game.Callbacks where

import Graphics.UI.GLUT
import Data.IORef

import Coroutine

import Game.Keyboard
import Game.Game (Game)
import Game.Rendering

type GameRef = IORef Game
type KeyboardRef = IORef Keyboard
type CallbackRefs = (KeyboardRef, GameRef)

display :: CallbackRefs -> DisplayCallback
display (kb, gr) = do
  clear [ColorBuffer]
  keyboard <- get kb
  game <- get gr

  let (rects, nextGame) = runC game keyboard

  writeIORef gr nextGame

  mapM_ renderShape rects
  flush

  postRedisplay Nothing

handleKeyboard :: CallbackRefs -> KeyboardMouseCallback
handleKeyboard (kb, _) k ks _ _ = modifyIORef kb (handleKeyEvent k ks)
