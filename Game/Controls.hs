module Game.Controls where

import Graphics.UI.GLUT (Key(..), SpecialKey(..))
import Data.Monoid

import Game.Keyboard

up :: Key
up = SpecialKey KeyUp

down :: Key
down = SpecialKey KeyDown

left :: Key
left = SpecialKey KeyLeft

right :: Key
right = SpecialKey KeyRight

sumPressedKeys :: Monoid a => [(Key, a)] -> Keyboard -> a
sumPressedKeys pairs kb =
  mconcat $ map getValue pairs
    where getValue (key, val) = if isKeyDown kb key then val else mempty
