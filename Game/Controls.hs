module Game.Controls (up, down, left, right) where

import Graphics.UI.GLUT (Key(..), SpecialKey(..))

up :: Key
up = SpecialKey KeyUp

down :: Key
down = SpecialKey KeyDown

left :: Key
left = SpecialKey KeyLeft

right :: Key
right = SpecialKey KeyRight
