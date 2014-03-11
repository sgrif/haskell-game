module Game.Controls where

import Graphics.UI.GLUT (Key(..), SpecialKey(..))

import Game.Keyboard

up :: Key
up = SpecialKey KeyUp

down :: Key
down = SpecialKey KeyDown

left :: Key
left = SpecialKey KeyLeft

right :: Key
right = SpecialKey KeyRight

valuesForPressedKeys :: Keyboard -> [(Key, a)] -> [a]
valuesForPressedKeys kb = map snd . filter (isKeyDown kb . fst)
