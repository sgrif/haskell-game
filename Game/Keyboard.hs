module Game.Keyboard (
  Keyboard,
  initKeyboard,
  handleKeyEvent,
  isKeyDown
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.UI.GLUT (Key(..), KeyState(..))

newtype Keyboard = Keyboard (Set Key) deriving (Show)

initKeyboard :: Keyboard
initKeyboard = Keyboard Set.empty

handleKeyEvent :: Key -> KeyState -> Keyboard -> Keyboard
handleKeyEvent k s = keyHandlerForState s k

keyHandlerForState :: KeyState -> Key -> Keyboard -> Keyboard
keyHandlerForState Up   = removeKey
keyHandlerForState Down = addKey

removeKey :: Key -> Keyboard -> Keyboard
removeKey k (Keyboard s) = Keyboard $ Set.delete k s

addKey :: Key -> Keyboard -> Keyboard
addKey k (Keyboard s) = Keyboard $ Set.insert k s

isKeyDown :: Keyboard -> Key -> Bool
isKeyDown (Keyboard s) k = Set.member k s
