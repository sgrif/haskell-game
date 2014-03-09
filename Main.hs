import Graphics.UI.GLUT hiding (Point)
import Data.IORef

import Game.Game (game)
import Game.Callbacks
import Game.Keyboard (initKeyboard)

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Hello, World!"

  gameRef <- newIORef game
  kbRef <- newIORef initKeyboard
  let refs = (kbRef, gameRef)

  keyboardMouseCallback $= Just (handleKeyboard refs)
  displayCallback $= display refs

  mainLoop

