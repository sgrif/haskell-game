module Game.DeltaClock (
  DeltaClock,
  initClock,
  getDelta
  ) where

import Data.Time.Clock.POSIX
import Data.IORef

type Delta = Double
data DeltaClock = DeltaClock POSIXTime

initClock :: IO DeltaClock
initClock = fmap DeltaClock getPOSIXTime

getDelta :: IORef DeltaClock -> IO Delta
getDelta clockRef = do
  clock <- readIORef clockRef
  now <- getPOSIXTime

  let (delta, newClock) = computeDelta now clock

  writeIORef clockRef newClock
  return delta

computeDelta :: POSIXTime -> DeltaClock -> (Delta, DeltaClock)
computeDelta time (DeltaClock lastTime) =
  let deltaTime = time - lastTime
  in (timeToDelta deltaTime, DeltaClock time)

timeToDelta :: POSIXTime -> Delta
timeToDelta = realToFrac
