module Coroutine where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Arrow
import Control.Category

newtype Coroutine i o = Coroutine { runC :: i -> (o, Coroutine i o) }

instance Functor (Coroutine i) where
  fmap f co = Coroutine $ \i ->
              let (o, co') = runC co i
              in (f o, fmap f co')

instance Applicative (Coroutine i) where
  pure x = Coroutine $ const (x, pure x)

  cof <*> cox = Coroutine $ \i ->
                let (f, cof') = runC cof i
                    (x, cox') = runC cox i
                in (f x, cof' <*> cox')

instance Category Coroutine where
  id = Coroutine $ \i -> (i, id)

  cof . cog = Coroutine $ \i ->
              let (x, cog') = runC cog i
                  (y, cof') = runC cof x
              in (y, cof' . cog')

instance Arrow Coroutine where
  arr f = Coroutine $ \i -> (f i, arr f)

  first co = Coroutine $ \(b, d) ->
             let (c, co') = runC co b
             in ((c, d), first co')

evalList :: Coroutine i o -> [i] -> [o]
evalList _ [] = []
evalList co (i:is) =
  let (o, co') = runC co i
  in o:evalList co' is

scanC :: (a -> b -> a) -> a -> Coroutine b a
scanC f i = Coroutine $ step i
  where step a b = let a' = f a b in (a', scanC f a')

zipWithC :: (a -> b -> c) -> Coroutine (a, b) c
zipWithC = arr . uncurry
