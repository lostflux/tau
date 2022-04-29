module Arrows where
import Control.Arrow (Arrow(arr, (&&&)), (>>>))
import Data.Bifunctor(bimap)

addA :: (Arrow cat, Num c) => (b -> c) -> (b -> c) -> cat b c
addA f g = 
  arr (\x -> (x, x)) >>>
    arr (bimap f g) >>>
      arr (uncurry (+))

addB :: Num a => (p -> a) -> (p -> a) -> p -> a
addB f g x = do
  let a = f x
  let b = g x
  a + b

addC :: Num c => (a -> c) -> (a -> c) -> a -> c
addC f g = f &&& g >>> uncurry (+)

sq :: Num a => a -> a
sq x = x * x
incr :: Num a => a -> a
incr x = x + 1

other :: Arrow a => a b c -> a b c' -> a b (c, c')
other f g = f &&& g
