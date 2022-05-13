{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Ciphers.RSA where

import Ciphers.AbstractAlgebra (addN, divN, extendedEuclidean, invN, mulN, subN)
import Control.Monad           (join)
import Data.Bifunctor          (bimap)
import Prelude                 hiding ()

encrypt :: (Integral a, Integral b) => a -> b -> a -> a
encrypt n e message = mod (message ^ e) n

e :: Integer -> Integer
e = encrypt 2038667 203

root :: (Ord b, Floating b) => b -> b -> b -> (b, b)
root a b c =
  if d < 0 then error "0" else (x, y)
    where
      x = e + sqrt d / (2 * a)
      y = e - sqrt d / (2 * a)
      d = b * b - 4 * a * c
      e = - b / (2 * a)


x :: Double
x = 172205490419

tupleu :: (Double, Double)
tupleu = root 1 (-830076) 172205490419

test :: Double -> (Double, Double)
test x = bimap (x /) (x /) tupleu

testX :: (Double, Double)
testX = test x
