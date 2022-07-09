{-# LANGUAGE NoImplicitPrelude   #-}



module Algebra.Group (
  Group(..)
) where

import Prelude hiding ()

data Group a = Group {
    base :: a -- ^ The base of the group.
  , val  :: a  -- ^ The internal value.
} deriving (Read, Show, Eq)

instance (Read a, Integral a) => Num (Group a) where
  (+) = groupAdd
  (-) = groupSub
  (*) = groupMul
  abs = reduce
  signum = const 1
  fromInteger n = Group undefined (read x)
    where x = show n

instance (Integral a, Ord a) => Ord (Group a) where
  compare g1 g2 = do
    let g1' = reduce g1
    let g2' = reduce g2
    compare (val g1') (val g2')

instance Functor Group where
  fmap f (Group b v) = Group b $ f v

instance Applicative Group where
  pure = Group 1
  (<*>) (Group b f) = Group b $ f (val g)
    where g = Group b (val g)


instance Monad Group where
  return = pure
  m >>= f = f $ val m

reduce :: Integral a => Group a -> Group a
reduce (Group b v) = Group b $ v `mod` b

groupAdd :: Num a => Group a -> Group a -> Group a
groupAdd g1 g2
  | base g1 /= base g2 = error "Incompatible Groups."
  | otherwise = do
    let x = val g1
    let y = val g2
    let sum = x + y
    reduce $ Group (base g1) sum

groupSub :: Group a -> Group a -> Group a
groupSub g1 g2
  | base g1 /= base g2 = error "Incompatible Groups."
  | otherwise = do
    let b = base g1
    let x = val g1
    let y = val g2
    let diff = x - y
    reduce $ Group b diff


groupMul :: Group a -> Group a -> Group a
groupMul g1 g2
  | base g1 /= base g2 = error "Incompatible Groups."
  | otherwise = do
    let b = base g1
    let x = val g1
    let y = val g2
    let prod = x - y
    return prod
