{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-}

module Algebra.Group (
  Group(..)
) where

import Prelude hiding (minBound, maxBound)
import Data.Set as Set (Set, singleton, fromList)


class Group a where
  groupOp :: a -> a -> a
  groupInv :: a -> a
  groupSet :: Set a

instance Group () where
  groupOp () () = ()
  groupInv () = ()
  groupSet = Set.singleton ()

instance Group Bool where
  groupOp a b =
    if a
      then not b
    else b

  groupInv = not
  groupSet = Set.fromList [False, True]

newtype IntegerGroup = IntegerGroup {n :: Integer}

minBound, maxBound :: IntegerGroup -> Integer
minBound _ = 0
maxBound (IntegerGroup n) = n-1


instance Group IntegerGroup where
  groupOp a b = IntegerGroup $ (n a + n b) `mod` n b
  groupInv (IntegerGroup a) = IntegerGroup (negate a)
  -- groupSet = Set.fromList [0.. n-1]
    where
      n = maxBound