module MyData (

  -- * Data types
    Trie(..)
  , isLeaf
  , isRoot
  , insert
  , makeTrie
  , makeRootTrie
  , find
  , printTrie
  , t1, t2, t3, t4, t5, t6, t7

  -- * Re-exported from Data.Trie
  , parse, tP1,
) where

import MyData.Trie
import MyData.Parser

r :: String
r = "root"
