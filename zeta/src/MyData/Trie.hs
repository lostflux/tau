{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MyData.Trie (
    Trie(..)
  , isLeaf
  , isRoot
  , insert
  , makeTrie
  , makeRootTrie
  , find
  , printTrie
  , t1, t2, t3, t4, t5, t6, t7
) where
import Data.Foldable (for_)
import Control.Monad (when)

-- class Trie a where
--   value :: Maybe a
--   children :: a -> [a]
--   isLeaf :: a -> Bool
--   isLeaf = null children

data Trie = EmptyTrie | Trie {
  value :: Char,
  children :: [Trie],
  isWord :: Bool
}

instance Show Trie where
  show EmptyTrie = "EmptyTrie"
  show (Trie v cs _) = "(" ++ show v ++ " " ++ concatMap show cs ++ ")"

instance Eq Trie where
  (==) EmptyTrie EmptyTrie = True
  (==) EmptyTrie _ = False
  (==) _ EmptyTrie = False
  (==) (Trie v1 cs1 w1) (Trie v2 cs2 w2) = v1 == v2 && cs1 == cs2 && w1 == w2

instance Ord Trie where
  compare EmptyTrie EmptyTrie = EQ
  compare EmptyTrie _ = LT
  compare _ EmptyTrie = GT
  compare (Trie v1 cs1 w1) (Trie v2 cs2 w2) = case compare v1 v2 of
    EQ -> compare cs1 cs2
    x -> x

-- | Check if a Trie is a leaf node.
isLeaf :: Trie -> Bool
isLeaf EmptyTrie = True
isLeaf (Trie _ [] _) = True
isLeaf _ = False

-- | Check if a Trie is a root node.
isRoot :: Trie -> Bool
isRoot EmptyTrie = False
isRoot (Trie '\0' _ _) = True
isRoot _ = False

rootTrie :: Trie
rootTrie = Trie '\0' [] False

insert :: String -> Trie -> Trie
insert str trie
  | null str = trie
  | isRoot trie = Trie (value trie) (iter (children trie) str) (isWord trie)
  | value trie /= head str = error "Incompatible word."
  | otherwise = Trie (value trie) (iter (children trie) (tail str)) (isWord trie)
  where
    iter :: [Trie] -> String -> [Trie]
    iter [] str = [makeTrie str]
    iter [t] str
      | value t == head str = [insert str t]
      | value t < head str = [t, makeTrie str]
      | otherwise = [makeTrie str, t]
    iter tries@(t1:t2:ts) chars@(c:cs)
      | value t1 < c && value t2 > c = t1 : makeTrie str : (t2:ts)
      | value t1 == c = insert chars t1 : (t2:ts)
      | value t2 == c = t1 : insert chars t2 : ts
      | otherwise = [t1,t2] ++ iter ts chars

makeRootTrie :: String -> Trie
makeRootTrie str = insert str rootTrie

makeTrie :: String -> Trie
makeTrie []         = EmptyTrie
makeTrie (x:y:xs)   = Trie x [makeTrie (y:xs)] False
makeTrie [x]        = Trie x [] True

find :: Char -> [Trie] -> Trie
find c [] = EmptyTrie
find c (t:ts) = if value t == c then t else find c ts

printTrie :: Trie -> IO ()
printTrie EmptyTrie = return ()
printTrie t
  | isRoot t = do
    for_ (children t) $ \c -> do
      printIter c ""
  | otherwise = do
    for_ (children t) (`printIter` [value t])
  where
    printIter :: Trie -> String -> IO ()
    printIter EmptyTrie _ = return ()
    printIter t str = do
      when (isWord t) $ putStrLn $ str ++ [value t]
      for_ (children t) (`printIter` (str ++ [value t]))


-- simple tests

t1, t2, t3, t4, t5, t6, t7 :: Trie
t1 = makeRootTrie "abc"
t2 = insert "abd" t1
t3 = insert "xyz" t2
t4 = insert "abcde" t3
t5 = insert "amittai" t4
t6 = insert "joel" t5
t7 = insert "siavava" t6
