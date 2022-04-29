module MyData.Trie (
    Trie(..)
  , isLeaf
  , isRoot
  , makeTrie
  , makeRootTrie
  , insert
  , find
  , toString
  , printTrie
  , repr
  , clean
  -- tests
  , t1, t2, t3, t4, t5, t6, t7, t8, t9, t10,
) where
import Control.Monad (when)
import Data.Char     (isDigit, toLower, toUpper)
import Data.Foldable (for_)
import Text.Printf   (printf)

-- class Trie a where
--   value :: Maybe a
--   children :: a -> [a]
--   isLeaf :: a -> Bool
--   isLeaf = null children


-- | A prefix tree.
data Trie =
  EmptyTrie -- ^ Empty trie.
  | Trie    -- ^ Non-empty trie.
    {
        value    :: Char    -- ^ The value of the node.
      , children :: [Trie]  -- ^ The children of the node.
      , isWord   :: Bool    -- ^ Whether the node is a terminal word.
    }

-- instance Monad Trie where


instance Show Trie where
  -- | Neat show
  show = toString

-- | Dirty show
--
-- Prints syntax around Trie.
--
-- Might be easier to read back from a text file.
repr :: Trie -> String
repr EmptyTrie     = "EmptyTrie"
repr (Trie v cs _) = "(" ++ show v ++ " " ++ concatMap repr cs ++ ")"

-- | Get the size of a Trie.
--
-- @O(n)@
size :: Trie -> Int
size EmptyTrie     = 0
size (Trie _ cs _) = foldr (\c acc -> size c + acc) 1 cs

instance Eq Trie where
  (==) EmptyTrie EmptyTrie               = True
  (==) EmptyTrie _                       = False
  (==) _ EmptyTrie                       = False
  (==) (Trie v1 cs1 w1) (Trie v2 cs2 w2) = v1 == v2 && cs1 == cs2 && w1 == w2

instance Ord Trie where
  compare EmptyTrie EmptyTrie = EQ
  compare EmptyTrie _ = LT
  compare _ EmptyTrie = GT
  compare (Trie v1 cs1 w1) (Trie v2 cs2 w2) = case compare v1 v2 of
    EQ -> compare cs1 cs2
    x  -> x

-- | Check if a Trie is a leaf node.
--
-- We define a root node as a non-empty node without children.
isLeaf :: Trie -> Bool
isLeaf EmptyTrie     = True
isLeaf (Trie _ [] _) = True
isLeaf _             = False

-- | Check if a Trie is a root node.
isRoot :: Trie -> Bool
isRoot EmptyTrie       = False
isRoot (Trie '\0' _ _) = True
isRoot _               = False

-- | The default root node.
--
-- Contains the NULL char `\\0` as its value.
rootTrie :: Trie
rootTrie = Trie '\0' [] False

-- | Insert a String into a Trie.
--
-- Returns a new Trie with the modification --
-- perhaps manage that new instance.
insert :: String -> Trie -> Trie
insert str EmptyTrie = makeRootTrie str
insert str trie
  | null str = trie
  | isRoot trie = Trie (value trie) (iter (children trie) str) (isWord trie)
  | value trie /= head str = error "Incompatible word."
  | otherwise = Trie (value trie) (iter (children trie) (tail str)) (isWord trie)
  where
    iter :: [Trie] -> String -> [Trie]
    iter tries [] = tries
    iter [] str = [makeTrie str]
    iter [t] str
      | value t == head str = [insert str t]
      | value t < head str = [t, makeTrie str]
      | otherwise = [makeTrie str, t]
    iter tries@(t1:t2:ts) chars@(c:cs)
      | value t1 > c = makeTrie chars : tries
      | value t1 == c = insert chars t1 : (t2:ts)
      | value t2 == c = t1 : insert chars t2 : ts
      | value t1 < c && value t2 > c = t1 : makeTrie chars : (t2:ts)
      | otherwise = t1 : iter (t2:ts) chars

-- | Construct a rooted Trie from a String.
makeRootTrie :: String -> Trie
makeRootTrie str = insert str rootTrie

-- | Construct a non-rooted Trie from a String.
makeTrie :: String -> Trie
makeTrie []       = EmptyTrie
makeTrie (x:y:xs) = Trie x [makeTrie (y:xs)] False
makeTrie [x]      = Trie x [] True

-- | Find a `Trie` with the given `Char` from a list of `Trie`s.
--
-- NOTE: This function is not tail-recursive.
--
-- We also don't care about the children of the `Trie`.
find :: Char -> [Trie] -> Trie
find c []     = EmptyTrie
find c (t:ts) = if value t == c then t else find c ts

-- | Print all the proper words in a `Trie`.
--
-- We do a depth-first traversal.
--
-- This function generates a sorted arrangement of the words in the Trie.
printTrie :: Trie -> IO ()
printTrie trie = putStr $ toString trie

-- | OLD: Print all the proper words in a `Trie`.
trieToStr' :: Trie -> String
trieToStr' EmptyTrie = ""
trieToStr' t
  | isRoot t = concatMap (`stringify` "") (children t)
  | otherwise = concatMap (`stringify` [value t]) (children t)
    where
      stringify :: Trie -> String -> String
      stringify EmptyTrie str = str
      stringify t str
        | isWord t = (str ++ [value t] ++ "\n") ++ concatMap (`stringify` (str ++ [value t])) (children t)
        | otherwise = concatMap (`stringify` (str ++ [value t])) (children t)

-- | NEW (Using printf)
toString :: Trie -> String
toString EmptyTrie = ""
toString t
  | isRoot t = concatMap (`stringify` "") (children t)
  | otherwise = concatMap (`stringify` [value t]) (children t)
    where
      stringify :: Trie -> String -> String
      stringify EmptyTrie str = str
      stringify t str
        | isWord t = printf "%s%c\n%s" str (value t) $ concatMap (`stringify` printf "%s%c" str (value t)) (children t)
        | otherwise = concatMap (`stringify` printf "%s%c" str (value t)) (children t)

loadFile :: FilePath -> IO Trie
loadFile fp = do
  text <- readFile fp
  return $ foldr (insert . clean) EmptyTrie (words text)

dumpToFile :: Trie -> FilePath -> IO ()
dumpToFile t fp = writeFile fp $ show t


---- TEXT OPS -----
-- | Clean up the given string by removing all non-alphabetical characters
clean :: String -> String
clean = lowercase . filter isalpha

-- | Check if char is puctuation
ispunct :: Char -> Bool
ispunct c = c `elem` "\n!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~‘“’”—…"

-- | Check if char is space
isspace :: Char -> Bool
isspace c = c `elem` " \t\r\f\v"

-- | Check if char is alphabetical, i.e. neither punct nor space
isalpha :: Char -> Bool
isalpha c = not $ isspace c || ispunct c || isDigit c

lowercase, uppercase :: String -> String
-- | Convert string to lowercase
lowercase = map toLower

dropLines :: String -> String
dropLines = filter (/= '\n')

-- | convert string to uppercase
uppercase = map toUpper

-- simple tests

writeToFile :: FilePath -> Trie -> IO ()
writeToFile fp trie = writeFile fp $ toString trie

t1, t2, t3, t4, t5, t6, t7, t8, t9, t10 :: Trie
t1 = makeRootTrie "abc"
t2 = insert "abd" t1
t3 = insert "xyz" t2
t4 = insert "abcde" t3
t5 = insert "amittai" t4
t6 = insert "joel" t5
t7 = insert "siavava" t6
t8 = insert "what" t7
t9 = insert "is" $ insert "this" $ insert "haha" $ insert "okay" $ insert "abcdef" t8
t10 = insert "also" t9

t11 :: IO Trie
t11 = loadFile "../../data/simpletext"

t12 :: IO Trie
t12 = loadFile "../../data/en"

