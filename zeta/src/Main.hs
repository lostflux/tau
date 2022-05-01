{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (when)
import Data.List     (isPrefixOf)
import Data.Set      (Set, (\\))
import Data.Set      qualified as Set
import MyData.Parser (Link, Links, WebPage (..), isValid)
import MyData.Parser qualified as Parser
import MyData.Trie   (Trie (..), (<|>))
import MyData.Trie   qualified as Trie
import System.Exit   (exitSuccess)
import Text.Printf   (printf)


-- | Set to true to log debug info.
debug :: Bool
debug = True

limit :: Int
limit = 1000

main :: IO ()
main = do
  putStrLn "\nStarting..."
  crawl
  putStrLn "\nDone.\n"


seedURLs :: [Link]
seedURLs = [
  -- "https://github.com/siavava"
    "https://www.deepmind.com"
  , "https://www.technologyreview.com"
  , "https://singularityhub.com"
  , "https://www.wired.com"
    -- "https://singularityhub.com/2022/04/20/gm-just-patented-a-self-driving-car-that-teaches-people-to-drive"
  ]

keyWords :: [(String, String)]
keyWords = [
      ("machine","learning")
    , ("deep","learning")
    , ("artificial","intelligence")
    , ("neural","network")
    , ("thinking","machine")
    , ("reinforcement", "learning")
    , ("ai", "")
  ]

crawl :: IO ()
crawl = do
  let docID = 0
  let allWords = EmptyTrie
  let seenURLs = Set.empty
  iter seedURLs seenURLs docID allWords

iter :: [Link] -> Links -> Int -> Trie -> IO ()
iter queue seenURLs docID allWords = do
  when (null queue || docID >= limit) $ do
    let file = "data/log/.all"
    writeFile file $ show allWords

    printf "Seen %d unique URLs.\n" (Set.size seenURLs + length queue)
    printf "THE END"
    exitSuccess

  let (url, rest) = (head &&& tail) queue
  -- if Set.member url seenURLs then do
  --   printf "%sIgnDupl:  %s%s\n" yellow url reset
  --   iter rest seenURLs docID allWords
  -- else do
  printf "%sFetch:    %s%s\n" green url reset
  page <- Parser.loadPage url
  if isValid page then do
    let !words = allWords <|> text page
    let !asList = Set.toList (links page \\ seenURLs)
    let !s = foldr Set.insert seenURLs asList
    let !q = rest ++ asList
    -- when debug $ printf "\n\nqueue length : %d\nseen urls: %d\n\n" (length q) (Set.size seenURLs)
    if hasKeyWords page then do
      printf "%sHit  %3d: %s%s\n" blue docID url reset
      logR docID url page
      iter q s (docID + 1) words
    else iter q s docID words
  else do
    printf "%sIgnBad:   %s%s\n" red url reset
    let filtered = filter (not . isPrefixOf url) rest
    iter filtered seenURLs docID allWords

-- | Does the page have any of the specified set of keywords?
hasKeyWords :: WebPage -> Bool
hasKeyWords page =
  check keyWords $ text page
    where
      check :: [(String, String)] -> Trie -> Bool
      check [] _        = False
      check _ EmptyTrie = False
      check (pair@(first, second) : others) trie =
        (Trie.lookup first trie
          && Trie.lookup second trie)
            || check others trie


logR :: Int -> Link -> WebPage -> IO ()
logR docID url page = do
  let file = printf "data/log/%d" docID
  writeFile file $ printf "%s\n%s\n" url $ show (text page)

--- colors
type Color = String
blue, cyan, green, red, yellow, reset :: Color
blue      = "\x1b[94m"
cyan      = "\x1b[96m"
green     = "\x1b[92m"
red       = "\x1b[31m"
yellow    = "\x1b[93m"
reset     = "\x1b[0m"

