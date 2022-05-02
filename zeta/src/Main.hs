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
limit = 2000

main :: IO ()
main = do
  putStrLn "\nStarting..."
  crawl
  putStrLn "\nDone.\n"


seedURLs :: [Link]
seedURLs = [
  -- "https://github.com/siavava"
    "https://www.technologyreview.com"
  , "https://www.deepmind.com"
  , "https://singularityhub.com"
  , "https://www.wired.com"
  , "https://www.vox.com"
  , "https://www.theverge.com"
  , "https://www.theguardian.com"
  , "https://www.theatlantic.com"
  , "https://www.washingtonpost.com"
  , "https://www.techcrunch.com"
    -- "https://singularityhub.com/2022/04/20/gm-just-patented-a-self-driving-car-that-teaches-people-to-drive"
  ]

-- | The number of matches we need to approve a page.
matchCount :: Int
matchCount = 2

-- | Keywords to search for.
--
-- Add keywords here.
targets :: [String]
targets = [
      "machine"
    , "machines"
    , "learning"
    , "deep"
    , "artificial"
    , "intelligence"
    , "neural"
    , "network"
    , "thinking"
    , "reinforcement"
    , "ai"
    , "recommender"
    , "system"
    , "systems"
    , "mind"
    , "language"
    , "processing"
    , "vision"
    , "ai"
  ]

crawl :: IO ()
crawl = do
  let docID = 0
  let allWords = EmptyTrie
  let seenURLs = Set.empty
  let allLinks = []
  iter seedURLs seenURLs docID allWords allLinks

iter :: [Link] -> Links -> Int -> Trie -> [String] -> IO ()
iter queue seenURLs docID allWords allLinks = do
  when (null queue || docID >= limit) $ do
    let dir = "data/log/"
    writeFile (printf "%s/metadata/all" dir) $ show allWords
    writeFile (printf "%s/metadata/urls" dir) $ unlines allLinks

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
    -- when debug $ printf "\n\t\tqueue length : %d\n\t\tseen urls: %d\n\n" (length q) (Set.size seenURLs)
    if hasKeyWords page then do
      printf "%sHit  %3d: %s%s\n" blue docID url reset
      logR docID url page
      iter q s (docID + 1) words (allLinks ++ [url])
    else iter q s docID words allLinks
  else do
    printf "%sIgnBad:   %s%s\n" red url reset
    let filtered = filter (not . isPrefixOf url) rest
    iter filtered seenURLs docID allWords allLinks

advance :: [Link] -> Links -> Int -> Trie -> [String] -> IO ()
advance q s docID words allLinks
  | length q > limit = iter (drop (length q - (limit `div` 2)) q) s docID words allLinks
  | otherwise = iter q s docID words allLinks

-- | Does the page have any of the specified set of keywords?
hasKeyWords :: WebPage -> Bool
hasKeyWords page =
  check targets $ text page
    where
      check :: [String] -> Trie -> Bool
      check [] _        = True
      check _ EmptyTrie = False
      check words trie = count >= matchCount
        where
          count = foldr (\x acc -> if Trie.lookup x trie then acc + 1 else acc) 0 words


logR :: Int -> Link -> WebPage -> IO ()
logR docID url page = do
  let file = printf "data/log/%d" docID
  writeFile file $ printf "%s\n%s\n%s\n\n%s\n" ttl yr url $ show (text page)
    where
      ttl = title page
      yr  = year page

--- colors
type Color = String
blue, cyan, green, red, yellow, reset :: Color
blue      = "\x1b[94m"
cyan      = "\x1b[96m"
green     = "\x1b[92m"
red       = "\x1b[31m"
yellow    = "\x1b[93m"
reset     = "\x1b[0m"

