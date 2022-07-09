{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS  -fprint-potential-instances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import           Control.Monad          (when)
import           Data.Char              (toLower)
import           GHC.IO.Handle          (hFlush)
import           GHC.IO.Handle.FD       (stdout)
import           Prelude                hiding (Word, words)
import           System.Environment     (getArgs)
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Random.Stateful (Random, getStdRandom, randomR)

main :: IO ()
main = do
  args <- getArgs
  let len = if null args then 5 else read $ head args
  allWords <- loadWords "data/en2"
  let currentSet = filterWords len allWords
  targetWord <- getRandomWord currentSet
  when (length args > 1 && lowercase (args !! 1) == "h") $ do
    putStrLn $ colorAll ("Word count: " ++ show (length allWords)) red
    putStrLn $ colorAll ("Target word: " ++ targetWord) red
  loop targetWord >>= \case
    True  -> putStrLn "You win!"
    False -> putStrLn "You lose!"

loop :: Word -> IO Bool
loop targetWord = do
  prompt
  getLine >>= \case
    "" ->
      putStrLn "Please guess something!"
        >> loop targetWord
    word ->
      if length word /= length targetWord
        then
          putStrLn ("Please guess a word of length " ++ show (length targetWord))
            >> loop targetWord
        else
          putStrLn (check targetWord word)
            >> if lowercase word == lowercase targetWord then return True else loop targetWord

prompt :: IO ()
prompt = putStr (colorAll "guess? " blue) >> hFlush stdout

check :: Word -> Word -> Word
check "" _ = ""
check word guess
  | lowercase word == lowercase guess = colorAll word green
  | otherwise = highlight word guess
  where
    highlight [] arr = arr
    highlight _ [] = []
    highlight (x : xs) (y : ys)
      | x == y = color y green ++ highlight xs ys
      | y `elem` xs = color y yellow ++ highlight xs ys
      | otherwise = color y red ++ highlight xs ys

colorAll :: Word -> Color -> Word
colorAll "" _        = ""
colorAll (x : xs) cc = color x cc ++ colorAll xs cc

color :: Char -> Color -> Word
color c cc = cc ++ [c] ++ reset

type Word = String

type WordList = [Word]

type Color = String

blue, cyan, green, red, yellow, reset :: Color
reset = "\x1b[0m"
red = "\x1b[31m"
green = "\x1b[92m"
yellow = "\x1b[93m"
blue = "\x1b[94m"
cyan = "\x1b[96m"

words :: WordList
words = ["a", "ab", "abc", "abcd", "abcde"]

lowercase :: Word -> Word
lowercase = map toLower

contained :: Char -> Word -> Bool
contained = elem

loadWords :: FilePath -> IO WordList
loadWords fp = do
  content <- readFile fp
  return $ lines content

filterWords ::
  -- | Word size.
  Int ->
  -- | The list of words.
  WordList ->
  -- | Members of the provided word list that have the specified word length.
  WordList
filterWords 0 _ = []
filterWords n ws = iter ws n []
  where
    iter :: WordList -> Int -> WordList -> WordList
    iter [] _ acc = acc
    iter (w : ws) n acc
      | length w == n = iter ws n $ acc ++ [w]
      | otherwise = iter ws n acc

------------------------------------------------------
----------------- RANDOM NUMBERS ---------------------
------------------------------------------------------

-- Generate a random number in range (low, high)
random :: (Integral a, Random a) => a -> a -> a
random low high = unsafePerformIO $ do
  let gen = randomR (fromIntegral low, fromIntegral high)
  getStdRandom gen

-- getStdRandom $ randomR (fromIntegral low, fromIntegral high)

getNumInRange :: Int -> Int -> Int
getNumInRange lo hi = unsafePerformIO $ do
  getStdRandom $ randomR (lo, hi)

-- >>> random 1 10
-- 6

getRandomWord :: WordList -> IO Word
getRandomWord wl = do
  let lo = 0 :: Int
  let hi = length wl - 1 :: Int
  let index = getNumInRange lo hi
  return (wl !! index)

test :: IO ()
test = print ("test" == "test")
