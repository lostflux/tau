{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS  -fprint-potential-instances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import System.Random ( getStdRandom, randomR )
import Control.Monad ( when )
import System.Environment ( getArgs )
import GHC.IO.Handle.FD ( stdout )
import GHC.IO.Handle ( hFlush )
import Data.Char ( toLower )

import Prelude hiding (Word, words)


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
    True -> putStrLn "You win!"
    False -> putStrLn "You lose!"

  print $ fib' 10000
  print "--------------------------------"
  print $ fib 100


loop :: Word -> IO Bool
loop targetWord = do
  prompt
  getLine >>= \case
    "" -> putStrLn "Please guess something!"
      >> loop targetWord
    word -> if length word /= length targetWord
      then putStrLn ("Please guess a word of length " ++ show (length targetWord))
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
      highlight _  [] = []
      highlight (x:xs) (y:ys)
        | x == y = color y green ++ highlight xs ys
        | y `elem` xs = color y yellow ++ highlight xs ys
        | otherwise = color y red ++ highlight xs ys

colorAll :: Word -> Color -> Word
colorAll "" _ = ""
colorAll (x:xs) cc = color x cc ++ colorAll xs cc

color :: Char -> Color -> Word
color c cc = cc ++ [c] ++ reset

type Word       = String
type WordList   = [Word]
type Color      = String

blue, cyan, green, red, yellow, reset :: Color
reset     = "\x1b[0m"
red       = "\x1b[31m"
green     = "\x1b[92m"
yellow    = "\x1b[93m"
blue      = "\x1b[94m"
cyan      = "\x1b[96m"

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

filterWords :: Int -- ^ Word size.
  -> WordList -- ^ The list of words.
  -> WordList -- ^ Members of the provided word list that have the specified word length.
filterWords 0 _ = []
filterWords n ws = iter ws n []
  where
    iter :: WordList -> Int -> WordList -> WordList
    iter [] _ acc = acc
    iter (w:ws) n acc
      | length w == n = iter ws n $ acc ++ [w]
      | otherwise = iter ws n acc

------------------------------------------------------
----------------- RANDOM NUMBERS ---------------------
------------------------------------------------------

getNumInRange :: Int -> Int -> IO Int
getNumInRange lo hi = getStdRandom $ randomR (lo, hi)

getRandomWord :: WordList -> IO Word
getRandomWord wl = do
  let lo = 0              :: Int
  let hi = length wl - 1  :: Int
  index <- getNumInRange lo hi
  return (wl !! index)


fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Integer -> Integer
fib' n = iter (0, 1) n
  where
    iter :: (Integer, Integer) -> Integer -> Integer
    iter (a, b) 0 = a
    iter (a, b) n = iter (b, a + b) (n - 1)
