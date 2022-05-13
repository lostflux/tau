{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Monad      (void)
import Data.Char          (isAlpha, toLower)
import Data.Text          (Text, isInfixOf, isPrefixOf, pack, strip, unpack)
import GHC.IO.Handle      (hFlush)
import GHC.IO.Handle.FD   (stdout)
import System.Environment (getArgs)
import System.IO          (isEOF)

import Prelude


main :: IO ()
main =
  getArgs >>= \case
  []  -> return ()
  arr -> void $ process arr

process :: [String] -> IO ()
process [] = return ()
process (file:files) = do
  contents <- readFile file >>= logContents
  void $ processFile contents >> printDone file
  process files

processFile :: String -> IO String
processFile contents = do
  void $ putStr "word? " >> hFlush stdout
  isEOF >>= \case
    True -> return contents
    False -> do
      getLine >>= \case
        "" -> putStrLn "Type \"EOF\" to end stream"
          >> processFile contents
        "EOF" -> return contents
        word -> do
          printResults $ matchFile word contents
          processFile contents

matchFile :: String -> String -> String
matchFile word file =
  rebuild "" $ colorWord word $ (map (matchLine word) . lines) file

matchLine :: String -> String -> String
matchLine word line
  | w `isInfixOf` ws = line
  | otherwise = ""
    where
      w = strip $ pack (lowercase word)
      ws = pack $ lowercase line

rebuild :: String -> [String] -> String
rebuild str []      = str
rebuild str ("":xs) = rebuild str xs
rebuild str (x:xs)  = "\t" ++ x ++ "\n" ++ rebuild str xs

colorWord :: String -> [String] -> [String]
colorWord _ [] = []
colorWord target (sentence:sentences) =
  unwords (colorInSentence target (words sentence)) : colorWord target sentences
    where
      colorInSentence :: String -> [String] -> [String]
      colorInSentence _ [] = []
      colorInSentence target (word:words)
        | w `isInfixOf` ws = (blue ++ word ++ green) : colorInSentence target words
        | ws `isInfixOf` w =
          if fullSequence target (word:words)
            then (blue ++ target ++ green) : colorInSentence target (dropSequence target words)
            else word : colorInSentence target words
        | otherwise = word : colorInSentence target words
          where
            !w = pack $ lowercase target
            !ws = pack $ lowercase $ filter (/= ' ') word

fullSequence :: String -> [String] -> Bool
fullSequence [] _ = True
fullSequence [x] [] = not $ isAlpha x
fullSequence _ [] = False
fullSequence target (y:ys)
  | ws `isInfixOf` w = fullSequence (dropSpaces (drop (length y) target)) ys
  | otherwise = False
    where
      !w = strip $ pack $ lowercase $ filter (/= ' ') target
      !ws = pack $ lowercase y

dropSpaces :: String -> String
dropSpaces = unpack . strip . pack


-- dropPunctuation :: String -> String
-- dropPunctuation word = [ letter | letter <- word, letter `notElem` ",.?!-:;\"\'([{<)]}>" ]

dropSequence :: String -> [String] -> [String]
dropSequence [] words = words
dropSequence _ [] = []
dropSequence target (y:ys)
  | y' `isInfixOf` t  = dropSequence (dropSpaces (drop (length y + 1) target)) ys
  | otherwise = y:ys
    where
      t = strip (pack target)
      y' = strip (pack y)

isPostfixOf :: Text -> Text -> Bool
isPostfixOf a b = b `isPrefixOf` a
logContents :: String -> IO String
logContents contents = do
  putStrLn $ format "CONTENTS" contents
  return contents

printResults :: String -> IO ()
printResults results =
  putStrLn $ format "RESULTS" results

printDone :: String -> IO ()
printDone file =
  putStrLn $ format "DONE" $ "Finished Processing: " ++ file

format :: String -> String -> String
format mode text
  | mode == "CONTENTS" = blue ++ text ++ reset
  | mode == "RESULTS" = green ++ text ++ reset
  | mode == "DONE" = red ++ text ++ reset
  | otherwise = cyan ++ text ++ reset

-- firstWord :: String -> String -> Bool
-- firstWord word text =
--   case words text of
--     [] -> False
--     (x:_) -> x == word

type Color = String
blue, cyan, green, red, yellow, reset :: Color
blue      = "\x1b[94m"
cyan      = "\x1b[96m"
green     = "\x1b[92m"
red       = "\x1b[31m"
yellow    = "\x1b[93m"
reset     = "\x1b[0m"

lowercase :: String -> String
lowercase = map toLower
