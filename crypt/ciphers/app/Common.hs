{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Common (
    clean
  , toInt
  , uppercase
  , lowercase
  , toUpper
  , toLower
  , chr
  , ord
) where

import Data.Char (toLower, toUpper, chr,  ord )

import Prelude

-- | Clean up the given string by removing all non-alphabetical characters
clean :: String -> String
clean = filter isalpha

-- | Check if char is puctuation
ispunct :: Char -> Bool
ispunct c = c `elem` "\n!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | Check if char is space
isspace :: Char -> Bool
isspace c = c `elem` " \t\r\f\v"

-- | Check if char is alphabeticall, i.e. neither punct nor space
isalpha :: Char -> Bool
isalpha c = not (isspace c || ispunct c)

lowercase, uppercase :: String -> String
-- | Convert string to lowercase
lowercase = map toLower

-- | convert string to uppercase
uppercase = map toUpper

toInt :: Char -> Int
toInt x = read [x] :: Int
