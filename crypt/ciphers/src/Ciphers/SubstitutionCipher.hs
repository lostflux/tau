{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# HLINT ignore "[]" #-}

module Ciphers.SubstitutionCipher (
    encrypt
  , decrypt
  , frequencies
  , highest
  , standardFrequencies
  , bruteforce
  , Pairing
  , Freq
  , unset
  , set
  , rotate
  , problem
) where

import Ciphers.Common        (dropLines)
import Data.Foldable (for_)
import Data.List     (group, sort, sortBy)
import Data.Maybe    (fromMaybe)
import GHC.IO.Handle (hFlush)
import Prelude
import System.IO     (stdout)

-- | Record of an item and a recorded frequency.
data Freq a = Freq { item :: a, freq :: Double }

type Pairing = (Char, Char)

instance Eq a => Eq (Freq a) where
  (==) (Freq a1 c1) (Freq a2 c2) = a1 == a2 && c1 == c2

instance Eq a => Ord (Freq a) where
  compare a b = compare (freq a) (freq b)

instance Show a => Show (Freq a) where
  show (Freq a c) = show a ++ ": " ++ show c ++ "\n"

frequencies :: (Eq a, Ord a) => [a] -> [Freq a]
frequencies arr = sortBy (flip compare) $ freqs (length arr) (grouped arr)
  where
    freqs :: Int -> [[a]] -> [Freq a]
    freqs total = map (\arr@(x:_) -> Freq { item = x, freq = fromIntegral (100 * length arr) / fromIntegral total })

    grouped :: (Eq a, Ord a) => [a] -> [[a]]
    grouped = group . sort

get :: (Eq a, Ord a) => Int -> [Freq a] -> (a, [Freq a])
get 0 (x:xs) = (item x, xs)
get _ [] = error "get: index out of bounds"
get n (x:xs) = do
  let (x', xs') = get (n-1) xs
  (x', x : xs')


highest :: Ord a => [Freq a] -> (a, [Freq a])
highest = get 0
-- highest [] = (undefined, [])
-- highest (x:xs) = (item x, xs)

encrypt :: String -> IO String
encrypt = undefined
decrypt :: String -> IO String
decrypt plaintext = do
  let cleaned = dropLines plaintext
  let freqs = frequencies cleaned          :: [Freq Char]
  let standard = standardFrequencies       :: [Freq Char]
  let pairings = match standard freqs 2    :: [[Pairing]]
  print $ "pairings: " ++ show (length pairings)
  for_ pairings $ \x -> do
    putStrLn $ "\n\n" ++ substitute x cleaned ++ "\n\n"
    -- _ <- getLine
    putStr "..."
    -- for_ x $ \(a, b) -> do
    --   putStrLn $ show a ++ " -> " ++ show b
    -- putStrLn "\n\n"

  let best = head pairings
  putStrLn $ substitute best cleaned

  -- let best' = head (match standard (rotate 1 freqs) 2)
  -- putStrLn $ substitute best' cleaned

  -- let best'' = head (match standard (rotate 2 freqs) 2)
  -- putStrLn $ substitute best'' cleaned

  -- let best''' = head (match standard (rotate 3 freqs) 2)
  -- putStrLn $ substitute best''' cleaned



  return "..."  -- Default return when we don't really care.

bruteforce :: String -> IO String
-- decrypt = undefined
bruteforce plaintext = do
  let cleaned = dropLines plaintext
  let freqs = frequencies cleaned          :: [Freq Char]
  let standard = standardFrequencies       :: [Freq Char]
  let pairings = match standard freqs 2    :: [[Pairing]]
  print $ "pairings: " ++ show (length pairings)
  iter (head pairings) cleaned
    where
      iter :: [Pairing] -> String -> IO String
      iter [] _ = return "..."
      iter _ [] = return "..."
      iter pairing text = do
        putStrLn $ "\n\n" ++ substitute pairing text ++ "\n\n"
        putStr "Type \"a  b\" to create a binding:" >> hFlush stdout
        -- input <- getLine
        a <- getChar
        b <- getChar
        putStrLn $ "a: " ++ show a ++ " b: " ++ show b
        s <- swap pairing a b
        iter s text


swap :: [Pairing] -> Char -> Char -> IO [Pairing]
swap pairing a b = do
  let x' = fromMaybe '?' $ lookup' a pairing
  let y' = fromMaybe '?' $ lookup' b pairing
  putStrLn $ "x': " ++ show x' ++ " y': " ++ show y'
  let pairing' = iter pairing a x' b y'
  for_ [1..length pairing] $ \i -> do
    putStrLn $ "pairing[" ++ show i ++ "]: " ++ show (pairing !! i) ++ " " ++ show (pairing' !! i)
  return pairing'
    where
      iter :: [Pairing] -> Char -> Char -> Char -> Char -> [Pairing]
      iter [] _ _ _ _ = []
      iter (x:xs) a x' b y'
        | a == snd x = (y', a) : iter xs x' a y' b
        | b == snd x = (x', b) : iter xs x' a y' b
        | otherwise = x : iter xs x' a y' b



set, unset :: [Pairing] -> Char -> Char -> [Pairing]
set [] a b = [(a, b)]
set pairings a b
  | b /= b' = (a, b) : filter (\(x, _) -> x /= a) (unset pairings a' b')
  | otherwise = pairings
  where
    a' = fromMaybe '?' $ lookup' b pairings
    b' = fromMaybe '?' $ lookup a pairings

unset [] _ _ = []
unset (x@(a, b):xs) a' b'
  | b == b' = (a, b') : xs
  | otherwise = x : unset xs a' b'

rotate :: Int -> [a] -> [a]
rotate n arr = l ++ f
  where
    (f, l) = splitAt n arr

lookup' :: (Eq b) => b -> [(a,b)] -> Maybe a
lookup' _ [] =  Nothing
lookup' key ((x,y):xys)
  | key == y  =  Just x
  | otherwise =  lookup' key xys

substitute :: [Pairing] -> String -> String
substitute pairings = map (\c -> fromMaybe c (lookup c pairings))

match :: [Freq Char] -> [Freq Char] -> Int -> [[Pairing]]
match _ [] _ = []
match [] _ _ = []
match [x] [y] _ = [[(item x, item y)]]
match [x] (y:_) _ = [[(item x, item y)]]
match (x:_) [y] _ = [[(item x, item y)]]
match st freqs n = do
  let (letter, rest) = highest freqs                  :: (Char, [Freq Char])
  let (s', st') = get 0 st                            :: (Char, [Freq Char])
  let (s'', st'') = get 1 st                          :: (Char, [Freq Char])
  let match' = (letter, s')                           :: Pairing
  let match'' = (letter, s'')                         :: Pairing
  let first = map (match' :) (match st' rest n)       :: [[Pairing]]
  let second = map (match'' :) (match st'' rest n)    :: [[Pairing]]
  first ++ second
  -- second
--   where
--     iter :: Int -> [Freq Char] -> [Freq Char] -> [[Pairing]] -> [[Pairing]]
--     iter 0 _ _ acc = acc
--     iter n st@(s:ss) freqs acc =
--       iter (n-1) ss freqs (acc ++  st freqs)
  -- for [1..n] $ \i -> do
  --   let current = []
  --   let (s', ss') = get i st
  --   let (f', freqs') = highest freqs
  --   let current = current ++ [(s', f')]
  --   let next = match ss' freqs' (n-1) (acc ++ [new])
  --   map (++ current) next
  -- current


-- emptyFreq :: Freq a
-- emptyFreq = Freq { item = undefined, freq = 0 }

-- isEmpty :: Freq a -> Bool
-- isEmpty (Freq _ 0) = True
-- isEmpty _ = False

standardFrequencies :: [Freq Char]
standardFrequencies = sortBy (flip compare) [
    Freq 'a' 8.087,
    Freq 'b' 1.493,
    Freq 'c' 2.781,
    Freq 'd' 4.253,
    Freq 'e' 12.702,
    Freq 'f' 2.228,
    Freq 'g' 2.015,
    Freq 'h' 6.094,
    Freq 'i' 6.966,
    Freq 'j' 0.153,
    Freq 'k' 0.772,
    Freq 'l' 4.094,
    Freq 'm' 2.587,
    Freq 'n' 6.749,
    Freq 'o' 7.507,
    Freq 'p' 1.929,
    Freq 'q' 0.096,
    Freq 'r' 5.987,
    Freq 's' 6.234,
    Freq 't' 9.056,
    Freq 'u' 2.758,
    Freq 'v' 0.978,
    Freq 'w' 2.360,
    Freq 'x' 0.150,
    Freq 'y' 1.974,
    Freq 'z' 0.074
  ]

problem :: IO ()
problem = do
  print matchings
  let matchings'' = matchings
  plains matchings
  ciphers matchings
  for_ [2..26] $ \n -> do
    putStr $ "n: " ++ show n ++ " "
    putStrLn $ getKey matchings'' n

matchings :: [Pairing]
matchings = map getFirst . group . sortBy compareFirst $ zip plaintext ciphertext ++ [('z', '-'), ('x', '-'), ('q', '-'), ('k', '-'), ('j', '-')]
  where
    plaintext   = "itwasdisclosedyesterdaythatseveralinformalbutdirectcontactshavebeenmadewithpoliticalrepresentativesofthevietconginmoscow"
    ciphertext  = "UZQSOVUOHXMOPVGPOZPEVSGZWSZOPFPESXUDBMETSXAIZVUEPHZHMDZSHZOWSFPAPPDTSVPQUZWYMXUZUHSXEPYEPOPDZSZUFPOMBZWPFUPZHMDJUDTMOHMQ"
    compareFirst = \p1 p2 ->compare (fst p1) (fst p2)
    getFirst = \(x:_) -> x

plains :: [Pairing] -> IO ()
plains = foldr ((>>) . putChar . fst) (return ())

ciphers :: [Pairing] -> IO ()
ciphers = foldr ((>>) . putChar . snd) (return ())


getKey :: [Pairing] -> Int -> String
getKey matchings' n = iter matchings' (26 `div` n) 0 ((26 `mod` n) + 1) ""
  where
    iter :: [Pairing] -> Int -> Int -> Int -> String -> String
    iter [] _ _ _ acc = acc
    iter (x:xs) stepsize currentstep 0 acc
      | currentstep == 0 = iter xs stepsize 1 0 (acc ++ [snd x])
      | otherwise = iter xs stepsize ((currentstep + 1) `mod` stepsize) 0 acc
    iter (x:xs) stepsize currentstep overflow acc
      | currentstep == 0 = iter xs stepsize 1 (overflow - 1) (acc ++ [snd x])
      | otherwise = iter xs stepsize ((currentstep + 1) `mod` (stepsize + 1)) overflow acc

