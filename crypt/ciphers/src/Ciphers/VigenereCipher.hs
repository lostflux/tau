{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ciphers.VigenereCipher (
    encrypt
  , decrypt
  , decryptShift
  , equalities
  , process
  , cp
) where

import Ciphers.Common      (clean, invWord, lowercase, splitT, toInt, uppercase)
import Ciphers.ShiftCipher (shiftChar)
import Ciphers.SubstitutionCipher (frequencies, standardFrequencies, Freq(..))
import Data.Foldable       (maximumBy, for_)
import Data.List           (elemIndices, groupBy, sort)
import Prelude             hiding (repeat)

-- | Encrypts a plaintext using a key, per the vigenère cipher.
-- encrypt = undefined
encrypt :: String -> String -> String
encrypt text keyword = zipWith shiftChar cleanedText repeatedKeyword
  where
    cleanedText = clean text
    repeatedKeyword = take (length cleanedText) (repeat keyword)

decrypt :: String -> String -> String
decrypt text keyword = encrypt text $ invWord keyword

-- | Decrypts a ciphertext using a key, per the vigenère cipher
decryptShift :: Integer -> String -> IO String
decryptShift _ [] = return []
decryptShift 0 str = return str
decryptShift n str = do
  let matrix = splitT 8 $ (uppercase . clean) str
  putStrLn "Matrix: "
  printMatrix matrix
  lowercase <$> build (show n) matrix []
  where
    build :: String -> [String] -> String -> IO String
    build _ [] result = return result
    build [] _ result = return result
    build (k : ks) matrix result =
      let key = toInt k
          line = matrix !! (key - 1)
          letter = line !! length result
       in build ks matrix $ result ++ [letter]

printMatrix :: [String] -> IO ()
printMatrix [] = return ()
printMatrix (x : xs) = do
  putStrLn $ "  " ++ x
  printMatrix xs

-- | Repeats a string, infinitely many times.
--
-- Since Haskell is a lazy language, we can easily
-- get the first $n$ elements of an infinite sequence.
repeat :: [a] -> [a]
repeat str = str ++ repeat str

equalities :: String -> String -> Int
equalities [] _ = 0
equalities _ [] = 0
equalities (x : xs) (y : ys)
  | x == y = 1 + equalities xs ys
  | otherwise = equalities xs ys

coincidences :: String -> [(Int, Int)]
coincidences str = iter 0 ((lowercase . clean) str) []
  where
    iter :: Int -> String -> [(Int, Int)] -> [(Int, Int)]
    iter _ [] result = result
    iter 25 _ result = result
    iter n arr result = iter (n+1) arr $ result ++ [(n, counts)]
      where
        counts = equalities arr $ drop n arr

getStr :: IO String
getStr = lowercase . clean <$> readFile "../data/vigenere01"

process :: IO ()
process = do
  let str = cp
  -- let str = "hellohellwhellp"
  print $ coincidences str
  print $ ngrams 3 str
  putStrLn $ tabulate $ recurrence (ngrams 4 str)
  return ()

tabulate :: [(String, [Int])] -> String
tabulate [] = []
tabulate (val:vals) =
  let (x, y) = val
      xs = tabulate vals
   in x ++ ": " ++ show y ++ "\n" ++ xs


ngrams :: Int -> String -> [String]
ngrams 0 _          = []
ngrams _ []         = []
ngrams n str@(_:cs) = filter (\x -> length x == n) $ take n str : ngrams n cs

rmdups :: [(String, [Int])] -> [(String, [Int])]
rmdups =
  map (maximumBy (\one two -> length (snd one) `compare` length (snd two)))
  . groupBy (\one two -> fst one == fst two) . sort

recurrence :: [String] -> [(String, [Int])]
recurrence [] = []
recurrence strings = rmdups $ iter strings 0 []
  where
    iter :: [String] -> Int -> [(String, [Int])] -> [(String, [Int])]
    iter [] _ results = results
    iter (str:strs) n results
      | str `elem` strs = iter strs (n+1) $ results ++ [(str, n : map (+ (n + 1)) allIndices)]
      | otherwise = iter strs (n+1) results
        where
          allIndices = elemIndices str strs

-- run :: Int -> IO ()
-- run n = putStrLn . (tabulate . recurrence . ngrams n) =<< getStr

-- decrypt' :: IO ()
-- decrypt' = putStrLn . lowercase <$> (`decrypt` "england") =<< getStr


matrix :: IO [String]
matrix = do
  let text = cp
  let text' = clean . lowercase $ text
  let m = splitT 8 text'
  for_ m $ \x -> do
    putStrLn x
  return m

-- final stuff
cp :: String
cp = "hcbxpcjlemyzlgjwagtfjhtnvvriarrqzvuqbipjrqhggrzwtfnahgkqfesrqszvodyabgcwafvvrotsotdreoaqnbnfzgcbqetqloafvvnpapnqvzrzvyarnrpzgoashycwzvvwaphmbssvvhyammusjhnsfwnbbhbshhuwtkjylhrangemwsjnvprdatnrpshseanrumabcbbphcacygjogiainojnvvbsdmhcbqgtvjeyloavjoianmrrln"
