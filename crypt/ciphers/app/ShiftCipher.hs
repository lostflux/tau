{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module ShiftCipher (
    encrypt
  , decrypt
  , bruteforceAttack
  , bruteforce
) where

import Prelude hiding (lookup)

import Data.Char ( ord, chr )
import Data.Foldable ( for_ )
import Data.List ( elemIndex )
import Data.Maybe (fromJust)
import Common ( clean, lowercase, uppercase )

---------- Cipher
encrypt :: Int -> String -> String
encrypt key str = do
  let rawText = lines str
  let cleanedText = map (uppercase . clean) rawText
  let ciphertext = map (shift key) cleanedText
  unlines ciphertext

decrypt :: Int -> String -> String
decrypt key ciphertext = do
  let rawText = lines ciphertext
  let cleanedText = map (uppercase . clean) rawText
  let plaintext = map (shift (-key)) cleanedText
  lowercase $ unlines plaintext

-- | Shift a string, the easy way -- add and mod.
-- | We assume char is uppercase.
shift :: Int -> String -> String
shift _ [] = []
shift n (c:cs) = shiftChar n c : shift n cs
  where
    shiftChar n c =
      let pos = ord c - ord 'A'
      in chr ((pos + n) `mod` 26 + ord 'A')


bruteforceAttack :: String -> [String]
bruteforceAttack str = checkAll 0 str []
  where
    checkAll 26 _ arr = arr
    checkAll n text results =
      checkAll (n+1) text (results ++ [decrypt n text])

bruteforce :: [Char] -> IO ()
bruteforce str = do
  let results = bruteforceAttack str
  for_ results $ \result -> do
    putStrLn $ "\t" ++ show (fromJust (elemIndex result results)) ++ ": " ++ result
