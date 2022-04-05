{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module VigenereCipher (
  decrypt
) where

import Prelude
import Data.Foldable (for_)
import Data.List (transpose)
import Common ( toInt, clean, lowercase, uppercase )

----------------------------------------------------------------
-- CYPHERS
----------------------------------------------------------------

decrypt :: Integer -> String -> IO String
decrypt _ [] = return []
decrypt 0 str = return str
decrypt n str = do
  let splitMessage = down $ (uppercase . clean) str
  putStrLn "Matrix: "
  for_ splitMessage \row -> do
    putStrLn $ "  " ++ row
  lowercase <$> build (show n) splitMessage []
  where

    build :: String -> [String] -> String -> IO String
    build _ [] result = return result
    build [] _ result = return result
    build (k:ks) matrix result = do
      let key = toInt k
      let line = matrix !! (key - 1)
      let letter = line !! length result
      build ks matrix $ result ++ [letter]

split :: Int -> String -> [String]
split 0 results = [results]
split _ [] = []
split n str = let (a,b) = splitAt n str in a : split n b

splitEights :: String -> [String]
splitEights = split 8

down :: String -> [String]
down = transpose . splitEights


-- data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
--   deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- instance Real Day where
--   toRational x = toRational $ fromEnum x

-- instance Integral Day where
--   toInteger = fromIntegral
--   quotRem x y =
--     let (x', y') = (fromIntegral x, fromIntegral y)
--       in (x' `div` y', x' `mod` y')


-- instance Num Day where
--   (+) day1 day2 = toEnum (fromEnum day1 + fromEnum day2)
--   (*) day1 day2 = toEnum (fromEnum day1 * fromEnum day2)
--   abs = id
--   signum = const 1
--   fromInteger = toEnum . fromIntegral
--   negate day = fromIntegral $ 6 - fromEnum day 
