{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# OPTIONS -Wall -fwarn-tabs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ciphers.AbstractAlgebra (
    addN
  , mulN
  , subN
  , divN
  , invN
  , gcd
  , gcdL
  , prompt
  , run
  , extendedEuclidean
  , factorize
  , factors
  , factors'
  , primeFactors
  , randomInt
  , pollard
  , pollard'
  )
where


import Prelude                hiding (gcd)
import System.IO.Unsafe       (unsafePerformIO)
import System.Random.Stateful (Random (random), getStdGen, setStdGen)
import Text.Printf (printf)
import Control.Monad (void)


-- | Compute multiplication modulo N.
--
--  The result is always in the range [0,N).
mulN :: Integral a => a -> a -> a -> a
mulN n a b = (a * b) `mod` n

addN :: Integral a => a -> a -> a -> a
addN n a b = (a + b) `mod` n

subN :: Integral a => a -> a -> a -> a
subN n a b = (a - b) `mod` n

gcd :: Integer -> Integer -> Integer
gcd = euclideanA

gcdL :: Integer -> Integer -> IO Integer
gcdL = euclideanL
  where
    euclideanL :: Integer -> Integer -> IO Integer
    euclideanL a 0 = putStrLn ("gcd " ++ show a ++ " 0: " ++ show a) >> return a
    euclideanL 0 b = putStrLn ("gcd 0 " ++ show b ++ ": " ++ show b) >> return b
    euclideanL a b
      | a == b = do
          putStrLn ("gcd (" ++ show (a `div` b) ++ ") " ++ show a ++ " " ++ show b ++ ": " ++ show b)
          return a
      | a > b = do
          putStrLn ("gcd (" ++ show (a `div` b) ++ ") " ++ show a ++ " " ++ show b ++ "...")
          euclideanL b (a `mod` b)
      | otherwise = do
          putStrLn ("gcd (" ++ show (a `div` b) ++ ") " ++ show a ++ " " ++ show b ++ "...")
          euclideanL a (b `mod` a)

extendedEuclidean :: Integral b => b -> b -> (b, b, b)
extendedEuclidean 0 y = (y, 0, 1)
extendedEuclidean x y =
  let (gcd, x', y') = extendedEuclidean (y `mod` x) x
      x'' = y' - (y `div` x) * x'
      y'' = x'
  in (gcd, x'', y'')

euclideanA :: Integral a => a -> a -> a
euclideanA a 0 = a
euclideanA 0 b = b
euclideanA a b
  | a == b = a
  | a > b = euclideanA b (a `mod` b)
  | otherwise = euclideanA a (b `mod` a)

divN :: Integral a => a -> a -> a -> a
divN n a b = (a * invN n b) `mod` n

invN :: Integral p => p -> p -> p
invN _ 0 = 0
invN _ 1 = 1
invN n a = iter n a 1
  where
    mul = mulN n
    iter n a b
      | a > n || b > n = 0
      | a `mul` b == 1 = b
      | otherwise = iter n a $ b + 1

prompt :: IO ()
prompt = do
  putStrLn "Please select an action:"
  putStrLn "1. Add"
  putStrLn "2. Subtract"
  putStrLn "3. Multiply"
  putStrLn "4. Divide"
  putStrLn "5. Inverse"
  putStrLn "6. GCD"
  putStrLn "7. GCD (Logging)"
  putStrLn "8. extendedEu"
  return ()

run :: Integer -> Integer -> Integer -> Integer -> IO Integer
run action base a b = case action of
  1 -> return $ addN base a b
  2 -> return $ subN base a b
  3 -> return $ mulN base a b
  4 -> return $ divN base a b
  5 -> return $ invN base a
  6 -> return $ gcd a b
  7 -> gcdL a b
  8 -> do
    let tu = extendedEuclidean a b
    print tu
    return $ (\(a, _, _) -> a) tu
  _ -> return 0

factorize :: Integer -> [(Integer, Integer)]
factorize n = [(i, n `div` i) | i <- [2.. rootN], n `mod` i == 0]
  where
    rootN :: Integer
    rootN = fromIntegral $ (+ 1) . floor . sqrt . fromIntegral $ n

factors :: Integer -> [Integer]
factors n = factorize n >>= \(i, _) -> [i]

factors' :: Integer -> [Integer]
factors' n = reverse $ factorize n >>= \(_, j) -> [j]

primeFactors :: Integer -> [Integer]
primeFactors n = sieve [] $ factors n
  where
    sieve acc []     = acc
    sieve acc (x:xs) = sieve (acc ++ [x]) [i | i <- xs, i `mod` x /= 0 ]

pollard :: Integer -> (Integer, Integer)
pollard n = iter n 2 1
  where
    iter n x i
      | g /= 1 && g /= n    = (g, n `div` g)
      | g == n              = iter n (randomInt 2 n) 1
      | otherwise           = iter n ((x ^ i) `mod` n) (i + 1)
        where
          g = gcd (x - 1) n

pollard' :: Integer -> IO (Integer, Integer)
pollard' n = iter n 2 1
  where
    iter :: Integer -> Integer -> Integer -> IO (Integer, Integer)
    iter n x i = do
      let g = gcd (x - 1) n
      void $ printf "n = %5d\ti = %5d\ta_i = %5d\tgcd = %5d\n" n i x g
      if g /= 1 && g /= n
        then return (g, n `div` g)
        else if g == n
          then iter n (randomInt 2 n) 1
          else iter n ((x ^ i) `mod` n) (i + 1)


randomInt :: Integer -> Integer -> Integer
randomInt lo hi = unsafePerformIO $ do
  !num <- randomN
  return $! lo + toInteger num `mod` (hi - lo + 1)

randomN :: IO Int
{-# NOINLINE randomN #-}
randomN = do
  gen <- getStdGen
  let (num, newGen) = random gen
  setStdGen newGen
  return num
