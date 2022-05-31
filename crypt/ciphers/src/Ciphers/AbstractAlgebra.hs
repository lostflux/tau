{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# OPTIONS -Wall -fwarn-tabs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}

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
  , pollardRho
  , findFactor, n
  , a, b, p, q, powP
  , n', a', b', c', d'
  , g_1, g_2
  )
where


import Control.Monad          (void, when)
import Prelude                hiding (gcd)
import System.IO.Unsafe       (unsafePerformIO)
import System.Random.Stateful (Random (random), getStdGen, setStdGen)
import Text.Printf            (printf, PrintfArg)
import Math.NumberTheory.Roots (integerSquareRoot)
import Data.List (elemIndex)
import Data.Maybe (fromJust)


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
invN n a = iter n (a `mod` n) 1
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
primeFactors n = sieve $ factors n
  where
    sieve []     = []
    sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

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
    iter n a i = do

      let g = gcd (a - 1) n
      let next = i + 1

      void $ printf "n = %5d\ta_%d = %5d\tgcd = %5d\n" n i a g

      if g /= 1 && g /= n then
        return (g, n `div` g)
      else if g == n then
        iter n (randomInt 2 n) 1
      else
        iter n ((a ^ next) `mod` n) next

pollardRho :: Integer -> IO (Integer, Integer)
pollardRho n = iter 2 2 1 1
  where
    iter :: Integer -> Integer -> Integer -> Integer -> IO (Integer, Integer)
    iter x y d i
      | d == n = return (1, n)
      | otherwise = do
        let x' = g x
        let y' = (g . g) y
        let d' = gcd (abs $ x' - y') n
        void $ printf "i = %d, x = %d, y = %d, d = %d\n" i x' y' d'
        if d' > 1 then do
          return (d', n `div` d')
        else
          iter x' y' d' (i + 1)
      where
        g x = (x^2 + 1) `mod` n

fermi :: Integer -> IO (Integer, Integer)
fermi n = iter $ ceiling $ sqrt (fromInteger n :: Double)
  where
    iter :: Integer -> IO (Integer, Integer)
    iter a
      | a > n = do
        putStrLn "Failed to find factor"
        return (1, n)
      | otherwise = do
        let b2 = a^2 - n
        let b = sqrt (fromInteger b2 :: Double)
        printf "%d, %f\n" a b
        if isInt b then do
          let intB = floor b
          printf "match found: %d, %d\n" a intB
          printf "product = %d\n" $ a * intB
          return (a, intB)
        else
          iter (a + 1)

test :: Integer -> IO ()
test n = print (fromIntegral n :: Double)

isInt :: Double -> Bool
isInt x = x == fromInteger (floor x)

isComposite :: Integer -> IO Bool
isComposite n = iter n 2
  where
    iter :: Integer -> Integer -> IO Bool
    iter n i
      | i >= n = return False
      | otherwise = do
        let val = i ^ (n - 1) `mod` n
        void $ printf "%d ^ %d `mod` %d = %d\n" i (n - 1) n val
        if val == 1 then
          return True
        else
          iter n (i + 1)

checkComposite :: Integer -> IO ()
checkComposite n = iter n 2
  where
    iter :: Integer -> Integer -> IO ()
    iter n i
      | i >= n = return ()
      | otherwise = do
        let val = i ^ (n - 1) `mod` n
        void $ printf "%d ^ %d `mod` %d = %d\n" i (n - 1) n val
        iter n (i + 1)

squareN :: Integral a => a -> a -> a
squareN base n = (n ^ 2) `mod` base

babyStep :: Integral a => a -> a -> a -> Int -> [a]
babyStep base h g count = take count $ iterate (`mul` g) h
  where
    mul = mulN base

giantStep :: Integral a => a -> a -> a -> Int -> [a]
giantStep base g m count = take count $ iterate (`mul` (g ^ m)) 1
  where
    mul = mulN base

powN :: (Integral a, Integral b) => a -> a -> b -> a
powN base n pow = (n `power` pow) `mod` base
  where
    power n p
      | p == 0    = 1
      | p == 1    = n
      | p == 2    = n^2
      | even p    = (power n (p `div` 2) ^ 2) `mod` base
      | otherwise = (n * (power n ((p - 1) `div` 2) ^ 2)) `mod` base

a, b, p, q :: Integer
q = 5
p = 27781703927
a = 1002883876
b = 21790753397
powP :: Integer -> Integer -> Integer
powP = powN p

babyGiant :: (Integral a, PrintfArg a) => a -> a -> a -> a -> Int -> IO a
babyGiant base h g m count = do
  let baby = babyStep base h g count
  let giant = giantStep base g m count
  iter baby giant 0
  where
    iter :: (Integral a, PrintfArg a) => [a] -> [a] -> Int -> IO a
    iter [] _ _ = return (-1)
    iter _ [] _ = return (-1)
    iter babySteps (x:xs) i = do
      if x `elem` babySteps then do
        printf "Found match: %d\n" x
        let index' = index x babySteps
        printf "babystep = %d\ngiantstep = %d\n" index' i
        return x
      else
        iter babySteps xs $ i + 1
      where
        index item list = fromJust $ elemIndex item list

phi :: Integer -> Integer
phi n = fromIntegral $ length $ primeFactors n

pohligHellman :: Integer -> Integer -> Integer -> IO ()
pohligHellman modulus number base = mapM_ check factors'
  where
    p = modulus - 1
    factors' = primeFactors p
    check :: Integer -> IO ()
    check factor = do
      let l = highestPower factor p 
      let nExp = factor ^ l
      let rem = modulus `div` nExp
      let aRem = powN modulus number rem  
      let range = [0..nExp-1]
      let results = zip range $ map (\x -> powN modulus base (rem * x)) range
      printf "check %d = %d\n\n" nExp $ find' aRem results
      print results

find' :: Integer -> [(Integer, Integer)] -> Integer
find' item arr = iter item arr
  where
    iter _ [] = -1
    iter item ((r, nR):rest)
      | nR == item = r
      | otherwise = iter item rest

highestPower :: Integer -> Integer -> Integer
highestPower a b = iter a b 0
  where
    iter a b i
      | b `mod` (a ^ (i+1)) == 0 = iter a b (i+1)
      | otherwise = i 

findFactor :: Integer -> Integer -> IO [Integer]
findFactor n count = iter start 1 count []
  where
    start = ceiling $ sqrt (fromInteger n :: Double)
    iter :: Integer -> Integer -> Integer -> [Integer] -> IO [Integer]
    iter start step count acc
      | count <= 0 = do
        print acc
        return acc
      | otherwise = do
        let current = start + step ^ 2
        let (d, m) = divMod n current
        let (d', m') = divMod n step
        -- printf "%d mod %d = %d\n" n current m
        if m == 0 && (m' == 0 && step /= 1) then do
          printf "Found factors: %d %d\n" current d
          printf "Found factors: %d %d\n" step d'
          iter start (step+1) (count-4) (current : d : step : d' : acc)
        
        else if m == 0 then do
          printf "Found factors: %d %d\n" current d
          iter start (step+1) (count-2) (current : d : acc)

        else if m' == 0 && step /= 1 then do
          printf "Found factors: %d %d\n" step d'
          iter start (step+1) (count-2) (step : d' : acc)

        else iter start (step+1) count acc

n :: Integer
n = 999999999999999999999999999999999919



n', a', b', c', d' :: Integer
n' = 2^29 - 1
a' = 258883717
b' = 301036180
c' = 126641959
d' = 2 * 3^2 * 5 * 11 * 29 * 79


a_1 :: Integer
a_1 = a' * b' * c'

g_1, g_2 :: IO Integer
g_1 = gcdL (a_1 + d') n'
g_2 = gcdL (a_1 - d') n'





randomInt :: Integer -> Integer -> Integer
randomInt lo hi = unsafePerformIO $ do
  !num <- randomN
  return $! lo + toInteger num `mod` (hi - lo + 1)

randomN :: IO Int
randomN = do
  gen <- getStdGen
  let (num, newGen) = random gen
  setStdGen newGen
  return num

