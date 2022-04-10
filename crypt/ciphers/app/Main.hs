module Main where

import qualified ShiftCipher as Shift
import qualified VigenereCipher as Vigenere
import qualified SubstitutionCipher as Substitution
import qualified AbstractAlgebra as Algebra
import System.IO (stdout, hFlush)
import Control.Monad (void)
import Control.Monad.Cont (join)

main :: IO ()
main = do
  -- text <- readFile "./data/TEXT"
  -- Shift.bruteforce text

  prompt
  mode <- readLn :: IO Int
  case mode of
    1 -> do
      message <- getMessage
      key <- getKey
      putStrLn $ Shift.encrypt key message
      main
    2 -> do
      message <- getMessage
      key <- getKey
      putStrLn $ Shift.decrypt key message
      main
    3 -> do
      message <- getMessage
      Shift.bruteforce message
      main
    4 -> do
      -- message <- readFile "./data/TEXT2"
      message <- getMessage
      -- key <- getKey
      let key = 0o435237014536765252023174645673653671333323451410336736546663323554124527651421542
      let key' = 7876565434321123434565678788787656543432112343456567878878765654433211234
      let key2'' = 0x4753E0CAF7D554133E69777ABDCB6DA7298437BBD66D9B4ED854ABEA62362
      print $ key == key'
      results <- Vigenere.decrypt key message
      putStrLn $ "\nresults: \n\t" ++ results
      main
    5 -> do
      message <- readFile "./data/TEXT3"
      print Substitution.standardFrequencies
      plaintext <- Substitution.bruteforce message
      putStrLn $ "original: \n\t" ++ message
      putStrLn $ "\nplaintext: \n\t" ++ plaintext
      main
    6 -> do
      Algebra.prompt
      action <- getAction
      base <- getBase
      b <- getNumber
      c <- getNumber
      Algebra.run action base b c >>= print
      main
    7 -> return ()
    _ -> do
      putStrLn "Invalid mode"
      main

prompt :: IO ()
prompt = do
  putStrLn "Operation?"
  putStrLn "1. Encrypt given message with key..."
  putStrLn "2. Decrypt given message with key..."
  putStrLn "3. Run brute-force attack..."
  putStrLn "4. Vigenere cipher decryption..."
  putStrLn "5. Decrypt using substitution ciphers..."
  putStrLn "6. Run algebraic solver..."
  putStrLn "7. Exit."
getMessage :: IO String
getMessage = do
  putStrLn "Enter text / message:"
  getLine

getKey :: IO Int
getKey = do
  putStr "Enter a key: " >> hFlush stdout
  readLn :: IO Int

getAction :: IO Integer
getAction = do
  putStr "action: " >> hFlush stdout
  readLn :: IO Integer

getBase :: IO Integer
getBase = do
  putStr "base: " >> hFlush stdout
  readLn :: IO Integer

getNumber :: IO Integer
getNumber = do
  putStr "number: " >> hFlush stdout
  readLn :: IO Integer
