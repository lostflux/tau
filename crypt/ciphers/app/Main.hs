module Main where

import qualified ShiftCipher as Shift
import qualified VigenereCipher as Vigenere
import qualified SubstitutionCipher as Substitution
import Control.Monad (void)

main :: IO ()
main = do
  text <- readFile "./data/TEXT"
  Shift.bruteforce text

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
      message <- readFile "./data/TEXT2"
      -- message <- getMessage
      -- key <- getKey
      let key = 7876565434321123434565678788787656543432112343456567878878765654433211234
      results <- Vigenere.decrypt key message
      putStrLn $ "\nresults: \n\t" ++ results
      main
    5 -> do
      message <- readFile "./data/TEXT3"
      print Substitution.standardFrequencies
      plaintext <- Substitution.bruteforce message
      putStrLn $ "original: \n\t" ++ message
      putStrLn $ "\nplaintext: \n\t" ++ plaintext
    6 -> return ()
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
  putStrLn "6. Exit."
getMessage :: IO String
getMessage = do
  putStrLn "Enter text / message:"
  getLine

getKey :: IO Int
getKey = do
  putStrLn "Enter a key:"
  readLn :: IO Int
