module Main where

import qualified ShiftCipher as Shift

main :: IO ()
main = do
  -- text <- readFile "./app/TEXT"
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
      return ()
    _ -> do
      putStrLn "Invalid mode"
      main

prompt :: IO ()
prompt = do
  putStrLn "Operation?"
  putStrLn "1. Encrypt given message with key..."
  putStrLn "2. Decrypt given message with key..."
  putStrLn "3. Run brute-force attack..."
  putStrLn "4. Exit"

getMessage :: IO String
getMessage = do
  putStrLn "Enter text / message:"
  getLine

getKey :: IO Int
getKey = do
  putStrLn "Enter a key:"
  readLn :: IO Int
