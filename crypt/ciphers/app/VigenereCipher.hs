{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module VigenereCipher
  ( encrypt,
    decrypt,
  )
where

import Common  (clean, lowercase, splitT, toInt, uppercase)
import Prelude

-- | Encrypts a plaintext using a key, per the vigenère cipher.
encrypt :: String -> String -> String
encrypt = undefined

-- | Decrypts a ciphertext using a key, per the vigenère cipher
decrypt :: Integer -> String -> IO String
decrypt _ [] = return []
decrypt 0 str = return str
decrypt n str = do
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
