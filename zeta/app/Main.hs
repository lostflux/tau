-- {-# LANGUAGE ImportQualifiedPost #-}
module Main where

import qualified MyData as MDT

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MDT.tP1
