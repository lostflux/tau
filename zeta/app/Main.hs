{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import MyData        qualified as MDT
import MyData.Parser qualified as MDT
import MyData.Trie   qualified as Trie

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MDT.tP1
