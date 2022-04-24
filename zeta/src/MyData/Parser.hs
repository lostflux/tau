-- {-# LANGUAGE ImportQualifiedPost #-}

module MyData.Parser (
  parse, tP1
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import Network.HTTP.Conduit
import Text.HTML.TagSoup

parse :: String -> IO ()
parse url = do
  lbs <- simpleHttp url
  putStrLn $ BL.toString lbs
  let lastModifiedDateTime = fromFooter $ parseTags (BL.toString lbs)
  putStrLn $ "wiki.haskell.org was last modified on "
      ++ lastModifiedDateTime
  where fromFooter = unwords . drop 6 . words
            . innerText . take 2 . dropWhile (~/= "<li id=footer-info-lastmod>")


tP1 = parse "https://wiki.haskell.org"
