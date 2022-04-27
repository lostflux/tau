{-# LANGUAGE ImportQualifiedPost #-}

module MyData.Parser (
  parse, tP1, testHXT
) where

-- >>> :set -package utf8-string-1.0.2
-- >>> :set -package text
-- >>> :set -package tagsoup-0.14.8
-- >>> :set -package HTTP-4000.4.0
import Control.Monad             (void)
import Data.ByteString           (ByteString)
import Data.ByteString.Lazy      qualified as BL
import Data.ByteString.Lazy.UTF8 qualified as BL
import Network.HTTP.Conduit      (simpleHttp)
import Network.HTTP.Simple       (getResponseBody)
import Text.HTML.TagSoup         (Tag, fromTagText, innerText, parseTags,
                                  sections, (~/=), (~==))
import Text.XML.HXT.Core         (ArrowTree ((//>)),
                                  ArrowXml (getAttrValue, hasName), no,
                                  readString, runX, withParseHTML, withWarnings,
                                  yes, (>>>))

extractText :: [Tag ByteString] -> [ByteString]
extractText = map fromTagText . filter (~/= "script") . filter (~/= "style")


parse :: String -> IO String
parse url = do
  lbs <- simpleHttp url
  -- putStrLn $ BL.toString lbs
  let tags = parseTags lbs
  -- void $ putStrLn $ concatMap (\x -> show x ++ "\n" ++ "\n") tags
  let lastModifiedDateTime = fromFooter $ parseTags (BL.toString lbs)
  -- putStrLn $ "wiki.haskell.org was last modified on " ++ lastModifiedDateTime
  return $ BL.toString lbs
  where fromFooter = unwords . drop 6 . words
            . innerText . take 2 . dropWhile (~/= "<li id=footer-info-lastmod>")


-- tP1 = parse "https://wiki.haskell.org"
tP1 :: IO String
tP1 = parse "https://github.com/siavava"

testHXT :: IO ()
testHXT = do
  -- html <- readFile "test.html"
  html <- parse "https://github.com/siavava"
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc //> hasName "a" >>> getAttrValue "href"
  mapM_ putStrLn links
