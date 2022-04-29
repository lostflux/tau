{-# LANGUAGE ImportQualifiedPost #-}

module MyData.Parser (
  tP1
) where

-- >>> :set -package utf8-string-1.0.2
-- >>> :set -package text
-- >>> :set -package tagsoup-0.14.8
-- >>> :set -package HTTP-4000.4.0
import Control.Arrow                  (arr, (<+>))
import Control.Arrow.IOStateListArrow (IOSLA)
import Control.Monad                  (void, when)
import Data.ByteString.Lazy           (ByteString)
import Data.ByteString.Lazy           qualified as ByteString
import Data.ByteString.Lazy.UTF8      qualified as ByteString
import Data.List                      (isPrefixOf)
import Data.Maybe                     (fromMaybe)
import GHC.Data.Maybe                 (fromJust)
import MyData.Trie                    (Trie (..), clean, insert, makeRootTrie)
import Network.HTTP.Conduit           (simpleHttp)
import Network.HTTP.Simple            (getResponseBody)
import Text.HTML.TagSoup              (Tag, fromTagText, innerText, parseTags,
                                       sections, (~/=), (~==))
import Text.Printf                    (printf)
import Text.XML.HXT.Arrow.XmlState    (XIOState)
import Text.XML.HXT.Core              (ArrowTree (deep, (//>)),
                                       ArrowXml (getAttrValue, getText, hasName, hasText, isText, getAttrName, hasAttr),
                                       XNode (XText), XmlTree, no, readString,
                                       runX, withParseHTML, withWarnings, yes,
                                       (>>>))
import Text.XML.HXT.DOM.XmlNode       (NTree (..))

data WebPage = EmptyPage | WebPage {
    text  :: Trie
  , year  :: String
  , title :: String
  , links :: [Link]
}

instance Show WebPage where
  show EmptyPage = "EmptyPage"
  show (WebPage t y tl l) = printf "WebPage {text = %s\n\nyear = %d\n\ntitle = %s\n\nlinks = %s}" (show t) y tl (show l)

type Link = String


loadPage :: Link -> IO WebPage
loadPage url = do
  html <- ByteString.toString <$> simpleHttp url
  -- putStrLn html
  let doc = readString [withParseHTML yes, withWarnings no] html
  txt <- getWords doc
  ttl <- getTitle doc
  lnks <- getLinks url doc
  yr <- getYear doc
  print txt
  
  -- printf "\n\n\n"

  -- return $ WebPage txt yr ttl lnks
  return EmptyPage

-- getTitle :: XmlTree -> IO String

-- | Get the title of the page.
--
-- Returns an empty string if the title is not found.
getTitle :: IOSLA (XIOState ()) XmlTree (NTree XNode) -> IO String
getTitle doc = do
  ttl <- runX $ doc
    //> hasName "title"
    //> deep (isText >>> getText)
  return $ getFirst ttl

-- | Get all the words in a webpage.
--
-- Returns a Trie.
getWords :: IOSLA (XIOState ()) XmlTree (NTree XNode) -> IO Trie
getWords doc = do
  text <- runX $ doc
    //> (
      hasName "p" 
      <+> hasName "h1" <+> hasName "h2" 
      <+> hasName "h3" <+> hasName "h4" 
      <+> hasName "h5" <+> hasName "h6"
    ) //> deep (isText >>> getText)
    >>> arr words
  return $ foldl (foldr (insert . clean)) EmptyTrie text


getYear :: IOSLA (XIOState ()) XmlTree (NTree XNode) -> IO String
getYear doc = do
  yr <- runX $ doc
    //> hasName "a"
    //> getAttrValue "article:modified_time"
    -- >>> getText
      -- //> hasAttr "last-modified"
      -- >>> getAttrValue "last-modified")
  print yr
  -- return $ getFirst yr
  return ""


-- | Get all the links in the page.
--
-- Returns a list of strings.
getLinks :: Link -> IOSLA (XIOState ()) XmlTree (NTree XNode) -> IO [String]
getLinks url doc = do
  links <- runX $ doc
    //> hasName "a"
    >>> getAttrValue "href"
    >>> arr (\x -> if "/" `isPrefixOf` x then printf "%s%s" url x else x)
    >>> arr (\x -> if "#" `isPrefixOf` x then printf "%s/%s" url x else x)
  return $ filter (not . null) links


-- tP1 = parse "https://wiki.haskell.org"
tP1 :: IO WebPage
-- tP1 = loadPage "https://github.com/siavava"
tP1 = loadPage "https://www.nytimes.com/2022/04/28/health/menthol-ban-fda.html"

tP2 :: IO WebPage
-- tP2 = getHTML "https://www.nytimes.com/2022/04/28/health/menthol-ban-fda.html"
tP2 = loadPage "https://github.com/siavava"


-- | Fall-back get first item of an list.
--
-- If empty list, returns an empty string.
getFirst :: [String] -> String
getFirst []    = ""
getFirst (x:_) = x
