{-# LANGUAGE OverloadedStrings #-}

import System.IO (withFile, hPutStrLn, IOMode(WriteMode))
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Maybe
import Data.Maybe
import Network.HTTP (Response , simpleHTTP , getRequest , getResponseBody , rspBody, mkRequest, RequestMethod(GET))
import Network.URI (parseURI)
import Network.Stream (Result)
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs (NTree)
import Codec.Binary.UTF8.String (encodeString)
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


-- main :: IO ()
main = do
  withFile "craftheads.txt" WriteMode (\handle -> do
    hPutStrLn handle "Hello"
    -- str <- either show rspBody getBody
    -- str <- getBody craftHeads_url
    page <- get craftHeads_url
    -- hPutStrLn handle $ toString $ str
    contents <- runX $ page >>> craftHeads_filter
    mapM_ (hPutStrLn handle) contents
    )

getBody :: String -> IO BS.ByteString
getBody url = case parseURI url of
  -- Nothing -> fail ""
  -- Just u -> simpleHTTP (getRequest $ show u) >>= fmap (take 10000) . getResponseBody
  Just u -> simpleHTTP (mkRequest GET u) >>= getResponseBody

craftHeads_url :: String
craftHeads_url = "http://craftheads.blog88.fc2.com/blog-entry-197.html"
-- craftHeads_url = "http://craftheads.jp/craftheads/Beers.html"

craftHeads_filter :: ArrowXml a => a XmlTree String
craftHeads_filter = css "div" >>> hasAttrValue "class" (== "entry-body-container") //> getText
openUrl :: String -> MaybeT IO BS.ByteString
openUrl url = case parseURI url of
  Nothing -> fail "invalid url"
  Just u -> liftIO $ simpleHTTP (mkRequest GET u) >>= getResponseBody

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

get :: String -> IO (IOSArrow XmlTree (NTree XNode))
get url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withParseHTML yes, withWarnings no] $ fromMaybe "error" (toString <$> contents)
