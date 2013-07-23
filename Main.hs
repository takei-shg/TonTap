{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

import System.IO (withFile, hPutStrLn, IOMode(WriteMode))
import Control.Applicative
import Control.Monad
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
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.DateTime (getCurrentTime)
import qualified Data.Text as Text
import qualified Data.Aeson as A

-- import qualified Network.Riak.JSON.Resolvable (get, put)
import qualified Network.Riak as R
import qualified Network.Riak.JSON as RJ


-- main :: IO ()
main = do
  -- let client = Riak.defaultClient { Riak.host = "127.0.0.1", Riak.port = "8087" }
  conn <- R.connect R.defaultClient
  page <- get craftHeads_url
  contents <- runX $ page >>> craftHeads_filter
  let pabName = "craftheads"
  let bucket = LBS8.pack pabName
  -- key <- key_gen pabName
  let key = LBS8.pack pabName
  let toStore = OnTapInfo { uri = craftHeads_url, siteName = "CraftHeads", html = Text.pack <$> contents }
  RJ.put conn bucket key Nothing toStore R.Default R.Default
  R.disconnect conn
--   withFile "craftheads.txt" WriteMode (\handle -> do
--     hPutStrLn handle "Hello"
--     -- str <- either show rspBody getBody
--     -- str <- getBody craftHeads_url
--     page <- get craftHeads_url
--     -- hPutStrLn handle $ toString $ str
--     contents <- runX $ page >>> craftHeads_filter
--     mapM_ putStrLn contents
--     mapM_ (hPutStrLn handle) contents
--     )

-- instance Riak.Resolvable Text where
--   resolve x y = x

data OnTapInfo = OnTapInfo {
  uri :: String
, siteName :: String
, html :: [Text.Text]
}

instance A.ToJSON OnTapInfo where
  toJSON x = A.object [
               "uri" A..= (uri x)
             , "siteName" A..= (siteName x)
             , "html" A..= (html x)
             ]

instance A.FromJSON OnTapInfo where
  parseJSON (A.Object v) = OnTapInfo
    <$> (v A..: "uri")
    <*> (v A..: "siteName")
    <*> (v A..: "html")
  parseJSON _ = mzero

key_gen :: String -> IO LBS8.ByteString
key_gen str = do
  currentTime <- getCurrentTime
  return $ LBS8.pack (str ++ show currentTime)

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
