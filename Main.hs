import Control.Monad.Trans
import Control.Monad.Maybe
import Data.Maybe
import Network.HTTP (Response , simpleHTTP , getRequest , getResponseBody , rspBody, mkRequest, RequestMethod(GET))
import Network.URI (parseURI)
import Network.Stream (Result)
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs (NTree)


-- main :: IO ()
main = do
  putStrLn "Hello"
  -- str <- either show rspBody getBody
  page <- get craftHeads_url
  contents <- runX $ page >>> craftHeads_filter
  putStrLn $ show contents

getBody :: String -> IO String
getBody url = case parseURI url of
  Nothing -> fail "invalid url"
  Just u -> simpleHTTP (getRequest $ show u) >>= fmap (take 10000) . getResponseBody

craftHeads_url :: String
craftHeads_url = "http://craftheads.blog88.fc2.com/blog-entry-197.html"
-- craftHeads_url = "http://craftheads.jp/craftheads/Beers.html"

-- craftHeads_filter :: ArrowXml a => a XmlTree String
craftHeads_filter = css "div" >>> hasAttrValue "class" (== "entry-body-container") //> getText
openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
  Nothing -> fail "invalid url"
  Just u -> liftIO $ simpleHTTP (mkRequest GET u) >>= getResponseBody

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

get :: String -> IO (IOSArrow XmlTree (NTree XNode))
get url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withParseHTML yes, withWarnings no] $ fromMaybe "error" contents
