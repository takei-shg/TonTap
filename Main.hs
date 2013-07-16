import Control.Monad.Trans
-- import Control.Monad.Maybe
import Data.Maybe
import Network.HTTP (Response , simpleHTTP , getRequest , getResponseBody , rspBody)
import Network.URI (parseURI)
import Network.Stream (Result)

main :: IO ()
main = do
  putStrLn "Hello"
  -- str <- either show rspBody getBody
  str <- getBody url
  putStrLn str

-- getBody :: IO (Result (Response ty))
getBody :: String -> IO String
getBody url = case parseURI url of
  Nothing -> fail "invalid url"
  Just u -> simpleHTTP (getRequest $ show u) >>= fmap (take 10000) . getResponseBody
  where
    url = "http://craftheads.jp/craftheads/News/News.html"
    -- url = "http://www.haskell.org"

url :: String
url = "http://craftheads.jp/craftheads/News/News.html"

-- openUrl :: String -> MaybeT IO String
-- openUrl url = case parseURI url of
--   Nothing -> fail "invalid url"
--   Just u -> return u
