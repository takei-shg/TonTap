import System.IO

main :: IO ()
main = do
  withFile "test.txt" ReadMode (\rhandle -> do
    contents <- hGetContents rhandle
    withFile "testwrite.txt" WriteMode (\whandle -> do
      hPutStrLn whandle contents)
    )
