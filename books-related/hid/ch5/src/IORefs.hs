-- |

module IORefs where
import           Control.Monad.Extra    (ifM, whenM, zipWithM_)
import           Data.Foldable          (traverse_)
import           Data.IORef
import           System.Directory       (doesDirectoryExist)
import           System.Directory.Extra (listContents)
import           System.Environment
import           Text.Read              (readMaybe)


sumNumbers :: IO Int
sumNumbers = do
  s <- newIORef 0
  go s
  where
    go s = readNumber >>= processNumber s

    readNumber = putStrLn "Write a number" >> (readMaybe <$> getLine)
    processNumber s Nothing  = readIORef s
    processNumber s (Just n) = modifyIORef' s (+n)>> go s

main1 :: IO ()
main1 = do
  s <- sumNumbers
  print s

fileCount :: FilePath -> IO Int
fileCount fpath = do
  counter <- newIORef 0
  whenM (doesDirectoryExist fpath) $ go counter fpath
  readIORef counter
  where
    go cnt fp = listContents fp >>= traverse_ (processEntry cnt)
    processEntry cnt fp = ifM (doesDirectoryExist fp) (go cnt fp) (inc cnt)
    inc cnt = modifyIORef' cnt (+1)


main :: IO ()
main = do
  args <- getArgs
  counts <- traverse fileCount args
  zipWithM_ printRes args counts
  where
    printRes fp c = putStrLn $ fp ++ ":\t" ++ show c

