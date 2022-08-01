
-- |

module ChapterExercises where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.State.Lazy
import           Data.Functor                   ((<&>))

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe

rDec :: Num a => Reader a a
rDec = ReaderT $ pure . subtract 1

rShow :: Show a => Reader a String
rShow = ReaderT $ pure . show


rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
--rPrintAndInc = ReaderT $  \r -> print ("Hi " ++ show r) >> return (r + 1)
-- rPrintAndInc = ReaderT $  \r -> do
--   print $ "Hi " ++ show r
--   return $ r + 1
--rPrintAndInc = ReaderT $ \r -> putStrLn ("Hi " ++ show r) >> return (r+1)
--rPrintAndInc = ask >>= liftIO . putStrLn . ("Hi " ++)  . show >> ask >>= return . (+1)
--rPrintAndInc = (ask >>= liftIO . putStrLn . ("Hi " ++)  . show >> ask) <&> (+1)
rPrintAndInc = do
  r <- ask
  liftIO $ putStrLn $ "Hi " ++ show r
  return $ r + 1


sPrintIncAccum :: (Num a, Show a) => StateT a IO String
-- sPrintIncAccum = get >>= liftIO . putStrLn . ("Hi " ++) . show >> modify (+1) >> get >>= return . show . subtract 1
-- sPrintIncAccum = (get >>= liftIO . putStrLn . ("Hi " ++) . show >> modify (+1) >> get) <&> (show . subtract 1)
sPrintIncAccum = do
  s <- get
  liftIO $ putStrLn ("Hi " ++ show s)
  let s' = s + 1
  put s'
  return $ show s


--

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <-
    runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)
