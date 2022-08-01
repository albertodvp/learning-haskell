{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef

import qualified Data.Map                   as M

import           Control.Monad.IO.Class
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Trans
data Config = Config {
  counts :: IORef (M.Map Text Integer),
  prefix :: Text
  }


type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k mp = (M.insert k new_value mp, new_value)
  where
    new_value = new_value_f 1
    new_value_f inc = maybe inc (+inc) (mp M.!? k)


app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key"
  config <- lift ask
  let key' = mappend (prefix config) unprefixed
  mp <- liftIO $ readIORef (counts config)
  let (newMap, newInteger) = bumpBoomp key' mp
  liftIO $ writeIORef (counts config) newMap
  html $
    mconcat [ "<h1>Success! Count was: "
            , TL.pack $ show newInteger
            , "</h1>"
            ]



main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config counter (TL.pack prefixArg)
        runR = ($ config) . runReaderT
    scottyT 3001 runR app
