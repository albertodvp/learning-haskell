{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AppRTWTST where
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT (runReaderT))
import           Control.Monad.State        (MonadState, StateT (runStateT),
                                             evalStateT)


import           Control.Monad.Trans.Writer (WriterT (runWriterT))
import           Control.Monad.Writer       (MonadWriter)
import           Du.AppTypes                (AppConfig, AppEnv, initialEnv)

newtype MyApp logEntry state a = MyApp {
  runApp :: ReaderT AppEnv (
      WriterT [logEntry] (
        StateT state IO
      )) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadWriter [logEntry], MonadState state)

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app cfg st = evalStateT (
  runWriterT (
      runReaderT (
          runApp app
          ) (initialEnv cfg)
      )
  ) st

