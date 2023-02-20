{-# LANGUAGE RecordWildCards #-}

module Du.DiskUsage where

import           Control.Monad.RWS
import           Du.AppRWST         (MyApp)
import           Du.AppTypes        (AppConfig (..), AppEnv (..))
import           Du.Utils           (checkExtension, currentPathStatus,
                                     traverseDirectoryWith')
import           System.Posix       (fileSize, isDirectory, isRegularFile)
import           System.Posix.Types (FileOffset)


data DUEntryAction =
    TraverseDir {dirpath :: FilePath, requiredReporting :: Bool}
  | RecordFileSize {fsize :: FileOffset}
  | None

diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
  where
    decide AppEnv {..} fs
      | isDirectory fs = TraverseDir path (depth <= maxDepth cfg)
      | isRegularFile fs && checkExtension cfg path = RecordFileSize (fileSize fs)
      | otherwise = None
    processEntry TraverseDir {..} = do
      usageOnEntry <- get
      traverseDirectoryWith' diskUsage
      when requiredReporting $ do
        usageOnExit <- get
        tell [(dirpath, usageOnExit - usageOnEntry)]
    processEntry RecordFileSize {fsize} = modify (+fsize)
    processEntry None = pure ()

