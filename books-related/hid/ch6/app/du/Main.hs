-- |

module Main where
import qualified Data.Text.IO        as TIO
import           Du.AppRWST          (runMyApp)
import           Du.AppTypes         (AppConfig (AppConfig))
import           Du.DirTree          (dirTree, treeEntryBuilder)
import           Du.DiskUsage        (diskUsage)
import           Du.FileCounter      (fileCount)
import           Options.Applicative
import           System.Posix.Types  (FileOffset)
import           TextShow

work :: AppConfig -> IO ()
work config = do
  (_, dirs) <- runMyApp dirTree config ()
  (_, counters) <- runMyApp fileCount config ()
  (_, usages) <- runMyApp diskUsage config (0 :: FileOffset)
  let report = toText $
               buildEntries "Directory tree:" treeEntryBuilder dirs
               <> buildEntries "File counter:" tabEntryBuilder counters
               <> buildEntries "File space usage:" tabEntryBuilder usages
  TIO.putStr report


buildEntries :: Builder -> (e -> Builder) -> [e] -> Builder
buildEntries title entryBuilder entries = unlinesB $ title : map entryBuilder entries

tabEntryBuilder :: TextShow s => (FilePath, s) -> Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp


getConfig :: Parser AppConfig
getConfig = AppConfig
            <$> argument str (metavar "BASE PATH" <> value "." <> showDefault)
            <*> option auto (long "max-depth" <> short 'd')
            <*> optional (strOption (long "extension" <> short 'e'))
            <*> switch (long "follow-symlinks" <> short 'L')

main :: IO ()
main = execParser (info (getConfig <**> helper) fullDesc) >>= work
