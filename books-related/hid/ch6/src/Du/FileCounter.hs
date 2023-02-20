module Du.FileCounter where
import           Control.Monad.RWS
import           Du.AppRWST             (MyApp)
import           Du.AppTypes            (AppConfig (maxDepth), AppEnv (..))
import           Du.Utils               (checkExtension, currentPathStatus,
                                         traverseDirectoryWith')
import           System.Directory.Extra (listFiles)
import           System.Posix           (isDirectory)

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
  AppEnv {..} <- ask
  fs <- currentPathStatus
  when (isDirectory fs && depth <= maxDepth cfg) $ do
    traverseDirectoryWith' fileCount
    files <- liftIO $ listFiles path
    tell [(path, length $ filter (checkExtension cfg) files)]
