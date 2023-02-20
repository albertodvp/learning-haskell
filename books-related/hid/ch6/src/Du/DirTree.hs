module Du.DirTree where
import           Control.Monad.RWS
import           Data.Text.Lazy.Builder
import           Du.AppRWST             (MyApp)
import           Du.AppTypes            (AppConfig (maxDepth), AppEnv (..))
import           Du.Utils               (currentPathStatus,
                                         traverseDirectoryWith')
import           System.FilePath        (takeBaseName)
import           System.Posix           (isDirectory)

dirTree :: MyApp (FilePath, Int) s ()
dirTree = do
  AppEnv {..} <- ask
  fs <- currentPathStatus
  when (isDirectory fs && depth <= maxDepth cfg) $ do
    tell [(takeBaseName path, depth)]
    traverseDirectoryWith' dirTree


treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
  where
    indent = replicate (2*n) ' '
