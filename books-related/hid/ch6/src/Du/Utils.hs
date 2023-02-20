-- |

module Du.Utils where
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Trans.RWS
import           Data.Foldable           (traverse_)
import           Du.AppRWST              (MyApp)
import           Du.AppTypes             (AppConfig (..), AppEnv (..))
import           System.Directory        (listDirectory)
import           System.FilePath         (isExtensionOf, (</>))
import           System.Posix

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
  AppEnv {fileStatus, path} <- ask
  liftIO $ fileStatus path


traverseDirectoryWith :: MyApp le s () -> MyApp le s ()
traverseDirectoryWith app = do
  curPath <- asks path
  content <- liftIO $ listDirectory curPath
  traverse_ go content
  where
    go name = flip local app
              $ \env -> env {
                path = path env </> name,
                depth = depth env + 1
                }

traverseDirectoryWith' :: MyApp ls s () -> MyApp ls s ()
traverseDirectoryWith' app = asks path >>= liftIO . listDirectory >>= traverse_ go
  where
    go name = flip local app
              $ \env -> env {
                path = path env </> name,
                depth = depth env + 1
                }


checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg fp = maybe True (`isExtensionOf` fp) (extension cfg)
