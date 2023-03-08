-- |

module Exercises where
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.List                 (singleton)
data ProtectedData a = ProtectedData String a

accessData :: String -> ProtectedData a -> Maybe a
accessData s (ProtectedData pass v) =
  if s == pass
  then Just v
  else Nothing


type Protected s a = MaybeT (Reader (ProtectedData s)) a

run :: ProtectedData s -> Protected s a -> Maybe a
run p m = runReader (runMaybeT m) p

access :: String -> Protected a a
--access s = ask >>= MaybeT . pure . accessData s
-- access s = asks (accessData s) >>= MaybeT . pure
access s = asks (accessData s) >>= MaybeT . pure


type Protected' s a = MaybeT (ReaderT (ProtectedData s) IO) a

run' :: ProtectedData s -> Protected' s a -> IO (Maybe a)
run' p m = runReaderT (runMaybeT m) p

access' :: Protected' a a
access' = liftIO getLine >>= asks . accessData >>= MaybeT . pure


data Item = Msg String
          | Section String [Item]
          deriving (Show,Eq)

type Log = [Item]

type Logging a = Writer Log a


-- ‘log s‘ logs the messages ‘s‘.
log :: Show t => t -> Logging ()
log = tell . singleton . Msg . show

-- ‘with_section s m‘ executes m and add its log in a section titled ‘s‘.
with_section :: String -> Logging a -> Logging a
-- with_section s m = do
--   (a, w) <- listen m
--   tell [Section s w] >> pure a
with_section s m = pass $ (, singleton . Section s) <$> m

