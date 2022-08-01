-- | n

module LiftMore where
import           Control.Monad                  (liftM)
import           Control.Monad.Trans.State.Lazy
-- import Control.Monad.Trans.Class
import           EitherT
class MonadTrans t where
  lift :: Monad m => m a -> t m a


instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right


instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)
