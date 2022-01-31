-- |

module IdentityT where


newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ f <$> fa

instance (Applicative m) => Applicative (IdentityT m) where
  pure = IdentityT . pure
  IdentityT ffab <*> IdentityT fa = IdentityT $ ffab <*> fa

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT fa)  >>= faIb = IdentityT $ fa >>= runIdentityT . faIb

