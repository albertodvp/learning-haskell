{-# LANGUAGE InstanceSigs #-}
-- |

module ComposeApp where


newtype Compose f g a = Compose { runCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgfab) <*> (Compose fga) = Compose $ fmap (<*>) fgfab <*> fga


instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap fam (Compose fga) = foldMap (foldMap fam) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse fhb cfga = hfgb
    where
      hfgb = sequence fhgb
      fhgb = fmap sequence fghb
      Compose fghb = fhb <$> cfga
