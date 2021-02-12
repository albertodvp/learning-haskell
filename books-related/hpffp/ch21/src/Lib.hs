module Lib where


import           Control.Applicative (liftA2, liftA3)
-- class (Functor t, Foldable t) => Traversable t where
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- sequenceA = traverse id
-- traverse f = sequenceA . fmap f

-- Identity
newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldr f b (Identity a) = f a b
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse gafb (Identity a) = Identity <$> gafb a
  sequenceA (Identity fa) = Identity <$> fa

-- Constant
newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ b (Constant a) = b
  foldMap f (Constant a) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a
  sequenceA (Constant a) = pure $ Constant a

-- Maybe
data Optional a =
    Nada
  | Yep a deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ b Nada    = b
  foldr f b (Yep a) = f a b
  foldMap _ Nada      = mempty
  foldMap fam (Yep a) = fam a


instance Traversable Optional where
  sequenceA Nada     = pure Nada
  sequenceA (Yep fa) = Yep <$> fa
  traverse _ Nada       = pure Nada
  traverse gafb (Yep a) = Yep <$> gafb a


-- List
data List a =
    Nil
  | Cons a (List a) deriving (Eq, Show)


instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Foldable List where
  foldMap f Nil         = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs
  foldr _ b Nil         = b
  foldr f b (Cons x xs) = f x (foldr f b xs)

instance Traversable List where
  traverse f Nil         = pure Nil
  traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)
  sequenceA Nil         = pure Nil
  sequenceA (Cons x xs) = liftA2 Cons x $ sequenceA xs

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c
  sequenceA (Three a b c) = Three a b <$> c

-- Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldr f c (Pair a b) = f b c

instance Traversable (Pair a) where
  sequenceA (Pair a b) = Pair a <$> b
  traverse f (Pair a b) = Pair a <$> f b


-- Big
data Big a b =
  Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldr f c (Big a b b') = f b (f b' c)
  foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = liftA2 (Big a) (f b) (f b')
  sequenceA (Big a b b') = liftA2 (Big a) b b'

-- TODO
-- Bigger
data Bigger a b =
  Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = mconcat $ f <$> [b, b', b'']

instance Traversable (Bigger a) where
  sequenceA (Bigger a b b' b'') = liftA3 (Bigger a) b b' b''

-- S
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  -- foldr f b (S na a) = f a (foldr f b na) -- TODO: this only (without foldMap, is not working. Didn't understand why...
  foldMap f (S na a) = foldMap f na <> f a
  -- TODO foldmap

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = liftA2 S (traverse f na) (f a)

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf a)     = Leaf $ f a
  fmap f (Node x a y) = Node (f <$> x) (f a) (f <$> y)

-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr, too,
-- for extra credit.
instance Foldable Tree where
  -- TODO: this only (without foldMap, is not working. Didn't understand why...
  foldMap _ Empty        = mempty
  foldMap f (Leaf a)     = f a
  foldMap f (Node x a y) = mconcat [foldMap f x, f a , foldMap f y]
  -- foldr _ b Empty        = b
  -- foldr f b (Leaf a)     = f a b
  -- foldr f b (Node x a y) = f a $ foldr f (foldr f b y) x

instance Traversable Tree where
  traverse f Empty        = pure Empty
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Node x a y) = liftA3 Node (traverse f x) (f a) (traverse f y)
