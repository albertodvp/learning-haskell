module Lib where

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
  | Const a (List a)

-- Three
data Three a b c = Three a b c

-- Pair
data Pair a b = Pair a b

-- Big
data Big a b =
  Big a b b

-- Bigger
data Bigger a b =
  Bigger a b b b


-- S
data S n a = S (n a) a deriving (Eq, Show)



data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap = undefined

-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr, too,
-- for extra credit.
instance Foldable Tree where
  foldMap = undefined
instance Traversable Tree where
  traverse = undefined
