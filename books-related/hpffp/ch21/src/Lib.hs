module Lib where

-- class (Functor t, Foldable t) => Traversable t where
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)


newtype Constant a b =
  Constant { getConstant :: a }

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
