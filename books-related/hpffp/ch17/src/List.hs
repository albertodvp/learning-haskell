module List where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

append :: List a -> List a -> List a
append Nil xs         = xs
append (Cons x xs) ys = Cons x  $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil         = b
fold f b (Cons x xs) = f x $ fold f b xs

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f = concat' . fmap f

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' i (Cons x xs) = Cons x $ take' (i-1) xs

repeat' :: a -> List a
repeat' a = Cons a $ repeat' a

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) $ zipWith' f xs ys
zipWith' _ Nil _                   = Nil
zipWith' _ _ Nil                   = Nil


instance Applicative List where
  pure = flip Cons Nil
  (<*>) fs xs = flatMap (<$> xs) fs

instance Semigroup a => Semigroup (List a) where
  (<>) = append

instance Monoid a => Monoid (List a) where
  mempty = Nil
