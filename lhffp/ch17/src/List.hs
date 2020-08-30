module List where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)


append :: List a -> List a -> List a
append Nil xs = xs
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


instance Applicative List where
  pure = flip Cons Nil
  --  (<*>) :: List (a -> b) -> List a -> List b
-- 0
--  (<*>) (Cons f fs) cons = (f <$> cons) `append` (fs <*> cons)
--  (<*>) Nil _ = Nil

-- 1
--  (<*>) fs xs = concat' $ fmap (`fmap` xs) fs

-- 2 -- !!! I AM CHANING THE ORDER HERE 2 TO 4 ARE WRONG
--  (<*>) fs xs = concat' $ fmap (\x -> flip fmap fs ($ x)) xs

-- 3 
--  (<*>) fs = concat' . fmap (flip fmap fs . flip ($))

-- 4 
--  (<*>) fs = flatMap $ (`fmap` fs) . flip ($)


-- 5
  -- fold :: (a -> b) -> b -> List a -> b
  -- flatMap :: (a -> List b) -> List a -> List b
  (<*>) fs xs = flatMap fapp fs
    where
      fapp f = fold (Cons . f) Nil xs
