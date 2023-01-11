{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyLib where

import           Test.QuickCheck


class MySemigroup a where
  (<~>) :: a -> a -> a


class MySemigroup a => MyMonoid a where
  mymempty :: a
  mymconcat :: [a] -> a
  mymconcat = foldr (<~>) mymempty


-- List

instance MySemigroup [a] where
  (<~>) = (++)

instance MyMonoid [a] where
  mymempty = []


-- MyNonEmpty

data MyNonEmpty a = a :| [a] deriving (Show, Eq)


instance MySemigroup (MyNonEmpty a) where
  (<~>) (a :| as) (b :| bs) = a :| (as ++ b : bs)


-- Trival
data Trival = Trival deriving (Eq, Show)

instance MySemigroup Trival where
  Trival <~> Trival = Trival

type TrivAssoc = Trival -> Trival -> Trival -> Bool

instance Arbitrary Trival where
  arbitrary = return Trival

instance MyMonoid Trival where
  mymempty = Trival

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance MySemigroup a => MySemigroup (Identity a) where
  Identity a <~> Identity b = Identity (a <~> b)

type IdentityTriv = Identity Trival
type IdentityAssoc = IdentityTriv -> IdentityTriv -> IdentityTriv -> Bool


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return  (Identity a)


instance MyMonoid a => MyMonoid (Identity a) where
  mymempty = Identity mymempty


-- Two
data Two a b = Two a b deriving (Eq, Show)

instance (MySemigroup a, MySemigroup b) => MySemigroup (Two a b) where
  (<~>) (Two a b) (Two a' b') = Two (a <~> a') (b <~> b')

type TwoTriv = Two Trival Trival
type TwoAssoc = TwoTriv -> TwoTriv -> TwoTriv -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (MyMonoid a, MyMonoid b) => MyMonoid (Two a b) where
  mymempty = Two mymempty mymempty

-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Arbitrary, Show)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance MySemigroup BoolConj where
  (<~>) (BoolConj b1) (BoolConj b2) = BoolConj (b1 && b2)

instance MyMonoid BoolConj where
  mymempty = BoolConj True


-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Arbitrary, Show)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance MySemigroup BoolDisj where
  (<~>) (BoolDisj b1) (BoolDisj b2) = BoolDisj (b1 || b2)

instance MyMonoid BoolDisj where
  mymempty = BoolDisj False

-- Or
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance MySemigroup (Or a b) where
  (<~>) (or) (or') = case or of
    Fst a -> Fst a
    Snd b -> case or' of
      Fst a' -> Fst a'
      Snd b' -> Snd b'

type OrTrivStr = Or Trival String
type OrAssoc = OrTrivStr -> OrTrivStr -> OrTrivStr -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (Fst a), return (Snd b)]

-- Combine
newtype MySum a = MySum {getSum :: a} deriving (Show, Eq)

instance Num a => MySemigroup (MySum a) where
  MySum a <~> MySum a' = MySum (a + a')

instance Num a => MyMonoid (MySum a) where
  mymempty = MySum 0

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance MySemigroup b => MySemigroup (Combine a b) where
  Combine f1 <~> Combine f2 = Combine f
    where
      f a = f1 a <~> f2 a

instance MyMonoid b => MyMonoid (Combine a b) where
  mymempty = Combine $ const mymempty

-- Validation


data Validation a b =
  MyFailure a | MySuccess b
  deriving (Eq, Show)

instance MySemigroup a => MySemigroup (Validation a b) where
  (<~>) (MyFailure a) (MyFailure a') = MyFailure (a <~> a')
  (<~>) (MyFailure a) s              = s
  (<~>) s@(MySuccess b) _            = s


-- Mem
newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance MySemigroup a => MySemigroup (Mem s a) where
  (<~>) (Mem f') (Mem f'') = Mem f
    where f s = (a' <~> a'', s'')
            where
              (a',  s' ) = f' s
              (a'', s'') = f'' s'


instance MyMonoid a => MyMonoid (Mem s a) where
  mymempty = Mem $ \s -> (mymempty, s)
