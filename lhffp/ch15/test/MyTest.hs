{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.QuickCheck

import MyLib

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)

semigroupAssoc :: (Eq m, MySemigroup m) => m -> m -> m -> Bool

semigroupAssoc a b c =
  (a <~> (b <~> c)) == ((a <~> b) <~> c)


type TrivAssoc = Trival -> Trival -> Trival -> Bool
type IdentityTriv = Identity Trival
type IdentityAssoc = IdentityTriv -> IdentityTriv -> IdentityTriv -> Bool

type TwoTriv = Two Trival Trival
type TwoAssoc = TwoTriv -> TwoTriv -> TwoTriv -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type OrTrivStr = Or Trival String
type OrAssoc = OrTrivStr -> OrTrivStr -> OrTrivStr -> Bool

-- Trival
data Trival = Trival deriving (Eq, Show)

instance MySemigroup Trival where
  Trival <~> Trival = Trival

instance Arbitrary Trival where
  arbitrary = return Trival

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance MySemigroup a => MySemigroup (Identity a) where
  Identity a <~> Identity b = Identity (a <~> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return  (Identity a)

-- Two
data Two a b = Two a b deriving (Eq, Show)

instance (MySemigroup a, MySemigroup b) => MySemigroup (Two a b) where
  (<~>) (Two a b) (Two a' b') = Two (a <~> a') (b <~> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- BoolCoj
newtype BoolConj = BoolConj Bool deriving (Eq, Arbitrary, Show)

instance MySemigroup BoolConj where
  (<~>) (BoolConj b1) (BoolConj b2) = BoolConj (b1 || b2)
  
-- Or
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance MySemigroup (Or a b) where
  (<~>) (or) (or') = case or of
    Fst a -> Fst a
    Snd b -> case or' of
      Fst a' -> Fst a'
      Snd b' -> Snd b'
    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (Fst a), return (Snd b)]

-- Combine
newtype MySum a = MySum {getSum :: a} deriving (Show, Eq)
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance MySemigroup b => MySemigroup (Combine a b) where
  (<~>) (Combine f1) (Combine f2) = Combine f
    where f x = f1 x <~> f2 x
