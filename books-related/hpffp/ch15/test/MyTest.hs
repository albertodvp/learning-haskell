module Main where

import           Test.QuickCheck

import           MyLib

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Testing MySemigroup..."
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  putStrLn "Testing MyMonoid..."
  quickCheck (monoidRightIdentity :: String -> Bool)
  quickCheck (monoidLeftIdentity :: String -> Bool)
  quickCheck (monoidRightIdentity :: Trival -> Bool)
  quickCheck (monoidLeftIdentity :: Trival -> Bool)
  quickCheck (monoidRightIdentity :: Two String Trival -> Bool)
  quickCheck (monoidLeftIdentity :: Two String Trival  -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj  -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj  -> Bool)



semigroupAssoc :: (Eq m, MySemigroup m) => m -> m -> m -> Bool

semigroupAssoc a b c =
  (a <~> (b <~> c)) == ((a <~> b) <~> c)


monoidRightIdentity :: (Eq m, MyMonoid m) => m -> Bool
monoidRightIdentity a =
  (a <~> mymempty) == a

monoidLeftIdentity :: (Eq m, MyMonoid m) => m -> Bool
monoidLeftIdentity a =
  (mymempty <~> a) == a

