module Main where

import           Test.QuickCheck

import           Lib

main :: IO ()
main = do
  putStrLn "\nTesting Identity a..."
  quickCheck (functorIdentity :: Identity [Char] -> Bool)
  quickCheck (functorCompose :: Fun Int Int -> Fun Int Int -> Identity Int -> Bool)

  putStrLn "\nTesting Pair a..."
  quickCheck (functorIdentity :: Pair String -> Bool)
  quickCheck (functorCompose :: Fun String Int -> Fun Int Char -> Pair String -> Bool)

  putStrLn "\nTesting Two a b..."
  quickCheck (functorIdentity :: Two String Int -> Bool)
  quickCheck (functorCompose :: Fun String Int -> Fun Int Char -> Two Bool String -> Bool)

  putStrLn "\nTesting Four a b c d..."
  quickCheck (functorIdentity :: Four String String String Int -> Bool)
  quickCheck (functorCompose :: Fun String Int -> Fun Int Char -> Four String Int Bool String -> Bool)

  putStrLn "\nTesting Quant a b..."
  quickCheck (functorIdentity :: Quant String Int -> Bool)
  quickCheck (functorCompose :: Fun Int String -> Fun String Char -> Quant Bool Int -> Bool)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x = fmap (g . f) x == fmap g (fmap f x)

