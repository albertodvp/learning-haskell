data Nat = Zero | Succ Nat deriving Show

natToInteger :: Nat -> Integer
natToInteger n = case n of
  Zero     -> 0 
  Succ nat -> 1 + natToInteger nat 


integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0     = Nothing
  | otherwise = Just (go i)
  where
    go x = case x of
      0 -> Zero
      _ -> Succ (go (x - 1))
