module MyLib where

data MyNonEmpty a = a :| [a]

class MySemigroup a where
  (<~>) :: a -> a -> a
  sconcat :: MyNonEmpty a -> a
  stimes :: Integral b => b -> a -> a
  {-# MINIMAL (<~>) #-}

instance MySemigroup ([] a) where
  (<~>) = (++)

instance MySemigroup (MyNonEmpty a) where
  (<~>) (a :| as) (b :| bs) = a :| (concat [as, [b], bs])
