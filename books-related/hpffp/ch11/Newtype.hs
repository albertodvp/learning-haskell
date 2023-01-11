{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42


newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)


-- put this on top
--{-# LANGUAGE FlexibleInstances #-}
--instance TooMany (Int, String) where
--  tooMany = tooMany . fst


-- or

newtype IntString =
  IntString (Int, String) deriving Show

instance TooMany IntString where
  tooMany (IntString is)= tooMany (fst is)


instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x+y)
