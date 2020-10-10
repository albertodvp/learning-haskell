module Lib where

-- ZipList
newtype ZL a = ZL { getZL :: [a] } deriving (Eq, Show)

-- Compose
newtype Cmp f g a = Cmp { getCmp :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Cmp f g) where
  fmap f (Cmp fga)  = Cmp $ fmap (fmap f) fga

instance (Applicative f, Applicative g) => Applicative (Cmp f g) where
  pure = Cmp . pure . pure
--  (Cmp fgf) <*> (Cmp fga)  = Cmp $ (fmap . fmap) fgf fga
  (Cmp fgf) <*> (Cmp fga) = undefined
-- Bin
data Bin a = Leaf a | Bin (Bin a) (Bin a) deriving (Eq, Show)

instance Functor Bin where
  fmap f (Leaf a)    = Leaf $ f a
  fmap f (Bin ba bb) = Bin (f <$> ba) (f <$> bb) 

instance Applicative Bin where
  pure = Leaf
  (Leaf f) <*> (Leaf a) = Leaf $ f a
  (Leaf f) <*> baa = f <$> baa
  (Bin bf bg) <*> (Bin ba bb) = Bin (bf <*> ba) (bg <*> bb)
  (Bin bf bg) <*> (Leaf a) = Bin (bf <*> Leaf a) (bg <*> Leaf a)
  
-- BinL
data BinL a = BinL a (BinL a) (BinL a) | LeafL deriving (Eq, Show)

instance Functor BinL where
  fmap _ LeafL = LeafL
  fmap f (BinL a blb blc) = BinL (f a) (f <$> blb) (f <$> blc)

instance Applicative BinL where
  pure a = BinL a LeafL LeafL
  (BinL f blf blg) <*> (BinL a bla blb) = BinL (f a) (blf <*> bla) (blg <*> blb)
  _ <*> _ = LeafL
  

-- Rose
data Rose a = Rose [Rose a] | LeafRose a deriving (Eq, Show)

-- RoseL
data RoseL a = RoseL a [RoseL a] deriving (Eq, Show)


-- (*>) and (<*)
