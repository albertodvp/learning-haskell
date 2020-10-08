module Lib where

-- ZipList
newtype ZL a = ZL { getZL :: [a] } deriving (Eq, Show)

-- Compose
newtype Cmp f g a = Cmp { getCmp :: f (g a) } deriving (Eq, Show)

-- Bin
data Bin a = Leaf a | Bin (Bin a) (Bin a) deriving (Eq, Show)

-- BinL
data BinL a = BinL a (BinL a) (BinL a) | LeafL deriving (Eq, Show)

-- Rose
data Rose a = Rose [Rose a] | LeafRose a deriving (Eq, Show)

-- RoseL
data RoseL a = RoseL a [RoseL a] deriving (Eq, Show)


-- (*>) and (<*)
