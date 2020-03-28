module AnimalFarm where

-- General types

data GuessWhat = ChickenButt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a, psecond :: b }
  deriving (Eq, Show)


-- Farm types

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
-- equivalent to
type Farmhouse' = Product NumCow NumPig

