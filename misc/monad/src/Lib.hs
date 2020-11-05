module Lib where

-- ZipList
newtype ZL a = ZL { getZL :: [a] } deriving (Eq, Show)

-- Compose
newtype Cmp f g a = Cmp { getCmp :: f (g a) } deriving (Eq, Show)

-- Bin
data Bin a = Leaf a | Bin (Bin a) (Bin a) deriving (Eq, Show)

-- Rose
data Rose a = Rose [Rose a] | LeafRose a deriving (Eq, Show)


-- RoseL
data RoseL a = RoseL a [RoseL a] deriving (Eq, Show)

-- Reader

newtype Reader r a  = Reader {runReader :: r -> a} deriving (Functor)

-- State

newtype State s a = State {runState :: s -> (a, s)} deriving (Functor)

-- Writer

newtype Writer w a = Writer {runWriter :: (a, w)} deriving (Functor, Show)

-- Cont
newtype Cont r a = Cont {runCont :: (a -> r) -> r}

