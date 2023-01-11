{-# LANGUAGE DeriveFunctor #-}

module Lib where

import           Control.Applicative (liftA2)
-- ZipList
newtype ZL a = ZL { getZL :: [a] } deriving (Eq, Show)

-- Compose
newtype Cmp f g a = Cmp { getCmp :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Cmp f g) where
  fmap f (Cmp fga)  = Cmp $ fmap (fmap f) fga

instance (Applicative f, Applicative g) => Applicative (Cmp f g) where
  pure = Cmp . pure . pure
  (Cmp fgf) <*> (Cmp fga) = Cmp $  liftA2 (<*>) fgf fga

-- Bin
data Bin a = Leaf a | Bin (Bin a) (Bin a) deriving (Eq, Show)

instance Functor Bin where
  fmap f (Leaf a)    = Leaf $ f a
  fmap f (Bin ba bb) = Bin (f <$> ba) (f <$> bb)

instance Applicative Bin where
  pure = Leaf
  (Leaf f) <*> baa            = f <$> baa
  (Bin bf bg) <*> la@(Leaf _) = Bin (bf <*> la) (bg <*> la)
  (Bin bf bg) <*> (Bin ba bb) = Bin (bf <*> ba) (bg <*> bb)


-- BinL
data BinL a = BinL a (BinL a) (BinL a) | LeafL deriving (Eq, Show)

instance Functor BinL where
  fmap _ LeafL            = LeafL
  fmap f (BinL a blb blc) = BinL (f a) (f <$> blb) (f <$> blc)

instance Applicative BinL where
  pure a = BinL a LeafL LeafL
  LeafL <*> _ = LeafL
  _ <*> LeafL = LeafL
  BinL f LeafL LeafL <*> BinL a bla blb = BinL (f a) (f <$> bla) (f <$> blb)
  BinL f blf blg <*> BinL a LeafL LeafL = BinL (f a) (($ a) <$> blf) (($ a) <$> blg)
  BinL f blf LeafL <*> BinL a bla blb = BinL (f a) (blf <*> bla) (f <$> blb)
  BinL f LeafL blg <*> BinL a bla blb = BinL (f a) (f <$> bla) (blg <*> blb)
  BinL f blf blg <*> BinL a LeafL blb = BinL (f a) (blf <*> blb) (blg <*> blb)
  BinL f blf blg <*> BinL a bla LeafL = BinL (f a) (blf <*> bla) (blg <*> bla)
  BinL f blf blg <*> BinL a bla blb = BinL (f a) (blf <*> bla) (blg <*> blb)


-- Rose
data Rose a = Rose [Rose a] | LeafRose a deriving (Eq, Show)

instance Functor Rose where
  fmap f (LeafRose a) = LeafRose $ f a
  fmap f (Rose xs)    = Rose $ (f <$>) <$>  xs

instance Applicative Rose where
  pure = LeafRose
  (LeafRose f) <*> rs = f <$> rs
  Rose rfs <*> rs     = Rose $ (<*> rs) <$> rfs


-- RoseL
data RoseL a = RoseL a [RoseL a] deriving (Eq, Show)

instance Functor RoseL where
  fmap f (RoseL a []) = RoseL (f a) []
  fmap f (RoseL a rs) = RoseL (f a) (fmap (fmap f) rs)

instance Applicative RoseL where
  pure a = RoseL a []
  RoseL f rfs <*> r@(RoseL x rxs) = RoseL (f x) $ ((f <$>) <$> rxs) <> ((<*> r) <$> rfs)

-- Reader

newtype Reader r a  = Reader {runReader :: r -> a} deriving (Functor)

instance Applicative (Reader r) where
  pure = Reader . const
  Reader rf <*> Reader rx = Reader $ rf <*> rx


-- State

newtype State s a = State {runState :: s -> (a, s)} deriving (Functor)

instance Applicative (State s) where
  pure = State . (,)
  State sf <*> State sa  = State (\s -> let (f, s') = sf s in
                                        let (a, s'') = sa s' in
                                        (f a, s''))

-- -- Writer

newtype Writer w a = Writer {runWriter :: (a, w)} deriving (Functor, Show)

instance Monoid w => Applicative (Writer w) where
  pure = Writer . flip (,) mempty
  Writer (f, w) <*> Writer (x, w') = Writer (f x, w <> w')






-- Cont
newtype Cont r a = Cont {runCont :: (a -> r) -> r}
