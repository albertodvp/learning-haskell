-- NOTE: this exercise was proposed in an event organized by the study group
--       of the [Haskell Milano Meetup](https://www.meetup.com/haskell-milano/)

{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Stems(module Stems) where

import           Protolude hiding (empty)

import qualified Prelude

import           Data.Text (dropAround, toLower, unpack)

import qualified Data.Map  as M

newtype Stem a = Stem {unStem :: [a]} deriving (Foldable, Show, Eq)

type Parser = Parsac Void [Char]

--
cleanLine :: Text -> [Char]
cleanLine = go . toS . Data.Text.toLower
  where
    go [] = []
    go (x:xs)
      | x `elem` ".:,'()&!?;" = ' ':go xs
      | otherwise = x:go xs


-- TODO toS vs unpack?
getStems :: Text -> [Stem Char]
getStems = map (Stem . Prelude.head) . group . sort . concatMap Prelude.words . filter (not . null) . map cleanAndTrim . lines
  where
    cleanAndTrim = cleanLine . dropAround (not . isLetter)

-- complementary
comp :: Eq a => Stem a -> Stem a -> Maybe (Stem a)
comp (Stem ys) (Stem xs)
  | ys `isPrefixOf` xs = Just $ Stem $ drop (length ys) xs
  | otherwise = Nothing

superStem :: Eq a => Stem a -> Stem a -> Bool
superStem y = isJust . comp y

dist :: Eq a => Stem a -> Stem a -> Maybe Int
dist y = fmap length . comp y

newtype Population a = Population {unPopulation :: [Stem a]} deriving (Foldable, Show, Eq)


-- TODO: isn't this a "sups"?
subs :: Eq a => Stem a -> Population a -> Population a
subs x (Population xs) = Population $ filter (superStem x) xs

size :: Population a -> Int
size = length

value :: Eq a => Population a -> Stem a -> Stem a -> Maybe Int
value xs y x = (size (subs x xs) *) <$> dist y x


-- TODO this implementation computes the 'n' less valuable
-- | given a population and one stem in it computes the (at-most) 'n' most valuable
-- super stems
query :: Eq a =>
  -- | maximum number of super stems
  Int ->
  -- | stem population
  Population a ->
  -- | base stem
  Stem a ->
  -- | super stems
  [Stem a]
query n pop base = take n $ sortOn (fromMaybe 0 . value pop base) (unPopulation $ subs base pop)


-- TODO: refine the value function so that query returns something better :-)
-- (in case you do not like it)


-- TODO: define population as a trie usind Data.Map
-- so that values are computed at trie construction time

type ToBeImplemented = Void

newtype PopulationTrie a = PopulationTrie (M.Map a (PopulationTrie a)) deriving (Eq, Show)

instance Foldable PopulationTrie where
  foldMap f (PopulationTrie n)
    | M.null n = mempty
    | otherwise = Prelude.foldr g mempty (M.toList n)
    where
      g (a, p) m = f a <> foldMap f p <> m

-- TODO: instance Foldable PopulationTrie where
-- TODO: instance Semigroup PopulationTrie where
-- TODO: instance Monoid PopulationTrie where
-- TODO: rewrite insert

empty :: PopulationTrie a
empty =  PopulationTrie M.empty

singleton :: Ord a => a -> PopulationTrie a
singleton a = PopulationTrie $ M.fromList [(a, Stems.empty)]

-- | primitive operation to update a population trie with a new stem
insert :: Ord a => Stem a -> PopulationTrie a -> PopulationTrie a
insert (Stem []) pt = pt
insert (Stem (x:xs)) (PopulationTrie prevM) = PopulationTrie newM
  where
    prevSubM = M.findWithDefault empty x prevM
    newSubM = insert (Stem xs) prevSubM
    newM = M.insert x newSubM prevM

mkTrie :: Ord a => Population a -> PopulationTrie a
mkTrie = foldr insert empty . unPopulation

-- TODO understand why this duplication exists?
unTrie :: Ord a => Stem a -> PopulationTrie a -> [Stem a]
unTrie (Stem base) p = map (Stem . (base ++). unStem . Prelude.head) $ group $ sortOn unStem $ go (Stem []) p []
  where
    go :: Stem a -> PopulationTrie a -> [Stem a] -> [Stem a]
    go base' (PopulationTrie m) acc
      | M.null m = acc
      | otherwise = foldr (g base' acc) [] (M.toList m)
    g :: Stem a -> [Stem a] -> (a, PopulationTrie a) -> [Stem a] -> [Stem a]
    g (Stem base') acc (a, q) ss = let newBase = Stem $ base'++[a]
                                  in go newBase q (newBase:ss ++ acc)

unTrie' :: Ord a => PopulationTrie a -> [Stem a]
unTrie' = unTrie (Stem [])

navigate :: Ord a => Stem a -> PopulationTrie a -> PopulationTrie a
navigate (Stem []) p = p
navigate (Stem (x:xs)) (PopulationTrie m) = case M.lookup x m of
  Just trie -> navigate (Stem xs) trie
  Nothing   -> empty


-- | select the trie of the given stem and traverse all children to
-- rebuild the 'n' best super-stems
queryTrie :: Ord a =>
  -- | maximum number of super stems
  Int ->
  -- | stem population
  PopulationTrie a ->
  -- | base ste
  Stem a ->
  -- | super stems
  [Stem a]
-- TODO user value
queryTrie num pop base = take num $ unTrie base $ navigate base pop

-- TODO: prove that is possible/impossible to prune the search based on values only
-- and that is unnecessary/necessary to store some other auxiliary information at
-- each node

-- TODO: optimize the population trie by hardcoding 'n' and tracking some
-- auxiliary information in the nodes to then traverse only the necessary subtries
-- to collect the best super-stems of a node

newtype PopulationTrieO a = PopulationTrieO ToBeImplemented

-- | primitive operation to update a population trie with a new stem
insertO :: Stem a -> PopulationTrieO a -> PopulationTrieO a
insertO = notImplemented

-- | select the trie of the given stem and traverse only necessary children to
-- rebuild the 'n' best super-stems
queryTrieO ::
  -- | stem population
  PopulationTrieO a ->
  -- | base ste
  Stem a ->
  -- | super stems
  [Stem a]
queryTrieO = notImplemented

-- TODO: write main so that we can add files to the population interactevely as
-- we perform searches


-- TODO: improve the trie so that it returns also matches for infixeses
