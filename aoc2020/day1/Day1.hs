import           Control.Applicative (liftA2)
import           Control.Monad       (guard, sequence)
import           Criterion.Main
import           Data.Foldable       (foldMap)
import           Data.Functor        ((<&>))
import qualified Data.HashTable.IO   as H
import qualified Data.IntSet         as IntSet
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes, isJust)
import           Data.Monoid

type Input = [Int]
type Resolver = Input -> IO Int
type Target = Int
type HashTable = H.CuckooHashTable Int (Int, Int)


--
-- PROBLEM 1
--
-- Correct answer: 440979
-- V1
-- Time complexity: O(n) = n^2
findRes1 :: Target -> Resolver
findRes1 t xs = pure $ head $ do
  x <- xs
  y <- xs
  guard $ x + y == t
  guard $ x /= y
  pure $ x*y

-- V2
-- Time complexity: O(n*min(m,W))  with lenght xs = n, lenght diffs = m, W = the number of bits of an Int
findRes1Fast :: Target -> Resolver
findRes1Fast t xs = pure $ x * (t - x)
  where
    x = head $ filter (`IntSet.member` diffs) xs -- O(1) + O(n*min(m,W))
    diffs = IntSet.fromList $ (t -) <$> xs -- O(n*min(n,W)) + O(n)

--
-- PROBLEM 2
--
-- Correct answer: 82498112
-- Time complexity: O(n) = n^3
findRes2 :: Target -> Resolver
findRes2 t xs = pure $ head $ do
  x <- xs
  y <- xs
  z <- xs
  guard $ x + y + z == t
  guard $ x /= y
  guard $ y /= z
  guard $ z /= x
  pure $ x*y*z

-- Time complexity: O(n^2) (I expected)
findRes2Fast :: Target -> Resolver
findRes2Fast t xs = fmap (head . catMaybes) $ sequence $ do
      x <- xs -- O(n^2)
      pure $ do
        ht <- iht
        myz <- H.lookup ht x -- O(1) https://hackage.haskell.org/package/hashtables-1.2.4.1/docs/Data-HashTable-Class.html#v:lookup
        pure $ do
          yz <- myz
          pure $ x * (t - fst yz) * snd yz

  where
    iht = H.fromList diffs :: IO HashTable  -- O(n)
    diffs = liftA2 subSave partialDiffs xs -- O(n^2)
    partialDiffs = (t -) <$> xs -- O(n)
    subSave x y = (x - y, (x,y))

-- TODO generalize a bit
resolvers :: Target -> Map.Map (String, String) Resolver
resolvers t = Map.fromList
  [ (("day1.1","fast"), findRes1Fast t)
  , (("day1.2", "fast"), findRes2Fast t)
  , (("day1.1","base"), findRes1 t)
  , (("day1.2","base"), findRes2 t)
  ]

inputFile :: FilePath
inputFile = "./input"

inputs :: IO Input
inputs = fmap read <$> (lines <$> readFile inputFile)

results :: IO (Map.Map (String, String) Int)
results = inputs >>= sequence . (<$> resolvers 2020) . flip ($)

main :: IO ()
main = defaultMain $ uncurry bgroup <$> group (uncurry benches <$> Map.toList (whnfIO . (inputs >>=) <$> resolvers 2020))
  where
    benches (groupName, name) ble = (groupName, bench name ble)
    groupUpdate (groupName, bench) m = Map.insertWith (++) groupName [bench] m
    group = Map.toList . foldr groupUpdate Map.empty

