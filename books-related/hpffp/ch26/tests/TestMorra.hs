-- |
module TestMorra where

import           Control.Monad
import           Morra
import           Test.HUnit

g = undefined

type GameFinCase = ((Int, Score), Bool)
-- isGameFinished :: PointsToWin -> Score -> Bool
gameFinCases :: [GameFinCase]
gameFinCases = [
  ((0, Score 0 0 g), True),
  ((1, Score 0 0 g), False),
  ((1, Score 0 1 g), True),
  ((1, Score 1 1 g), True),
  ((1, Score 1 0 g), True),
  ((5, Score 1 0 g), False),
  ((5, Score 3 3 g), False),
  ((5, Score 5 3 g), True),
  ((5, Score 3 6 g), True)
  ]

mkAssertion :: GameFinCase -> Assertion
mkAssertion (funcArgs, bool) = assert (uncurry isGameFinished funcArgs == bool)

tests :: [Test]
tests = TestCase . mkAssertion <$> gameFinCases


main :: IO ()
main = void $ runTestTT (TestList tests)

