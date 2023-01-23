-- |

import qualified Data.Deque    as D
import           Data.List     (unfoldr)
import qualified Data.Stack    as S
import           System.TimeIt (timeItNamed)
fill n insert s = foldl (flip insert) s [1..n]

sumAll s view remove = sum $ unfoldr iter s
  where
    iter s = view s >>= \x -> Just (x, remove s)


main :: IO ()
main = do
  let n = 10^6
  timeItNamed "Stack" $
    print $ sumAll (fill n S.push S.empty) S.top S.pop
  timeItNamed "Deque" $
    print $ sumAll (fill n D.push_front D.empty) D.front D.pop_front

