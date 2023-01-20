-- |

module TestStack where

import           Data.Stack

main = do
  let st = push 15 $ push 10 $ push 5 $ push 0 $ empty
      st' = pop $ pop st
      st'' = push 100 st'
      shouldBeTrue = [top st' == Just 5,
                      top st'' == Just 100,
                      isEmpty $ pop $ pop st']
  print $ and shouldBeTrue

