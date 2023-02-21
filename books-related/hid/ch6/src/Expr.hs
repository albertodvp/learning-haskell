-- |

module Expr where

import           TextShow

data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a) deriving (Show, Eq)

-- 1) [x] Deriving TextShow
-- 2) [x] Write tests
-- 3) [ ] Implement logic with parser comb

instance TextShow a => TextShow (Expr a) where
  showbPrec _ (Lit a)              = showb a
  showbPrec outerPrec (Add e1 e2)  = builderHelper "+" outerPrec 5 e1 e2
  showbPrec outerPrec (Mult e1 e2) = builderHelper "*" outerPrec 6 e1 e2

builderHelper :: TextShow a => Builder -> Int -> Int -> Expr a -> Expr a -> Builder
builderHelper op outP currP e1 e2 = showbParen (outP > currP) content
  where
    content :: Builder
    content = mconcat [showbPrec currP e1, op, showbPrec currP e2]

-- TODO
parse :: Num a => String -> Expr a
parse s = Lit 42
