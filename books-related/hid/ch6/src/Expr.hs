module Expr where


import           Control.Applicative  ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T

import           TextShow
data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a) deriving (Show, Eq)
type DoubleExpr = Expr Double
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


mayBetween :: T.Text -> T.Text -> A.Parser a -> A.Parser a
mayBetween b e p = between <|> p
  where
    between = A.string b *> p <* A.string e



trimmedParser :: A.Parser a -> A.Parser a
trimmedParser p = A.skipMany A.space *> p <* A.skipMany A.space

trimmedDoubleParser :: A.Parser Double
trimmedDoubleParser = trimmedParser A.double

parseLit :: A.Parser DoubleExpr
parseLit = Lit <$> trimmedDoubleParser

parseOp :: Char -> (DoubleExpr -> DoubleExpr -> DoubleExpr) -> A.Parser DoubleExpr
parseOp op opConst = opConst <$> parseExp' <*> (A.char op *> parseExp')

parseAdd :: A.Parser DoubleExpr
parseAdd = parseOp '+' Add

parseMult :: A.Parser DoubleExpr
parseMult = parseOp '*' Mult

parseExp :: A.Parser DoubleExpr
parseExp = parseLit parseAdd <|> parseMult <|> parseLit

parseExp' :: A.Parser DoubleExpr
parseExp' = parseLit <|> parseExp


-- TODO
parseE :: Num a => String -> Expr a
parseE s = Lit 42
