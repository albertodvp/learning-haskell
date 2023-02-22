-- |

module CG where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text

-- Define our arithmetic expressions data type
data Expr = Add Expr Expr | Mul Expr Expr | Num Int deriving Show

-- Parse a single integer
parseNum :: Parser Expr
parseNum = Num <$> decimal

-- Parse an expression inside parentheses
parseParens :: Parser Expr
parseParens = char '(' *> parseExpr <* char ')'

-- Parse multiplication expressions
parseMul :: Parser Expr
parseMul = Mul <$> (parseNum <|> parseParens) <* char '*' <*> parseExpr

-- Parse addition expressions
parseAdd :: Parser Expr
parseAdd = Add <$> parseMul <*> (char '+' *> parseExpr)

-- Parse any arithmetic expression
parseExpr :: Parser Expr
parseExpr = parseAdd <|> parseMul <|> parseNum <|> parseParens

-- Example usage
main :: IO ()
main = do
  let exprString = "2*(3+4)+5*6"
  case parseOnly parseExpr exprString of
    Left err   -> putStrLn $ "Error parsing expression: " ++ err
    Right expr -> print expr
