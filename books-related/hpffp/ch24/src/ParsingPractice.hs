-- |

module ParsingPractice where

import           Control.Applicative
import           Text.Parser.Combinators
import           Text.Trifecta
stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

one'' :: Parser ()
one'' = one >> eof

oneTwo'' :: Parser ()
oneTwo'' = oneTwo >> eof

p123 :: Parser [Char]
p123 = (string "123" <|> string "12" <|> string "1") <* eof


stringFromCharP :: CharParsing m => String -> m String
stringFromCharP (x:xs) = liftA2 (:) (char x) (stringFromCharP xs)
stringFromCharP []     = pure mempty

stringFromCharP' :: String -> Parser String
stringFromCharP' = foldr f (pure mempty)
  where
    f c ps = liftA2 (:) (char c) ps

