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

s :: Parser String
s = (string "1" >> eof) <|> (string "12" >> eof) <|> (string "123" >> eof)
