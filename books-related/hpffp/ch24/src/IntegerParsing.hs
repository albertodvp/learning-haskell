-- |

module IntegerParsing where
import           Control.Applicative
import           Text.Trifecta
parseDigit :: Parser Char
parseDigit = oneOf "1234567890"

base10Integer :: Parser Integer
base10Integer = read <$> many parseDigit

base10Integer' :: Parser Integer
base10Integer' = read <$> (try (liftA2 (:) (char '-') (many parseDigit)) <|> many parseDigit)

