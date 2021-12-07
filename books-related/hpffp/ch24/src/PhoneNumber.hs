{-# LANGUAGE OverloadedStrings #-}

module PhoneNumber where
import           Control.Applicative
import           Control.Monad
import           Test.Hspec
import           Text.Trifecta
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

parse2digit :: Parser Int
parse2digit = do
  x <- digit
  y <- digit
  return $ read [x,y]

step :: Char -> Parser [Char]
step x = (x:) . pure <$> digit

step' :: [Char] -> Parser [Char]
step' xs = (xs ++) . pure <$> digit

parse2digit' :: Parser Int
parse2digit' = read <$> (digit >>= step)

parse3digit :: Parser Int
parse3digit = read <$> (digit >>= step >>= step')

parse4digit :: Parser Int
parse4digit = read <$> (digit >>= step >>= step' >>= step')

parseNDigit :: Int -> Parser Int
parseNDigit n = read <$> replicateM n digit


parseNPA :: Parser NumberingPlanArea
parseNPA =     try (between (char '(') (char ')') (parseNDigit 3))
           <|> try (string "1-") *> parseNDigit 3
           <|> parseNDigit 3
sep :: Parser Char
sep = oneOf " -"

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  npa <- parse3digit
  skipOptional $ char '-'
  e <- parse3digit
  skipOptional $ char '-'
  PhoneNumber npa e <$> parse4digit

parsePhone' :: Parser PhoneNumber
parsePhone' = liftA3
                PhoneNumber
                (parseNPA <* skipOptional sep)
                (parseNDigit 3 <* skipOptional sep)
                (parseNDigit 4)


succ2Maybe :: Result a -> Maybe a
succ2Maybe (Success a) = Just a
succ2Maybe _           = Nothing

parseBS :: String -> Result PhoneNumber
parseBS = parseString parsePhone' mempty

main :: IO ()
main = hspec $ do
  describe "Parse phone number" $ do
    it "parses no separator" $ do
      let r = succ2Maybe $ parseBS "1234567890"
          expected = Just $ PhoneNumber 123 456 7890
      r `shouldBe` expected
    it "parses dash separated" $ do
      let r = succ2Maybe $ parseBS "123-456-7890"
          expected = Just $ PhoneNumber 123 456 7890
      r `shouldBe` expected
    it "parses space separated" $ do
      let r = succ2Maybe $ parseBS "123 456 7890"
          expected = Just $ PhoneNumber 123 456 7890
      r `shouldBe` expected
    it "parses brackets" $ do
      let r = succ2Maybe $ parseBS "(123) 456-7890"
          expected = Just $ PhoneNumber 123 456 7890
      r `shouldBe` expected
    it "parses prefix" $ do
      let r = succ2Maybe $ parseBS "1-123-456-7890"
          expected = Just $ PhoneNumber 123 456 7890
      r `shouldBe` expected




