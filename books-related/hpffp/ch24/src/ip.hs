module IPV4 where

import           Control.Monad
import qualified Data.Map      as M
import           Data.Word
import           Test.Hspec
import           Text.Trifecta
data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

t1 = "172.16.254.1"
e1 = 2886794753
t2 = "204.120.0.15"
e2 = 3430416399

parseOctec :: Integral a => Parser a
parseOctec = do
  x <- natural
  guard $ x >= 0 && x < 256
  return $ fromInteger x

parseIpv4 :: Parser Word32
parseIpv4 = do
  a <- parseOctec
  _ <- char '.'
  b <- parseOctec
  _ <- char '.'
  c <- parseOctec
  _ <- char '.'
  d <- parseOctec
  return (a*2^24 + b*2^16 + c*2^8 + d)

hexSymbols :: [Char]
hexSymbols = ['0'..'9']++['A'..'F']

hexMap :: Integral a => M.Map Char a
hexMap = M.fromList $ zip hexSymbols [0..]

parseHex :: Integral a => Parser a
parseHex = f <$> anyChar
  where
    f c = case M.lookup c hexMap of
            Just x  -> x
            Nothing -> error "Cannot parse"

parse p = parseString p mempty

main :: IO ()
main = hspec $ do
  describe "IPv4" $ do
    it ("should parse correctly " ++ t1) $ do
      case parse parseIpv4 t1 of
        Success x -> x `shouldBe` e1
        _         -> error "Parsing failed"
  describe "IPv4" $ do
    it ("should parse correctly " ++ t2) $ do
      case parse parseIpv4 t2 of
        Success x -> x `shouldBe` e2
        _         -> error "Parsing failed"
