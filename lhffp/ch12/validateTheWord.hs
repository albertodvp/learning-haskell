--import Data.Maybe

newtype Word' =
  Word' String deriving (Eq, Show)

vowels = "aeiou"

vowelCheck :: Char -> Maybe Char
vowelCheck c = if c `elem` vowels then Just c else Nothing


isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

mkWord :: String -> Maybe Word'
mkWord s = if fromIntegral (length s) / 2 > fromIntegral nv then Just (Word' s) else Nothing
  where
    nv = length $ filter isJust $ map vowelCheck s

  
