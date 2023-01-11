-- 1)

notThe :: String -> Maybe String
notThe x = case x of
  "the" -> Nothing
  _     -> Just x

replaceThe :: String -> String
replaceThe = unwords . map conv . map notThe . words
  where
    conv :: Maybe String -> String
    conv x = case x of
      Nothing -> "a"
      Just x  -> x



-- 2)
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fst . foldl f (0, Just "") . map notThe . words
  where
    f :: (Integer, Maybe String) -> Maybe String -> (Integer, Maybe String)
    f (v, Nothing) curr@(Just (x:xs)) = if x `elem` "aeiou" then (v+1, curr) else (v, curr)
    f (v, _) curr  = (v, curr)



-- 3)
vowelHood :: Char -> Maybe Char
vowelHood c
  | c `elem` "aeiou"  = Just c
  | otherwise         = Nothing

countVowels :: String -> Integer
countVowels = foldr f 0 . map vowelHood
  where
    f :: (Maybe Char) -> Integer -> Integer
    f x acc = case x of
      Nothing -> acc
      Just _  -> acc + 1

