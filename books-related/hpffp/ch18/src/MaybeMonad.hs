module MaybeMonad where

data Cow = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
 } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str


noNegative :: Int -> Maybe Int
noNegative x
  | x < 0     = Nothing
  | otherwise = Just x


weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
       then Nothing
       else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing    -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing   -> Nothing
        Just agey -> case noNegative weight' of
          Nothing      -> Nothing
          Just weighty ->
            weightCheck (Cow nammy agey weighty)


mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = noEmpty name' >> noNegative age' >> noNegative weight' >> weightCheck (Cow name' age' weight')

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = do
  n <- noEmpty name'
  a <- noNegative age'
  w <- noNegative weight'
  weightCheck $ Cow n a w

