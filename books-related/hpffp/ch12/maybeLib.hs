isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False


isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a


fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a


fromMaybe' :: a -> Maybe a -> a
fromMaybe' a = mayybee a id


listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x


maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]


catMaybes :: [Maybe a] -> [a]
catMaybes []    = []
catMaybes (x:xs) = case x of
  Nothing -> catMaybes xs
  Just a  -> a : catMaybes xs


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if length cm == length xs then Just cm else Nothing
  where cm = catMaybes xs


