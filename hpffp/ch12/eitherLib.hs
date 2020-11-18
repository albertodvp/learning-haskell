lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f x acc = case x of
      Left a  -> a:acc
      Right _ -> acc


rights' :: [Either a b] -> [b]
rights' = foldl f []
  where
    f acc x = case x of
      Right b -> b:acc
      Left _ -> acc


partitionsEithers' :: [Either a b] -> ([a], [b])
partitionsEithers' = foldr f ([], [])
  where
    f (Left  a) (as, bs) = (a:as, bs)
    f (Right b) (as, bs) = (as, b:bs)


eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' f e = case e of
  Left  a -> Nothing
  Right b -> Just $ f b


either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' fa fb e = case e of
  Left  a -> fa a
  Right b -> fb b


eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' fb = either' (const Nothing) (Just . fb)

