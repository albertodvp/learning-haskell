myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfold :: (b -> Maybe (a, b))
         -> b
         -> [a]

myUnfold f b = case f b of
  Just (a, b1) -> a : myUnfold f b1
  Nothing      -> []


betterIterate :: (a -> a)
              -> a
              -> [a]
betterIterate f = myUnfold f'
  where f' a'= Just (a', f a')


