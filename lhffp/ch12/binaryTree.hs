data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b

unfold f a = case f a of
  Just (a',b',a'') -> Node (unfold f a') b' (unfold f a'')
  Nothing          -> Leaf


treeBuild :: Integer -> BinaryTree Integer
treeBuild i = unfold f 0
  where
    f x
      | x < i    = Just (x+1, x, x+1)
      | otherwise = Nothing
