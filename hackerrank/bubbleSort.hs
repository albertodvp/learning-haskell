step :: Ord a => [a] -> [a]
step [] = []
step [x] = [x]
step (x:xs)
  | x > head xs = head xs : step (x:tail xs)
  | otherwise = x:step xs

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = foldl (\acc _ -> step acc) xs xs
l = [2,3, 1, 0, -14, 3, 1, 45, 6, 4, 2, 2, 10, 4, -34, 3, 1]

-- >>> bubbleSort l
-- [-34,-14,0,1,1,1,2,2,2,3,3,3,4,4,6,10,45]


