module BubbleSort where


foldLeftF :: Ord a => ([a], Integer) -> a -> ([a], Integer)
foldLeftF ([], 0) x = ([x], 0)
foldLeftF (l, s) x = if t>x then (reverse (t:x:tail (reverse l)), s+1) else (reverse (x:(reverse l)), s)
  where t = head $ reverse l


sortOnce :: Ord a => [a]-> ([a], Integer)
sortOnce = foldl foldLeftF ([], 0)

l = [11,1,3,2,4,5,10]

bubbleFoldF :: Ord a => ([a], Integer) -> Integer -> ([a], Integer)
bubbleFoldF (acc, s) _ = (a, s+ss)
  where
    (a,ss) = sortOnce acc
bubbleSort :: Ord a => [a] -> ([a], Integer)
bubbleSort l = foldl bubbleFoldF (l,0) [1..fromIntegral (length l)]
