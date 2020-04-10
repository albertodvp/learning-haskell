module RewritingWithFolds where

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (x:xs) = f x (myFoldr f b xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ b [] = b
myFoldl f b (x:xs) = myFoldl f (f b x) xs

myAnd :: [Bool] -> Bool
myAnd = foldr (\x acc -> if acc == False then False else x) True
myAnd' :: [Bool] -> Bool
myAnd' = foldl (\acc x -> if acc == False then False else x) True

myOr :: [Bool] -> Bool
myOr = foldr (\x acc -> if acc == True then True else x) False
myOr' :: [Bool] -> Bool
myOr' = foldr (\acc x -> if acc == True then True else x) False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem y = foldr (\x acc -> x == y || acc) False
myElemAny :: Eq a => a -> [a] -> Bool
myElemAny y = myAny (== y)

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldl (flip ((:) . f)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldl (\acc x -> if f x then acc ++ [x] else acc) []

mySquish :: [[a]] -> [a]
mySquish = foldr (++) []
mySquish' :: [[a]] -> [a]
mySquish'  = foldl (++) []

mySquishMap :: (a -> [b]) -> [a] -> [b]
--mySquishMap f = foldl (flip (flip (++) . f)) []
mySquishMap f = foldl (\acc x -> acc ++ f x) []

mySquishMap' :: (a -> [b]) -> [a] -> [b]
mySquishMap' f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = mySquishMap id


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f (x:xs) = foldr combF x xs
  where
    combF y acc = if f y acc == GT then y else acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f (x:xs) = foldr combF x xs
  where
    combF y acc = if f ny acc == LT then y else acc


myMinimumBy' _ [] = error "empty list"
myMinimumBy' f (x:xs) = foldl (\acc x -> if f x acc == LT then x else acc) x xs
