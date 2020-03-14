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

-- TODO
myReverse' :: [a] -> [a]
myReverse' = foldl (\acc x -> x:acc) []



--myElem :: Eq a => a -> [a] -> Bool
--myElem y = foldr (\x acc -> if y == x then True else acc) False
