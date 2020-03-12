module RewritingWithFolds where

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (x:xs) = f x (myFoldr f b xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ b [] = b
myFoldl f b (x:xs) = myFoldl f (f b x) xs

myAnd :: [Bool] -> Bool
myAnd = foldr (\a b -> if a == False then False else b) True

myOr :: [Bool] -> Bool
myOr = foldr (\a b -> if a == True then True else b) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> if f a then True else b) False

--myElem :: Eq a => a -> [a] -> Bool

