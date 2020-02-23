module MyStandartFunctions where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x == True then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = if x == e then True else myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (==e)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
