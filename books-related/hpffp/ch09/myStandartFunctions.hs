module MyStandartFunctions where

myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = if x == False then False else myAnd xs

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = if f x == True then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs) = if x == e then True else myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (==e)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- mySquish
mySquish :: [[a]] -> [a]
mySquish []     = []
mySquish (x:xs) = x ++ mySquish xs


-- mySquishMap
mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap _ []     = []
mySquishMap f (x:xs) = f x ++ mySquishMap f xs

-- mySquishAgain
mySquishAgain :: [[a]] -> [a]
mySquishAgain = mySquishMap id

-- myMaximumBy
myMaximumBy :: (a -> a -> Ordering)
               -> [a] -> a
myMaximumBy _ [] = error("empty list")
myMaximumBy _ (x:[]) = x
myMaximumBy compF (x:xs) =
  if compF x y == GT then x else y
  where y = myMaximumBy compF xs


-- myMinimumBy
myMinimumBy :: (a -> a -> Ordering)
               -> [a] -> a
myMinimumBy _ [] = error("empty list")
myMinimumBy _ (x:[]) = x
myMinimumBy compF (x:xs) =
  if compF x y == LT then x else y
  where y = myMinimumBy compF xs
