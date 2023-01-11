module ListOps where

-- Spine and cons cells
spine_list = [1, undefined]
len_spine = length spine_list

myLength :: [a] -> Integer
myLength []      = 0
myLength (_: xs) = 1 + myLength xs

sumListElem :: Num a => [a] -> a
sumListElem []     = 0
sumListElem (x:xs) = x + sumListElem xs

-- Mapping

myMap :: (a->b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
    | pred x    = x : myFilter pred xs
    | otherwise = myFilter pred xs


-- Zipping

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _            = []
myZip _ []            = []
myZip (x:xs) (x':xs') = (x,x') : myZip xs xs'

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _             = []
myZipWith _ _ []             = []
myZipWith f (x:xs) (x': xs') = f x x' : myZipWith f xs xs'

myZip' :: [a] -> [b] -> [(a,b)]
myZip' a b = myZipWith (\x y -> (x, y)) a b

