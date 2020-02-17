module CH9 where

-- myEnumFrom
eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd LT LT = [LT] 
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd EQ LT = [] 
eftOrd GT EQ = []
eftOrd GT GT = [GT]
eftOrd GT LT = [] 

eftInt :: Int -> Int -> [Int]
eftInt x y 
    | x < y  = x : eftInt (x+1) y
    | x == y = [x]
    | x > y  = []


eftChar :: Char -> Char -> [Char]
eftChar c1 c2
    | c1 == c2            = [c1]
    | compare c1 c2 == LT = c1 : eftChar (succ c1) c2
    | otherwise           = []



splitListOn :: (a -> Bool) -> [a] -> [[a]]
splitListOn _ [] = []
splitListOn cond l = takeWhile cond l : (splitListOn cond . drop 1 . dropWhile cond) l

splitLineOnChar :: [Char] -> [[Char]]
splitLineOnChar [] = []
splitLineOnChar line = takeWhile (/=' ') line  : (splitLineOnChar . drop 1 . dropWhile (/=' ')) line

splitLineOnGivenChar :: Char -> [Char] -> [[Char]]
splitLineOnGivenChar c = splitListOn (/=c)

-- List comprehensions
 
double_list l = [x*2 | x <- l]

double_list_with_predicate l p = [x*2 | x <- l, p x]


double_1_to_10 = double_list [1..10]
double_1_to_10' = [x*2 | x <- [1..10]]

double_1_to_10_mult_2 = double_list_with_predicate [1..10] $ (==0) .  flip rem 2
double_1_to_10_mult_2' = [x*2 | x <- [1..10], rem x 2 == 0]
power_2_3_of_1_to_10 = [x^y | x <- [1..10], y <- [2,3]]
power_2_3_of_1_to_10_pred = [x^y | x <- [1..10], y <- [2,3], x^y < 100]

-- Comprehend thy list
mySqr = [x^2 | x <- [1..10]]

f = [x | x <- mySqr, rem x 2 == 0 ]

s = [(x,y) | x <- mySqr, y <- mySqr, x<50, y>50]

t = take 5 s

acro xs = [x | x <- xs , elem x ['A'..'Z']]

-- Exercises Square cube
powerList p = [x^p | x <- [1..5]]

myS = powerList 2
myC = powerList 3

-- 1
tuples_myS_myC = [(x,y) | x <- myS, y <- myC]

-- 2
tuples_myS_myC_less_50 = [(x,y) | x <- myS, y <- myC, x < 50, y < 50]

-- 3
output_list_length = length tuples_myS_myC_less_50

-- Normal forms
num :: [Int]; num = [1,2,3]


-- Lenght is strict in the spine 
-- TODO: len [a] evaluates in WHNF?
spine_list = [1, undefined] 
len_spine = length spine_list

myLength :: [a] -> Integer
myLength [] = 0
myLength (_: xs) = 1 + myLength xs

sumListElem :: Num a => [a] -> a
sumListElem []     = 0
sumListElem (x:xs) = x + sumListElem xs 

myMap :: (a->b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
    | pred x    = x : myFilter pred xs
    | otherwise = myFilter pred xs


-- fmap, map, filter, zip, unzip, zipWith 
--

