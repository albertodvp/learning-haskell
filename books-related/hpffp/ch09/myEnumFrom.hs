module MyEnumFrom where

-- myEnumFrom
eftBool :: Bool -> Bool -> [Bool]
eftBool False True  = [False, True]
eftBool False False = [False]
eftBool True True   = [True]
eftBool True False  = []

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

