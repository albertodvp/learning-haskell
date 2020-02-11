module CH9 where

-- TODO

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
