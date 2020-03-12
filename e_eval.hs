module EEval where


-- ATTEMPT 1:
term :: Double -> Double -> Double
term i x = x ^ round i / product [1..i]



eval x = sum (map (\y -> y x) (map term [0..10]))
