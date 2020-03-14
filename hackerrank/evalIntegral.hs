module Integral where

evalPolynomial :: (Integral a, Integral b, Fractional c) => [a] -> [b] -> c -> c
evalPolynomial s t x = sum $ zipWith (\a b -> fromIntegral a * x ^^ b) s t

solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [area, volume]
  where
    step = 0.001 :: Double
    area = subIntervalSumMap (\x -> step * evalPolynomial a b x)
    volume = subIntervalSumMap (\x -> step * pi * (evalPolynomial a b x)^2)
    subIntervalSumMap f = sum $ map f [l', l'+step..r']
    l' = fromIntegral l
    r' = fromIntegral r
