module VowelsStops where

stops = "pbtdkg"
vowels = "aeiou"

combs :: String -> String -> [String]
combs stops vowels= [ [x,y,z] |
                      x <- stops,
                      y <- vowels,
                      z <- stops ]

combs' :: String -> String -> [String]
combs' stops vowels= [ [x,y,z] |
                       x <- stops,
                       y <- vowels,
                       z <- stops,
                       x == 'p']

nouns = ["cat", "dog", "mouse"]
verbs = ["eat", "kill"]


type Sentence = [String]
combs'' :: Sentence -> Sentence -> [Sentence]
combs'' n v = [ [x,y,z] | x <- n, y <- v, z <- n]




