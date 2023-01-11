import           Data.Char

-- 1

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf (x:xs) l = if elem x l then isSubseqOf xs (tail (dropWhile (\x -> x /= x)l)) else False


tests = [
  isSubseqOf "blah" "blahwoot" == True,
  isSubseqOf "blah" "wootblah" == True,
  isSubseqOf "blah" "wboloath" == True,
  isSubseqOf "blah" "wootbla" == False,
  isSubseqOf "blah" "halbwoot" == False,
  isSubseqOf "blah" "blawhoot" == True
  ]

test = if minimum tests == True then print "Pass" else error "Fail"


-- 2
splitWords :: String -> [String]
splitWords "" = []
splitWords s = next: splitWords sTail
  where
    next = takeWhile (\x -> x /= ' ') s
    sTail = dropWhile (\x -> x == ' ') (dropWhile (\x -> x /= ' ') s)

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (\s@(x:xs) -> (s, toUpper x:xs)) (splitWords s)
