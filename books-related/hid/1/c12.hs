module C12 where

main :: IO()
main = do
  [fname] <- getArgs
  text <- TIO.readFile fname

