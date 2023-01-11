module Main(main) where


import           System.Environment (getArgs)


import           Lib

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r", fname, dir] -> rotateFromFile (read dir) fname
    ["-o", fname] -> orientFromFile fname
    _ -> putStrLn $ "Usage: locator -o filename\n" ++
                    "       locator -r filename direction"

