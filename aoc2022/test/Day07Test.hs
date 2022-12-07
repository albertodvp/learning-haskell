{-# LANGUAGE OverloadedStrings #-}

module Day07Test (day07_tests) where

import Day07
import Test.HUnit
import Text.Megaparsec (parse)

day07_tests =
    [ "FSNode" ~: "emptyRoot size" ~: name emptyRoot ~=? ""
    , "FSNode" ~: "emptyRoot" ~: size emptyRoot ~=? 0
    , "FSNode" ~: "emptyRoot" ~: children emptyRoot ~=? Just []
    , "insert" ~: "dir" ~: insertDir "dirname" emptyRoot ~=? Right (FSNode "" 0 (Just [FSNode "dirname" 0 $ Just []]))
    , "insert" ~: "dir on file" ~: insertDir "dirname" (FSNode "filename" 42 Nothing) ~=? Left "filename is a file"
    , "insert" ~: "file" ~: insertFile "filename" 42 emptyRoot ~=? Right (FSNode "" 42 (Just [FSNode "filename" 42 Nothing]))
    , "insert" ~: "dir on file" ~: insertFile "filename" 42 (FSNode "filename" 42 Nothing) ~=? Left "filename is a file"
    , "insert" ~: "file sum sizes" ~: (insertFile "filename1" 20 emptyRoot >>= insertFile "filename2" 22) ~=? Right (FSNode "" 42 (Just [FSNode "filename2" 22 Nothing, FSNode "filename1" 20 Nothing]))
    , "parse" ~: "parse change root" ~: parse changeRootP "" "$ cd /\n" ~=? Right ChangeRoot
    , "parse" ~: "parse change back" ~: parse changeBackP "" "$ cd ..\n" ~=? Right ChangeBack
    , "parse" ~: "parse change dir" ~: parse changeDirP "" "$ cd dirname\n" ~=? Right (ChangeDir "dirname")
    , "parse" ~: "parse list" ~: parse listP "" "$ ls\n10 filename1.ext1\n20 filename2.ext2\n" ~=? Right (List [(10, "filename1.ext1"), (20, "filename2.ext2")])
    , "parse" ~: "parse commands" ~: parse commandsP "" "$ cd ..\n$ ls\n10 filename1.ext1\n20 filename2.ext2\n" ~=? Right ([ChangeBack, List [(10, "filename1.ext1"), (20, "filename2.ext2")]])
    ]
