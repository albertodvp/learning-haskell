{-# LANGUAGE OverloadedStrings #-}

module Day07Test (day07Tests) where

import           Day07
import           Test.HUnit
import           Text.Megaparsec (parse)

day07Tests =
    [ "FSNode" ~: "emptyRoot size" ~: name emptyRoot ~?= ""
    , "FSNode" ~: "emptyRoot" ~: size emptyRoot ~?= 0
    , "FSNode" ~: "emptyRoot" ~: children emptyRoot ~?= Just []
    , "insert" ~: "dir" ~: insertDir "dirname" emptyRoot ~?= Right (FSNode "" 0 (Just [FSNode "dirname" 0 $ Just []]))
    , "insert" ~: "dir on file" ~: insertDir "dirname" (FSNode "filename" 42 Nothing) ~?= Left "filename is a file"
    , "insert" ~: "file" ~: insertFile 42 "filename" emptyRoot ~?= Right (FSNode "" 42 (Just [FSNode "filename" 42 Nothing]))
    , "insert" ~: "dir on file" ~: insertFile 42 "filename" (FSNode "filename" 42 Nothing) ~?= Left "filename is a file"
    , "insert" ~: "file sum sizes" ~: (insertFile 20 "filename1" emptyRoot >>= insertFile 22 "filename2") ~?= Right (FSNode "" 42 (Just [FSNode "filename2" 22 Nothing, FSNode "filename1" 20 Nothing]))
    , "insertInto" ~: "bad path (dir)" ~: insertInto ["a"] emptyRoot (Nothing, "dirname") ~?= Left "a does not exist"
    , "insertInto" ~: "bad path (file)" ~: insertInto ["a"] emptyRoot (Just 42, "filename") ~?= Left "a does not exist"
    , "insertInto" ~: "good path base (dir)" ~: insertInto [] (FSNode "" 0 (Just [])) (Nothing, "username") ~?= Right (FSNode "" 0 (Just [FSNode "username" 0 (Just [])]))
    , "insertInto" ~: "good path base (file)" ~: insertInto [] (FSNode "" 0 (Just [])) (Just 42, "filename") ~?= Right (FSNode "" 42 (Just [FSNode "filename" 42 Nothing]))
    , "insertInto" ~: "good path base (file) (does not alter)" ~: insertInto [] (FSNode "" 0 (Just [FSNode "otherfile" 42 Nothing])) (Just 42, "filename") ~?= Right (FSNode "" 42 (Just [FSNode "filename" 42 Nothing, FSNode "otherfile" 42 Nothing]))
    , "insertInto" ~: "good path one step (dir)" ~: insertInto ["home"] (FSNode "" 0 (Just [FSNode "home" 0 $ Just []])) (Nothing, "username") ~?= Right (FSNode "" 0 (Just [FSNode "home" 0 $ Just [FSNode "username" 0 (Just [])]]))
    , "insertInto" ~: "good path one step (file)" ~: insertInto ["home"] (FSNode "" 0 (Just [FSNode "home" 0 $ Just []])) (Just 42, "filename") ~?= Right (FSNode "" 42 (Just [FSNode "home" 42 $ Just [FSNode "filename" 42 Nothing]]))
    , "insertInto" ~: "good path" ~: insertInto ["home", "username"] (FSNode "" 20 (Just [FSNode "home" 20 $ Just [FSNode "otherfile" 20 Nothing, FSNode "username" 0 $ Just []]])) (Just 22, "filename") ~?= Right (FSNode "" 42 (Just [FSNode "home" 42 $ Just [FSNode "otherfile" 20 Nothing, FSNode "username" 22 $ Just [FSNode "filename" 22 Nothing]]]))
    , "parse" ~: "parse change root" ~: parse changeRootP "" "$ cd /\n" ~?= Right ChangeRoot
    , "parse" ~: "parse change back" ~: parse changeBackP "" "$ cd ..\n" ~?= Right ChangeBack
    , "parse" ~: "parse change dir" ~: parse changeDirP "" "$ cd dirname\n" ~?= Right (ChangeDir "dirname")
    , "parse" ~: "parse list (files only)" ~: parse listP "" "$ ls\n10 filename1.ext1\n20 filename2.ext2\n" ~?= Right (List [(Just 10, "filename1.ext1"), (Just 20, "filename2.ext2")])
    , "parse" ~: "parse list (dirs only)" ~: parse listP "" "$ ls\ndir dirname1\ndir dirname2\n" ~?= Right (List [(Nothing, "dirname1"), (Nothing, "dirname2")])
    , "parse" ~: "parse list (mix)" ~: parse listP "" "$ ls\ndir dirname\n42 filename\n" ~?= Right (List [(Nothing, "dirname"), (Just 42, "filename")])
    , "parse" ~: "parse commands" ~: parse commandsP "" "$ cd ..\n$ ls\n10 filename1.ext1\n20 filename2.ext2\ndir dirname\n" ~?= Right [ChangeBack, List [(Just 10, "filename1.ext1"), (Just 20, "filename2.ext2"), (Nothing, "dirname")]]
    , "parse" ~: "parse commands" ~: parse commandsP "" "$ cd dirname\n$ ls\n10 filename.ext\ndir dirname\n" ~?= Right [ChangeDir "dirname", List [(Just 10, "filename.ext"), (Nothing, "dirname")]]
    , "runCommand" ~: "root" ~: runCommand ChangeRoot (FSState emptyRoot ["c", "b", "a"]) ~?= FSState emptyRoot []
    , "runCommand" ~: "root (non empty)" ~: runCommand ChangeRoot (FSState (FSNode "" 0 (Just [FSNode "dirname" 0 $ Just []])) ["c", "b", "a"]) ~?= FSState (FSNode "" 0 (Just [FSNode "dirname" 0 $ Just []])) []
    , "runCommand" ~: "back" ~: runCommand ChangeBack (FSState emptyRoot ["c", "b", "a"]) ~?= FSState emptyRoot ["b", "a"]
    , "runCommand" ~: "back (non empty)" ~: runCommand ChangeBack (FSState (FSNode "" 0 (Just [FSNode "dirname" 0 $ Just []])) ["c", "b", "a"]) ~?= FSState (FSNode "" 0 (Just [FSNode "dirname" 0 $ Just []])) ["b", "a"]
    , "runCommand" ~: "change" ~: runCommand (ChangeDir "d") (FSState emptyRoot ["c", "b", "a"]) ~?= FSState emptyRoot ["d", "c", "b", "a"]
    , "runCommand" ~: "change (non empty)" ~: runCommand (ChangeDir "d") (FSState (FSNode "" 0 (Just [FSNode "dirname" 0 $ Just []])) ["c", "b", "a"]) ~?= FSState (FSNode "" 0 (Just [FSNode "dirname" 0 $ Just []])) ["d", "c", "b", "a"]
    , "runCommand" ~: "list (non empty)" ~: runCommand (List [(Nothing, "subdirname"), (Just 42, "filename")]) (FSState (FSNode "" 0 (Just [FSNode "dirname" 0 $ Just []])) ["dirname"]) ~?= FSState (FSNode "" 42 (Just [FSNode "dirname" 42 $ Just [FSNode "filename" 42 Nothing, FSNode "subdirname" 0 (Just [])]])) ["dirname"]
    , "runCommand" ~: "list (non empty)" ~: runCommand (List [(Nothing, "subsubdirname"), (Just 42, "subfilename")]) (FSState (FSNode "" 42 (Just [FSNode "dirname" 42 $ Just [FSNode "filename" 42 Nothing, FSNode "subdirname" 0 (Just [])]])) ["subdirname", "dirname"]) ~?= FSState (FSNode "" 84 $ Just [FSNode "dirname" 84 $ Just [FSNode "filename" 42 Nothing, FSNode "subdirname" 42 $ Just [FSNode "subfilename" 42 Nothing, FSNode "subsubdirname" 0 $ Just []]]]) ["subdirname", "dirname"]
    , "dirs" ~: "" ~: dirs (FSNode "" 42 (Just [FSNode "dirname" 42 $ Just [FSNode "filename" 42 Nothing, FSNode "subdirname" 0 (Just [])]])) ~?= [(42, ""), (42, "dirname"), (0, "subdirname")]
    ]
