{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day07 where

import Control.Applicative (liftA2)
import Control.Applicative.Combinators (choice)
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Text as T
import Protolude
import Text.Megaparsec (Parsec, errorBundlePretty, parse, takeWhile1P)
import Text.Megaparsec.Char (char, eol, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Utils
import qualified Prelude

type Size = Int
type NodeName = Text
data FSNode = FSNode {name :: NodeName, size :: Size, children :: Maybe [FSNode]} deriving (Eq)

instance Prelude.Show FSNode where
    show (FSNode name size (Just children)) = concat [T.unpack name, " ", Prelude.show size, " children#: ", Prelude.show (length children)] ++ concatMap ((("\n" ++ T.unpack name ++ "/") ++) . Prelude.show) children
    show (FSNode name size _) = concat [T.unpack name, " ", Prelude.show size]

type Path = [NodeName]

emptyRoot :: FSNode
emptyRoot = FSNode "" 0 $ Just []
mkDirNode :: NodeName -> FSNode
mkDirNode dirName = FSNode dirName 0 (Just [])
mkFileNode :: NodeName -> Size -> FSNode
mkFileNode fileName size = FSNode fileName size Nothing

type ListEntry = (Maybe Size, NodeName)
data Command = ChangeRoot | ChangeBack | ChangeDir NodeName | List [ListEntry] deriving (Eq, Show)

type Parser = Parsec Void Text

changeRootP :: Parser Command
changeRootP = ChangeRoot <$ (string "$ cd /" >> eol)
changeBackP :: Parser Command
changeBackP = ChangeBack <$ (string "$ cd .." >> eol)
changeDirP :: Parser Command
changeDirP = fmap ChangeDir $ string "$ cd " >> takeWhile1P Nothing (/= '\n') <* eol
listFileP :: Parser ListEntry
listFileP = liftA2 (,) (fmap Just $ L.decimal <* string " ") (takeWhile1P Nothing (/= '\n'))
listDirP :: Parser ListEntry
listDirP = liftA2 (,) (Nothing <$ string "dir ") (takeWhile1P Nothing (/= '\n'))
listP :: Parser Command
listP = do
    _ <- string "$ ls" >> eol
    files <- many $ choice [listFileP, listDirP] <* eol
    return $ List files

commandsP :: Parser [Command]
commandsP = many $ choice [changeRootP, changeBackP, changeDirP, listP]

data FSState = FSState {root :: FSNode, path :: Path} deriving (Eq, Show)
initialState :: FSState
initialState = FSState emptyRoot []

insertDir :: NodeName -> FSNode -> Either Text FSNode
insertDir _ (FSNode fileName _ Nothing) = Left $ T.append fileName " is a file"
insertDir dirName (FSNode currDirName size (Just nodes)) = Right $ FSNode currDirName size (Just (mkDirNode dirName : nodes))
insertFile :: Size -> NodeName -> FSNode -> Either Text FSNode
insertFile _ _ (FSNode fileName _ Nothing) = Left $ T.append fileName " is a file"
insertFile fileSize fileName (FSNode currDirName size (Just nodes)) = Right $ FSNode currDirName (fileSize + size) (Just (mkFileNode fileName fileSize : nodes))

insertInto :: Path -> FSNode -> ListEntry -> Either Text FSNode
insertInto (nextStep : _) (FSNode _ _ (Just [])) _ = Left $ T.append nextStep " does not exist"
insertInto [] node (Nothing, nodeName) = insertDir nodeName node
insertInto [] node (Just size, nodeName) = insertFile size nodeName node
insertInto (nextStep : rest) (FSNode currDirName currSize (Just children)) (mSize, nodeName) = case List.findIndex ((== nextStep) . name) children of
    Just i -> do
        newChild <- insertInto rest ((List.!!) children i) (mSize, nodeName)
        let newChildren = replaceAt i newChild children
        return (FSNode currDirName (maybe currSize (+ currSize) mSize) (Just newChildren))
    Nothing -> Left $ T.append nextStep " does not exist"

runCommand :: Command -> FSState -> FSState
runCommand ChangeRoot FSState{root = root} = FSState root []
runCommand ChangeBack FSState{root = root, path = path} = FSState root $ List.drop 1 path
runCommand (ChangeDir nodeName) FSState{root = root, path = path} = FSState root $ nodeName : path
runCommand (List entries) s@FSState{root = root, path = path} = case foldM (insertInto (reverse path)) root entries of
    Right newRoot -> FSState newRoot path
    Left err -> Prelude.error $ T.unpack err -- NOTE: assume the output is semantically correct

dirs :: FSNode -> [(Size, NodeName)]
dirs (FSNode _ _ Nothing) = []
dirs (FSNode dirName size (Just xs)) = (size, dirName) : concatMap dirs xs

fsStateP :: Parser FSState
fsStateP = foldl (flip runCommand) initialState <$> commandsP

p1 :: FSNode -> Int
p1 = sum . filter (<= 100000) . List.sort . map fst . dirs

totalSpace :: Int
totalSpace = 70000000
neededSpace :: Int
neededSpace = 30000000
p2 root@(FSNode _ usedSpace _) = fst $ List.head $ filter ((>= spaceToFree) . fst) $ List.sortOn fst $ dirs root
  where
    freeSpace = totalSpace - usedSpace
    spaceToFree = neededSpace - freeSpace -- NOTE: assume this is positive

day07 :: IO ()
day07 =
    readFile "inputs/day07.txt" >>= \t -> case parse fsStateP "" t of
        Left err -> print $ errorBundlePretty err -- NOTE: assume the output is syntatically correct
        Right FSState{root = root, path = path} -> print (p1 root, p2 root)
