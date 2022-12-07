{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day07 where

import Control.Applicative (liftA2)
import Control.Applicative.Combinators (choice)
import qualified Data.Text as T
import Protolude
import Text.Megaparsec (Parsec, takeWhile1P)
import Text.Megaparsec.Char (char, eol, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Size = Int
type NodeName = Text
data FSNode = FSNode {name :: NodeName, size :: Size, children :: Maybe [FSNode]} deriving (Show, Eq)

emptyRoot :: FSNode
emptyRoot = FSNode "" 0 $ Just []
mkDirNode :: NodeName -> FSNode
mkDirNode dirName = FSNode dirName 0 (Just [])
mkFileNode :: NodeName -> Size -> FSNode
mkFileNode fileName size = FSNode fileName size Nothing

insertDir :: NodeName -> FSNode -> Either Text FSNode
insertDir _ (FSNode fileName _ Nothing) = Left $ T.append fileName " is a file"
insertDir dirName (FSNode currDirName size (Just nodes)) = Right $ FSNode currDirName size (Just (mkDirNode dirName : nodes))
insertFile :: NodeName -> Size -> FSNode -> Either Text FSNode
insertFile _ _ (FSNode fileName _ Nothing) = Left $ T.append fileName " is a file"
insertFile fileName fileSize (FSNode currDirName size (Just nodes)) = Right $ FSNode currDirName (fileSize + size) (Just (mkFileNode fileName fileSize : nodes))

type ParsedFile = (Size, NodeName)
data Command = ChangeRoot | ChangeBack | ChangeDir NodeName | List [ParsedFile] deriving (Eq, Show)

type Parser = Parsec Void Text

changeRootP :: Parser Command
changeRootP = ChangeRoot <$ (string "$ cd /" >> eol)
changeBackP :: Parser Command
changeBackP = ChangeBack <$ (string "$ cd .." >> eol)
changeDirP :: Parser Command
changeDirP = ChangeDir <$> ((string "$ cd " >> takeWhile1P Nothing (/= '\n')) <* eol)
listP :: Parser Command
listP = do
    _ <- string "$ ls" >> eol
    files <- many $ liftA2 (,) (L.decimal <* string " ") (takeWhile1P Nothing (/= '\n') <* eol)
    return $ List files

commandsP :: Parser [Command]
commandsP = many $ choice [changeRootP, changeBackP, changeDirP, listP]

day07 :: IO ()
day07 = readFile "inputs/day07.txt" >>= print
