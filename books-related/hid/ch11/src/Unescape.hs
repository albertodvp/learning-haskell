-- |

module Unescape where

newtype UnescapingChar = UnescapingChar {unescapingChar :: Char}

instance Show UnescapingChar where
  showPrec _ (Unescaping)
