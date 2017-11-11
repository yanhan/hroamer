module Hroamer.Parser
  ( parseDirStateLine
  ) where

import Control.Applicative ((*>), (<*))
import Data.Functor.Identity (Identity)
import Data.Text (Text, pack)
import Foundation
import Text.Parsec
       (Parsec, alphaNum, anyChar, char, count, eof, lookAhead, manyTill,
        string, try)

parseDirStateLine :: Parsec Text () (Maybe (Text, Text))
parseDirStateLine = try commentLine <|> normalLine
  where
    commentLine :: Parsec Text () (Maybe a)
    commentLine = do
      char '"'
      manyTill anyChar eof
      return Nothing

    normalLine :: Parsec Text () (Maybe (Text, Text))
    normalLine = do
      l <-
        manyTill anyChar (try . lookAhead $ (sepBarParser *> uuidParser) <* eof)
      sepBarParser
      s <- uuidParser
      return $ Just $ (pack l, pack s)

    sepBarParser :: Parsec Text () [Char]
    sepBarParser = string " | "

    uuidParser :: Parsec Text () [Char]
    uuidParser = do
      s1 <- count 8 alphaNum
      char '-'
      s2 <- count 4 alphaNum
      char '-'
      s3 <- count 4 alphaNum
      char '-'
      s4 <- count 4 alphaNum
      char '-'
      s5 <- count 12 alphaNum
      return $ s1 <> "-" <> s2 <> "-" <> s3 <> "-" <> s4 <> "-" <> s5
