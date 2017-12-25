module Hroamer.Parser
  ( parseDirStateLine
  ) where

import Data.Text (Text, pack)
import Foundation
import Text.Parsec
       (Parsec, alphaNum, anyChar, char, count, eof, many1, manyTill,
        optional, space, try)
import Text.Parsec.Char (satisfy)

import Hroamer.Char (isNotSpace)

parseDirStateLine :: Parsec Text () (Maybe (Text, Text))
parseDirStateLine = try commentLine <|> normalLine
  where
    commentLine :: Parsec Text () (Maybe a)
    commentLine = do
      char '"'
      manyTill anyChar eof
      return Nothing

    notSpace :: Parsec Text () Char
    notSpace = satisfy isNotSpace

    normalLine :: Parsec Text () (Maybe (Text, Text))
    normalLine = do
      l <- many1 notSpace
      sepBarParser
      s <- uuidParser
      optional $ do
        sepBarParser
        -- The original path to the file, we just consume and discard
        many1 notSpace
      eof
      return $ Just $ (pack l, pack s)

    sepBarParser :: Parsec Text () [Char]
    sepBarParser = many1 space >> char '|' >> many1 space

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
