module Hroamer.Parser
  ( parseUserDirStateFile
  ) where

import Control.Applicative ((*>), (<*))
import Data.Functor.Identity (Identity)
import Data.Text (Text, pack)
import Foundation
import Text.Parsec
       (ParsecT, alphaNum, anyChar, char, count, eof, lookAhead, manyTill,
        string, try)

parseUserDirStateFile :: ParsecT Text () Identity (Maybe (Text, Text))
parseUserDirStateFile = try commentLine <|> normalLine
  where
    commentLine = do
      char '"'
      manyTill anyChar eof
      return Nothing
    normalLine = do
      l <-
        manyTill anyChar (try . lookAhead $ (sepBarParser *> uuidParser) <* eof)
      sepBarParser
      s <- uuidParser
      return $ Just $ (pack l, pack s)
    sepBarParser = string " | "
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
