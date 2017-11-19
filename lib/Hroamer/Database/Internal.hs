module Hroamer.Database.Internal
  ( FilesTableRow(..)
  , addFileDetailsToDb
  ) where

import Database.SQLite.Simple (Connection, execute)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
import Data.Text (Text)
import Foundation
import System.FilePath.Posix (FilePath)

data FilesTableRow = FilesTableRow
  { dir :: FilePath
  , filename :: FilePath
  , uuid :: Text
  } deriving (Eq, Show)

instance FromRow FilesTableRow where
  fromRow = FilesTableRow <$> field <*> field <*> field

instance ToRow FilesTableRow where
  toRow ftr = toRow (dir ftr, filename ftr, uuid ftr)

addFileDetailsToDb :: FilePath -> Connection -> ([Char], Text) -> IO ()
addFileDetailsToDb dir conn (filename, uuid) =
  execute
    conn
    "INSERT INTO files(dir, filename, uuid) VALUES(?, ?, ?);"
    (FilesTableRow {dir = dir, filename = filename, uuid = uuid})
