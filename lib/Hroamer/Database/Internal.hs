module Hroamer.Database.Internal
  ( FilesTableRow(..)
  , addFileDetailsToDb
  , deleteFileFromDb
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

addFileDetailsToDb :: Connection -> FilePath -> ([Char], Text) -> IO ()
addFileDetailsToDb conn dir (filename, uuid) =
  execute
    conn
    "INSERT INTO files(dir, filename, uuid) VALUES(?, ?, ?);"
    (FilesTableRow {dir = dir, filename = filename, uuid = uuid})

deleteFileFromDb :: Connection -> FilePath -> [Char] -> IO ()
deleteFileFromDb conn cwd filename =
  execute
    conn
    "DELETE FROM files WHERE dir = ? AND filename = ?;"
    [cwd, filename]
