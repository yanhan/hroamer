module Hroamer.Database
  ( FilesTableRow(..)
  , addFileDetailsToDb
  , createDbAndTables
  , deleteFileFromDb
  ) where

import Data.Text (Text)
import Database.SQLite.Simple
       (Connection, execute, execute_, withConnection)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
import Foundation
import System.Directory (doesFileExist)
import System.FilePath.Posix (FilePath)

data FilesTableRow =
  FilesTableRow FilePath
                FilePath
                Text
  deriving (Show)

instance FromRow FilesTableRow where
  fromRow = FilesTableRow <$> field <*> field <*> field

instance ToRow FilesTableRow where
  toRow (FilesTableRow dir filename file_uuid) =
    toRow (dir, filename, file_uuid)

createDbAndTables :: FilePath -> IO ()
createDbAndTables path_to_db = do
  db_exists <- doesFileExist path_to_db
  if not db_exists
    then withConnection
           path_to_db
           (\conn -> do
              execute_
                conn
                "CREATE TABLE IF NOT EXISTS files(dir TEXT, filename TEXT, uuid CHAR(36), CONSTRAINT files__idx_dir_filename UNIQUE(dir, filename) ON CONFLICT ROLLBACK, CONSTRAINT files__uuid UNIQUE(uuid) ON CONFLICT ROLLBACK);"
              execute_ conn "CREATE INDEX files__idx_dir ON files(dir);")
    else return ()

addFileDetailsToDb :: FilePath -> Connection -> ([Char], Text) -> IO ()
addFileDetailsToDb dir conn (filename, uuid) =
  execute
    conn
    "INSERT INTO files(dir, filename, uuid) VALUES(?, ?, ?);"
    (FilesTableRow dir filename uuid)

deleteFileFromDb :: FilePath -> Connection -> [Char] -> IO ()
deleteFileFromDb cwd conn filename =
  execute
    conn
    "DELETE FROM files WHERE dir = ? AND filename = ?;"
    [cwd, filename]
