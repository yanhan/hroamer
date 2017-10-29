module Hroamer.Database
  ( createDbAndTables
  , deleteFileFromDb
  ) where

import Database.SQLite.Simple
       (Connection, execute, execute_, withConnection)
import Foundation
import System.Directory (doesFileExist)
import System.FilePath.Posix (FilePath)

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

deleteFileFromDb :: FilePath -> Connection -> [Char] -> IO ()
deleteFileFromDb cwd conn filename =
  execute
    conn
    "DELETE FROM files WHERE dir = ? AND filename = ?;"
    [cwd, filename]
