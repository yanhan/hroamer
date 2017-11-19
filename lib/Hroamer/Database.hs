module Hroamer.Database
  ( FilesTableRow(..)
  , createDbAndTables
  , deleteFileFromDb
  , getAllFilesInDir
  , getRowFromUUID
  , updateDbToMatchDirState
  , updateDirAndFilename
  , wrapDbConn
  ) where

import Data.Text (Text)
import Database.SQLite.Simple
       (Connection, execute, execute_, query, withConnection)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
import Foundation
import Foundation.Collection (mapM_)
import System.Directory (doesFileExist)
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
    (FilesTableRow {dir = dir, filename = filename, uuid = uuid})

deleteFileFromDb :: FilePath -> Connection -> [Char] -> IO ()
deleteFileFromDb cwd conn filename =
  execute
    conn
    "DELETE FROM files WHERE dir = ? AND filename = ?;"
    [cwd, filename]

getAllFilesInDir :: FilePath -> FilePath -> IO [([Char], Text)]
getAllFilesInDir path_to_db dirname =
  withConnection
    path_to_db
    (\conn ->
       query
         conn
         "SELECT filename, uuid FROM files WHERE dir = ?;"
         [dirname])

getRowFromUUID :: Connection -> Text -> IO [FilesTableRow]
getRowFromUUID dbconn uuid =
  query dbconn "SELECT dir, filename, uuid FROM files WHERE uuid = ?" [uuid]

updateDirAndFilename :: Connection -> FilesTableRow -> IO ()
updateDirAndFilename dbconn ftr =
  execute
    dbconn
    "UPDATE files SET dir=?, filename=? WHERE uuid=?;"
    ftr

updateDbToMatchDirState :: FilePath -> FilePath -> [([Char], Text)] -> [[Char]] -> IO ()
updateDbToMatchDirState cwd path_to_db file_to_uuid__only_on_system files_only_in_db
 =
  withConnection
    path_to_db
    (\conn -> do
       execute_ conn "BEGIN TRANSACTION;"
       mapM_ (addFileDetailsToDb cwd conn) file_to_uuid__only_on_system
       mapM_ (deleteFileFromDb cwd conn) files_only_in_db
       execute_ conn "COMMIT;")

wrapDbConn :: FilePath -> (b -> IO a) -> (Connection -> b) -> IO a
wrapDbConn pathToDb workFunction dbFunction =
  withConnection pathToDb (\conn -> workFunction (dbFunction conn))
