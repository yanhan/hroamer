module Hroamer.Database
  ( FilesTableRow(..)
  , deleteFileFromDb
  , getAllFilesInDir
  , getRowFromUUID
  , initDb
  , updateDbToMatchDirState
  , updateDirAndFilename
  , wrapDbConn
  ) where

import Data.Text (Text)
import Database.SQLite.Simple
       (Connection, execute, execute_, query, withConnection)
import Foundation
import Foundation.Collection (mapM_)
import System.Directory (doesFileExist)
import System.FilePath.Posix (FilePath)

import Hroamer.Database.Internal
       (FilesTableRow(FilesTableRow, dir, filename, uuid),
        addFileDetailsToDb, deleteFileFromDb)

initDb :: FilePath -> IO ()
initDb path_to_db = do
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
       mapM_ (addFileDetailsToDb conn cwd) file_to_uuid__only_on_system
       mapM_ (deleteFileFromDb conn cwd) files_only_in_db
       execute_ conn "COMMIT;")

wrapDbConn :: FilePath -> (a -> IO b) -> (Connection -> a) -> IO b
wrapDbConn pathToDb workFunction dbFunction =
  withConnection pathToDb (\conn -> workFunction (dbFunction conn))
