module TestHelpers
  ( RowCount
  , clearDb
  , deleteTempDirForTest
  , getTotalRows
  , setupDbForTest
  ) where

import Database.SQLite.Simple
       (Connection, execute_, query_, withConnection)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Foundation
import System.Directory (removeDirectory, removeFile)
import System.FilePath.Posix ((</>), FilePath, takeDirectory)
import System.IO.Temp (createTempDirectory)

import Hroamer.Database (createDbAndTables)

type RowCount = Int

instance FromRow Int where
  fromRow = field

getTotalRows :: Connection -> IO [RowCount]
getTotalRows dbconn = query_ dbconn "SELECT COUNT(1) FROM files;"

setupDbForTest :: [Char] -> IO FilePath
setupDbForTest filenameTemplate = do
  dirPath <- createTempDirectory "/tmp"  filenameTemplate
  let pathToDb = dirPath </> "hroamer.db"
  createDbAndTables pathToDb
  return pathToDb

deleteTempDirForTest :: FilePath -> IO ()
deleteTempDirForTest pathToDb = do
  removeFile pathToDb
  removeDirectory $ takeDirectory pathToDb

clearDb :: FilePath -> IO ()
clearDb pathToDb = withConnection pathToDb (\dbconn ->
  execute_ dbconn "DELETE FROM files;")
