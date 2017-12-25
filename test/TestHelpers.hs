module TestHelpers
  ( RowCount
  , clearDb
  , deleteTempDirForTest
  , genCharNotNull
  , genValidFilePathChar
  , getTotalRows
  , isNotPathSeparator
  , rmrf
  , setupDbForTest
  ) where

import Data.Char (chr)
import Data.Map (Map)
import Database.SQLite.Simple
       (Connection, execute_, query_, withConnection)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Data.Text (Text)
import Data.Traversable (traverse)
import Foundation
import System.Directory (removeDirectory, removeFile)
import System.FilePath.Posix
       ((</>), FilePath, pathSeparator, takeDirectory)
import System.IO.Temp (createTempDirectory)
import System.Process (createProcess, proc, waitForProcess)
import Test.QuickCheck (Gen, choose, suchThat)

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

rmrf :: Map Text FilePath -> IO ()
rmrf tempDirs = do
  traverse (\dir -> do
    let rmrfProc = proc "/bin/rm" ["-rf", toList dir]
    (_, _, _, ph) <- createProcess rmrfProc
    waitForProcess ph) tempDirs
  return ()

isNotPathSeparator :: Char -> Bool
isNotPathSeparator = (/= pathSeparator)

genCharNotNull :: Gen Char
genCharNotNull = choose (chr 1, maxBound :: Char)

genValidFilePathChar :: Gen Char
genValidFilePathChar = suchThat genCharNotNull $
  (\ch -> foldr (\f acc -> acc && f ch) True [isNotPathSeparator])
