module Hroamer.DatabaseSpec
  ( spec
  ) where

import Database.SQLite.Simple (query_, withConnection)
import Foundation
import System.Directory (doesFileExist, doesPathExist)
import System.FilePath.Posix ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Hroamer.Database (FilesTableRow, createDbAndTables)

spec :: Spec
spec = do
  describe "createDbAndTables" $ do
    it "should create a new SQLite database if it does not exist" $
      withSystemTempDirectory "createDbAndTables" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        doesPathExist pathToDb `shouldReturn` False
        createDbAndTables pathToDb
        doesFileExist pathToDb `shouldReturn` True
        withConnection
          pathToDb
          (\dbconn -> do
             query_ dbconn "SELECT * FROM files;" `shouldReturn`
               ([] :: [FilesTableRow]))
