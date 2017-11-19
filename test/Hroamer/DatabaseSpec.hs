module Hroamer.DatabaseSpec
  ( spec
  ) where

import Database.SQLite.Simple (withConnection)
import Foundation
import System.Directory (doesFileExist, doesPathExist)
import System.FilePath.Posix ((</>))
import System.IO (readFile)
import System.IO.Temp (withSystemTempDirectory, writeTempFile)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Hroamer.Database (createDbAndTables, deleteFileFromDb)
import Hroamer.Database.Internal (addFileDetailsToDb)
import TestHelpers (getTotalRows)

spec :: Spec
spec = do
  describe "createDbAndTables" $ do
    it "should create a new SQLite database if it does not exist" $
      withSystemTempDirectory "createDbAndTables" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        doesPathExist pathToDb `shouldReturn` False
        createDbAndTables pathToDb
        doesFileExist pathToDb `shouldReturn` True
        withConnection pathToDb (\dbconn -> do getTotalRows dbconn `shouldReturn` [0])

    it "should not create a SQLite database if a file exists at that location" $
      withSystemTempDirectory "createDbAndTables" $ \dirPath -> do
        let contents = "abracadabra"
        pathToDb <- writeTempFile dirPath "hroamerdb"  contents
        createDbAndTables pathToDb
        doesFileExist pathToDb `shouldReturn` True
        readFile pathToDb `shouldReturn` contents

  describe "deleteFileFromDb" $ do
    it "should delete the specified dir and filename if they are in the table" $ do
      withSystemTempDirectory "createDbAndTables" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        createDbAndTables pathToDb
        withConnection
          pathToDb
          (\dbconn -> do
            addFileDetailsToDb dbconn "/usr/local/bin" ("voltron", "951ebecc-0048-4102-9c32-ecebb21c5bdc")
            addFileDetailsToDb dbconn "/home/monroe/images" ("hifive.jpeg", "cbc7f439-11ab-4255-8d1a-542874de1b17")
            getTotalRows dbconn `shouldReturn` [2]
            deleteFileFromDb "/usr/local/bin" dbconn "voltron"
            getTotalRows dbconn `shouldReturn` [1])

    it "should not change the database if the specified dir and filename are not in the table" $ do
      withSystemTempDirectory "createDbAndTables" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        createDbAndTables pathToDb
        withConnection
          pathToDb
          (\dbconn -> do
            addFileDetailsToDb dbconn "/scared/shitless" ("bro", "09b190ee-63ed-4170-b77d-9565640c2545")
            deleteFileFromDb "/another/path" dbconn "yes"
            getTotalRows dbconn `shouldReturn` [1])
