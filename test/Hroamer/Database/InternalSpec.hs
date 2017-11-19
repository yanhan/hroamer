module Hroamer.Database.InternalSpec
  ( spec
  ) where

import Database.SQLite.Simple (withConnection, query_)
import Foundation
import System.FilePath.Posix ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldReturn)

import Hroamer.Database (createDbAndTables)
import Hroamer.Database.Internal
       (addFileDetailsToDb, deleteFileFromDb)
import TestHelpers (RowCount, getTotalRows)

spec :: Spec

spec = do
  describe "addFileDetailsToDb" $ do
    it "should add a new row to the database (assuming the (dir, filename) combo does not exist)" $ do
      withSystemTempDirectory "createDbAndTables" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        createDbAndTables pathToDb
        let dir = "/home/edmund"
        let filename = "secret.txt"
        let uuid = "951ebecc-0048-4102-9c32-ecebb21c5bdc"
        withConnection
          pathToDb
          (\dbconn -> do
            getTotalRows dbconn `shouldReturn` [0]
            addFileDetailsToDb dbconn dir (filename, uuid)
            getTotalRows dbconn `shouldReturn` [1])

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
            deleteFileFromDb dbconn "/usr/local/bin"  "voltron"
            getTotalRows dbconn `shouldReturn` [1])

    it "should not change the database if the specified dir and filename are not in the table" $ do
      withSystemTempDirectory "createDbAndTables" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        createDbAndTables pathToDb
        withConnection
          pathToDb
          (\dbconn -> do
            addFileDetailsToDb dbconn "/scared/shitless" ("bro", "09b190ee-63ed-4170-b77d-9565640c2545")
            deleteFileFromDb dbconn "/another/path"  "yes"
            getTotalRows dbconn `shouldReturn` [1])
