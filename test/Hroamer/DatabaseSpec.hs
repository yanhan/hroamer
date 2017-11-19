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

import Hroamer.Database (createDbAndTables)
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
