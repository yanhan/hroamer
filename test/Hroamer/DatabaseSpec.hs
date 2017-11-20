module Hroamer.DatabaseSpec
  ( spec
  ) where

import qualified Data.List as List
import Database.SQLite.Simple (withConnection)
import Foundation
import System.Directory (doesFileExist, doesPathExist)
import System.FilePath.Posix ((</>))
import System.IO (readFile)
import System.IO.Temp (withSystemTempDirectory, writeTempFile)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Hroamer.Database (createDbAndTables, getAllFilesInDir)
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

  describe "getAllFilesInDir" $ do
    it "should return all rows whose `dir` equal to the given value" $ do
      withSystemTempDirectory "createDbAndTables" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        let dirOfInterest = "/home/betty/irc/real"
        let firstTuple = ("creds.txt", "eae274ee-9411-4dcf-8465-dd5dd5155087")
        let secondTuple = ("arena.txt", "c304d170-57a0-4d72-bcae-e59a4177b69d")
        createDbAndTables pathToDb
        withConnection pathToDb (\dbconn -> do
          addFileDetailsToDb dbconn dirOfInterest firstTuple
          addFileDetailsToDb dbconn "/opt/local/bin" ("storm", "bd45efb5-2bb7-4dcf-86ce-a262ed958e7a")
          addFileDetailsToDb dbconn dirOfInterest secondTuple)
        fileDetails <- getAllFilesInDir pathToDb dirOfInterest
        length fileDetails `shouldBe` 2
        List.sort fileDetails `shouldBe` [secondTuple, firstTuple]
