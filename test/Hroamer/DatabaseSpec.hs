module Hroamer.DatabaseSpec
  ( spec
  ) where

import Control.Monad (mapM_)
import qualified Data.List as List
import Database.SQLite.Simple (withConnection)
import Foundation
import System.Directory (doesFileExist, doesPathExist)
import System.FilePath.Posix ((</>))
import System.IO (readFile)
import System.IO.Temp (withSystemTempDirectory, writeTempFile)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Hroamer.Database
       (createDbAndTables, getAllFilesInDir, getRowFromUUID,
        updateDbToMatchDirState, updateDirAndFilename)
import Hroamer.Database.Internal
       (FilesTableRow(FilesTableRow, dir, filename, uuid),
        addFileDetailsToDb)
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
      withSystemTempDirectory "getAllFilesInDir" $ \dirPath -> do
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

  describe "getRowFromUUID" $ do
    it "should retrieve the row with the uuid" $ do
      withSystemTempDirectory "getRowFromUUID" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        let dirOfInterest = "/home/fire/is/at"
        let fileOfInterest = "somewhere"
        let uuidOfInterest = "9cef0b1c-113b-4b51-88f2-1f93d9178a15"
        createDbAndTables pathToDb
        withConnection pathToDb (\dbconn -> do
          addFileDetailsToDb dbconn "/usr/share/man/man1" ("make.1.gz", "0ae251b2-bd1d-41c6-b574-40198710e9e3")
          addFileDetailsToDb dbconn dirOfInterest (fileOfInterest, uuidOfInterest)
          row <- getRowFromUUID dbconn uuidOfInterest
          length row `shouldBe` 1
          listToMaybe row `shouldBe`
            Just (FilesTableRow dirOfInterest fileOfInterest uuidOfInterest))

  describe "updateDirAndFilename" $ do
    it "should update the dir and filename of the row that has the given uuid" $ do
      withSystemTempDirectory "updateDirAndFilename" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        let originalDir = "/my/imaginary/dir/"
        let originalFile = "ledger.c"
        let newDir = "/favorite/pasttimes"
        let newFile = "movies"
        let uuid = "5730152c-9a3f-4b4e-b373-62ea1a2abb4f"
        createDbAndTables pathToDb
        withConnection pathToDb (\dbconn -> do
          addFileDetailsToDb dbconn originalDir (originalFile, uuid)
          addFileDetailsToDb dbconn "/main/man" ("yu", "45889947-8104-4404-a704-7d59d2bd1aed")
          updateDirAndFilename dbconn $ FilesTableRow newDir newFile uuid
          row <- getRowFromUUID dbconn uuid
          length row `shouldBe` 1
          listToMaybe row `shouldBe`
            Just (FilesTableRow newDir newFile uuid))

  describe "updateDbToMatchDirState" $ do
    it "should insert into the table files only on the system and delete from the table files not on the system" $ do
      withSystemTempDirectory "updateDbToMatchDirState" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        let dirOfInterest = "/going/to"
        let ftrOne = FilesTableRow "/leaving/this"  "alone"  "855fba5b-8c27-41a0-9d0b-aa60e25f4193"
        let ftrTwo = FilesTableRow dirOfInterest  "be-created"  "91bd704f-fb43-4cd7-8eb6-3afc08086d72"
        let ftrThree = FilesTableRow dirOfInterest "ecstatic"  "dd132727-df0f-405f-bcd1-f972916bcb4e"
        let ftrFour = FilesTableRow dirOfInterest  "destination"  "1bcaf24f-8a88-4377-ae15-8beaf75a2b4f"
        createDbAndTables pathToDb
        withConnection pathToDb (\dbconn -> do
          addFileDetailsToDb dbconn dirOfInterest ("delete", "ce053cea-bd64-42c6-b3ed-13931a917017")
          addFileDetailsToDb dbconn (dir ftrOne) (filename ftrOne, uuid ftrOne)
          addFileDetailsToDb dbconn dirOfInterest ("sink", "fb4d61ef-77ef-4515-b4b8-7d83db42b20d")
          updateDbToMatchDirState
            dirOfInterest
            pathToDb
            [
              (filename ftrTwo, uuid ftrTwo)
            , (filename ftrThree, uuid ftrThree)
            , (filename ftrFour, uuid ftrFour)
            ]
            ["delete", "sink"]
          getTotalRows dbconn `shouldReturn` [4]
          mapM_
            (\ftr ->
              getRowFromUUID dbconn (uuid ftr) >>= (shouldBe $ Just ftr) . listToMaybe)
             [ftrOne, ftrTwo, ftrThree, ftrFour])
