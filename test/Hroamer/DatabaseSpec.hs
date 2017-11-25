module Hroamer.DatabaseSpec
  ( spec
  ) where

import Control.Monad (mapM_)
import qualified Data.List as List
import Database.SQLite.Simple (execute_, withConnection)
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Foundation
import System.Directory
       (doesFileExist, doesPathExist, removeDirectory, removeFile)
import System.FilePath.Posix ((</>), FilePath, takeDirectory)
import System.IO (readFile)
import System.IO.Temp
       (createTempDirectory, withSystemTempDirectory, writeTempFile)
import Test.Hspec
       (Spec, after, afterAll, beforeAll, describe, it, shouldBe,
        shouldReturn)

import Hroamer.Database
       (createDbAndTables, getAllFilesInDir, getRowFromUUID,
        updateDbToMatchDirState, updateDirAndFilename, wrapDbConn)
import Hroamer.Database.Internal
       (FilesTableRow(FilesTableRow, dir, filename, uuid),
        addFileDetailsToDb)
import TestHelpers
       (clearDb, deleteTempDirForTest, getTotalRows, setupDbForTest)

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

  beforeAll (setupDbForTest "DatabaseSpec") $ afterAll deleteTempDirForTest $ after clearDb $ do
    describe "getAllFilesInDir" $ do
      it "should return all rows whose `dir` equal to the given value" $ \pathToDb -> do
        let dirOfInterest = "/home/betty/irc/real"
        let firstTuple = ("creds.txt", "eae274ee-9411-4dcf-8465-dd5dd5155087")
        let secondTuple = ("arena.txt", "c304d170-57a0-4d72-bcae-e59a4177b69d")
        withConnection pathToDb (\dbconn -> do
          addFileDetailsToDb dbconn dirOfInterest firstTuple
          addFileDetailsToDb dbconn "/opt/local/bin" ("storm", "bd45efb5-2bb7-4dcf-86ce-a262ed958e7a")
          addFileDetailsToDb dbconn dirOfInterest secondTuple)
        fileDetails <- getAllFilesInDir pathToDb dirOfInterest
        length fileDetails `shouldBe` 2
        List.sort fileDetails `shouldBe` [secondTuple, firstTuple]

    describe "getRowFromUUID" $ do
      it "should retrieve the row with the uuid" $ \pathToDb -> do
        let dirOfInterest = "/home/fire/is/at"
        let fileOfInterest = "somewhere"
        let uuidOfInterest = "9cef0b1c-113b-4b51-88f2-1f93d9178a15"
        withConnection pathToDb (\dbconn -> do
          addFileDetailsToDb dbconn "/usr/share/man/man1" ("make.1.gz", "0ae251b2-bd1d-41c6-b574-40198710e9e3")
          addFileDetailsToDb dbconn dirOfInterest (fileOfInterest, uuidOfInterest)
          row <- getRowFromUUID dbconn uuidOfInterest
          length row `shouldBe` 1
          listToMaybe row `shouldBe`
            Just (FilesTableRow dirOfInterest fileOfInterest uuidOfInterest))

    describe "updateDirAndFilename" $ do
      it "should update the dir and filename of the row that has the given uuid" $ \pathToDb -> do
        let originalDir = "/my/imaginary/dir/"
        let originalFile = "ledger.c"
        let newDir = "/favorite/pasttimes"
        let newFile = "movies"
        let uuid = "5730152c-9a3f-4b4e-b373-62ea1a2abb4f"
        withConnection pathToDb (\dbconn -> do
          addFileDetailsToDb dbconn originalDir (originalFile, uuid)
          addFileDetailsToDb dbconn "/main/man" ("yu", "45889947-8104-4404-a704-7d59d2bd1aed")
          updateDirAndFilename dbconn $ FilesTableRow newDir newFile uuid
          row <- getRowFromUUID dbconn uuid
          length row `shouldBe` 1
          listToMaybe row `shouldBe`
            Just (FilesTableRow newDir newFile uuid))

    describe "updateDbToMatchDirState" $ do
      it "should insert into the table files only on the system and delete from the table files not on the system" $ \pathToDb -> do
        let dirOfInterest = "/going/to"
        let ftrOne = FilesTableRow "/leaving/this"  "alone"  "855fba5b-8c27-41a0-9d0b-aa60e25f4193"
        let ftrTwo = FilesTableRow dirOfInterest  "be-created"  "91bd704f-fb43-4cd7-8eb6-3afc08086d72"
        let ftrThree = FilesTableRow dirOfInterest "ecstatic"  "dd132727-df0f-405f-bcd1-f972916bcb4e"
        let ftrFour = FilesTableRow dirOfInterest  "destination"  "1bcaf24f-8a88-4377-ae15-8beaf75a2b4f"
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

    describe "wrapDbConn" $ do
      it "will encapsulate a db connection so workhorse functions don't have to handle it" $ \pathToDb -> do
        let uuid = "e3e8bbc5-f2d5-4674-a958-c88aabf392cc"
        let dirname = "/let/there/be"
        let fname = "dragons"
        let ftr = FilesTableRow dirname fname uuid
        let f getRow = do
                rowList <- getRow uuid
                return $ fmap filename $ listToMaybe rowList
        createDbAndTables pathToDb
        withConnection pathToDb (\dbconn ->
          addFileDetailsToDb dbconn dirname (fname, uuid))
        justActualFilename <- wrapDbConn pathToDb f getRowFromUUID
        justActualFilename `shouldBe` (Just fname)
