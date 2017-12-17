{-# LANGUAGE ScopedTypeVariables #-}

module Hroamer.FileOpsSpec
  ( spec
  ) where

import Control.Monad.Reader (runReader, runReaderT)
import Data.Map.Strict (Map, fromList, lookup)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Data.Traversable (Traversable(traverse))
import Foundation hiding (fromList)
import System.Directory
       (createDirectory, doesFileExist, doesPathExist)
import System.IO (readFile, writeFile)
import System.IO.Temp (createTempDirectory)
import System.FilePath.Posix ((</>), FilePath)
import System.Process (createProcess, proc, waitForProcess)
import Test.Hspec
       (Arg, Spec, SpecWith, afterAll, beforeAll, describe, it, parallel,
        shouldBe, shouldReturn)

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.Database.Internal as HroamerDbInt

import Hroamer.Database (FilesTableRow(FilesTableRow))
import Hroamer.DataStructures
       (FileOpsReadState(FileOpsReadState), FileRepr(FileRepr))
import Hroamer.FileOps
       (FileOp(CopyOp, LookupDbCopyOp, TrashCopyOp), doFileOp,
        generateFileOps)
import Hroamer.FileOps.Internal (dirToTrashCopyTo)

doFileOpSpecTrashCopyKey :: Text
doFileOpSpecTrashCopyKey = "trashCopy"

doFileOpSpecCopyOpFileKey :: Text
doFileOpSpecCopyOpFileKey = "copyOpFile"

createDirsForTest :: IO (Map Text FilePath)
createDirsForTest = do
  trashCopyTestDir <- createTempDirectory "/tmp"  "doFileOpSpecTrashCopyTestDir"
  copyOpFileTestDir <- createTempDirectory "/tmp"  "doFileOpSpecCopyOpFileDir"
  return $ fromList [ (doFileOpSpecTrashCopyKey, trashCopyTestDir)
                    , (doFileOpSpecCopyOpFileKey, copyOpFileTestDir)
                    ]

rmrf :: Map Text FilePath -> IO ()
rmrf tempDirs = do
  traverse (\dir -> do
    let rmrfProc = proc "/bin/rm" ["-rf", toList dir]
    (_, _, _, ph) <- createProcess rmrfProc
    waitForProcess ph) tempDirs
  return ()

spec :: Spec
spec = parallel $ beforeAll createDirsForTest $ afterAll rmrf $ do
  describe "generateFileOps" $
    it "should generate a list of FileOp; with TrashCopyOp first, followed by everything else (sorted by filename)" $ \_ ->
      let cwd = "/boiling/water"
          trashCopyDir = "/rising/turtle"
          -- files that will not be touched
          safeOneFilename = "avocado"
          safeOneUuid = "012fe19a-b303-4b1d-bf4d-4422e938f7d4"
          safeOne = (safeOneFilename, safeOneUuid)
          safeTwoFilename = "barley"
          safeTwoUuid = "43012c07-6279-4a52-9369-25a422bd2df0"
          safeTwo = (safeTwoFilename, safeTwoUuid)
          -- files that will be removed
          --
          -- first file
          toRemoveOneFilename = "crabs"
          toRemoveOneUuid = "c8055958-fffb-40d2-89f6-73b6625667b7"
          toRemoveOne = (toRemoveOneFilename, toRemoveOneUuid)
          -- second file
          toRemoveTwoFilename = "allspark"
          toRemoveTwoUuid = "9f7f3317-0f0f-4d4d-887d-18446353c909"
          toRemoveTwo = (toRemoveTwoFilename, toRemoveTwoUuid)
          -- files that will be copied
          --
          -- first new file
          toCopyOneFilename = "courgette"
          toCopyOneUuid = safeTwoUuid
          toCopyOne = (toCopyOneFilename, toCopyOneUuid)
          -- second new file
          toCopyTwoFilename = "dill"
          toCopyTwoUuid = safeTwoUuid
          toCopyTwo = (toCopyTwoFilename, toCopyTwoUuid)
          -- files that will be renamed
          toRenameFilename = "aubergine"
          toRenameUuid = toRemoveTwoUuid
          toRename = (toRenameFilename, toRenameUuid)
          -- file that has to be looked up
          toLookupFilename = "egg"
          toLookupUuid = "c95581e7-0b48-4b68-b780-13a0e20c979c"
          toLookup = (toLookupFilename, toLookupUuid)
          --
          initial = [toRemoveOne, toRemoveTwo, safeOne, safeTwo]
          current = [toLookup, toCopyOne, toRename, safeOne, safeTwo, toCopyTwo]
          r = FileOpsReadState cwd "" trashCopyDir
          expected = [ TrashCopyOp
                         (FileRepr cwd toRemoveTwoFilename)
                         (FileRepr (dirToTrashCopyTo trashCopyDir toRemoveTwoUuid)
                                   toRemoveTwoFilename)
                         toRemoveTwoUuid
                     , TrashCopyOp
                         (FileRepr cwd toRemoveOneFilename)
                         (FileRepr (dirToTrashCopyTo trashCopyDir toRemoveOneUuid)
                                   toRemoveOneFilename)
                         toRemoveOneUuid
                     , CopyOp (FileRepr (dirToTrashCopyTo trashCopyDir toRemoveTwoUuid)
                                        toRemoveTwoFilename)
                              (FileRepr cwd toRenameFilename)
                     , CopyOp
                         (FileRepr cwd safeTwoFilename)
                         (FileRepr cwd toCopyOneFilename)
                     , CopyOp
                         (FileRepr cwd safeTwoFilename)
                         (FileRepr cwd toCopyTwoFilename)
                     , LookupDbCopyOp (FileRepr cwd toLookupFilename) toLookupUuid
                     ]
          actual = runReader (generateFileOps current initial) r
      in actual `shouldBe` expected

  describe "doFileOp" $ do
    it "for TrashCopyOp, should move the existing path to a new directory under the trash copy directory" $
      \mapOfTempDirs -> do
        let tempDir = fromJust $ lookup doFileOpSpecTrashCopyKey mapOfTempDirs
            pathToDb = tempDir </> "filesdb"
            srcDir = tempDir </> "svenson"
            srcFile = "asks"
            srcUuid = "e582b948-d554-46e1-8799-a24fbc0c7207"
            srcFileRepr = FileRepr srcDir  srcFile
            pathToTrashCopyDir = tempDir </> "nord"
            destDir = pathToTrashCopyDir </> (toList srcUuid)
            destFile = "runReaderT"
            destFileRepr = FileRepr destDir  destFile
        HroamerDb.createDbAndTables pathToDb
        createDirectory srcDir
        writeFile (srcDir </> srcFile) "move asks to readerT"
        createDirectory pathToTrashCopyDir
        HroamerDb.wrapDbConn pathToDb (\addFileDetailsToDb -> do
            addFileDetailsToDb srcDir (srcFile, srcUuid)
          ) HroamerDbInt.addFileDetailsToDb
        HroamerDb.wrapDbConn
          pathToDb
          (\updateDirAndFilename ->
            runReaderT
              (doFileOp
                updateDirAndFilename
                (TrashCopyOp srcFileRepr destFileRepr srcUuid))
              (FileOpsReadState destDir pathToDb pathToTrashCopyDir)
          )
          HroamerDb.updateDirAndFilename
        -- assertions
        doesFileExist (srcDir </> srcFile) `shouldReturn` False
        doesFileExist (destDir </> destFile) `shouldReturn` True
        -- check database state
        HroamerDb.wrapDbConn
          pathToDb
          (\getRowFromUUID -> do
            l <- getRowFromUUID srcUuid
            case l of
              [] -> False `shouldBe` True
              [(FilesTableRow dir filename uuid)] -> do
                dir `shouldBe` destDir
                filename `shouldBe` destFile
                uuid `shouldBe` srcUuid
              _ -> False `shouldBe` False)
          HroamerDb.getRowFromUUID

    it "for CopyOp for existing file, it should copy the file to its destination" $
      \mapOfTempDirs -> do
        let tempDir = fromJust $ lookup doFileOpSpecCopyOpFileKey mapOfTempDirs
            pathToDb = tempDir </> "copyOpFileDb"
            srcDir = tempDir </> "happy"
            srcFile = "as-a-fiddle"
            srcFileRepr = FileRepr srcDir srcFile
            srcUuid = "57ff0289-e5aa-4356-9dff-606a4c4ee832"
            destDir = tempDir </> "alex"
            destFile = "is-similar"
            destFileRepr = FileRepr destDir destFile
            destFilePath = destDir </> destFile
            fileContents = "Ho! Ho! Ho! Merry Christmas!"
            copyOp = CopyOp srcFileRepr destFileRepr
        HroamerDb.createDbAndTables pathToDb
        createDirectory srcDir
        writeFile (srcDir </> srcFile) fileContents
        HroamerDb.wrapDbConn
          pathToDb
          (\addFileDetailsToDb -> addFileDetailsToDb srcDir (srcFile, srcUuid))
          HroamerDbInt.addFileDetailsToDb
        createDirectory destDir
        doesPathExist destFilePath `shouldReturn` False
        runReaderT
          (doFileOp undefined copyOp)
          (FileOpsReadState "" "" "")
        doesFileExist destFilePath `shouldReturn` True
        doesFileExist (srcDir </> srcFile) `shouldReturn` True
        readFile destFilePath `shouldReturn` fileContents
