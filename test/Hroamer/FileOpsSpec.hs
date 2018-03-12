{-# LANGUAGE ScopedTypeVariables #-}

module Hroamer.FileOpsSpec
  ( spec
  ) where

import Control.Monad (mapM_)
import Control.Monad.Reader (runReader, runReaderT)
import Data.Map.Strict (Map, fromList, lookup)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import Foundation hiding (fromList)
import System.Directory
       (createDirectory, createDirectoryIfMissing, doesFileExist,
        doesPathExist, pathIsSymbolicLink)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hGetContents, readFile, writeFile)
import System.IO.Temp (createTempDirectory)
import System.FilePath.Posix ((</>), FilePath)
import System.Process
       (CreateProcess(std_out), StdStream(CreatePipe), createProcess,
        proc, waitForProcess)
import Test.Hspec
       (Arg, Spec, SpecWith, afterAll, beforeAll, describe, it, parallel,
        shouldBe, shouldReturn)

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.Database.Internal as HroamerDbInt

import Hroamer.Database (FilesTableRow(FilesTableRow))
import Hroamer.DataStructures
       (AbsFilePath(AbsFilePath), FileOpsReadState(FileOpsReadState),
        FileRepr(FileRepr))
import Hroamer.FileOps
       (FileOp(CopyOp, LookupDbCopyOp, TrashCopyOp), doFileOp,
        generateFileOps)
import Hroamer.FileOps.Internal (dirToTrashCopyTo)
import TestHelpers (rmrf)

doFileOpSpecTrashCopyKey :: Text
doFileOpSpecTrashCopyKey = "trashCopy"

doFileOpSpecTrashCopySourceMissingKey :: Text
doFileOpSpecTrashCopySourceMissingKey = "trashCopySourceMissing"

doFileOpSpecCopyOpSourceMissing :: Text
doFileOpSpecCopyOpSourceMissing = "copyOpSourceMissing"

doFileOpSpecCopyOpFileKey :: Text
doFileOpSpecCopyOpFileKey = "copyOpFile"

doFileOpSpecCopyOpDirKey :: Text
doFileOpSpecCopyOpDirKey = "copyOpDir"

doFileOpSpecCopyOpSymlinkKey :: Text
doFileOpSpecCopyOpSymlinkKey = "copyOpSymlink"

createDirsForTest :: IO (Map Text FilePath)
createDirsForTest = do
  trashCopyTestDir <- createTempDirectory "/tmp"  "doFileOpSpecTrashCopyTestDir"
  trashCopySourceMissingTestDir <-
    createTempDirectory "/tmp"  "doFileOpSpecTrashCopySourceMissingDir"
  copyOpSourceMissingTestDir <-
    createTempDirectory "/tmp"  "doFileOpSpecCopyOpSourceMissing"
  copyOpFileTestDir <- createTempDirectory "/tmp"  "doFileOpSpecCopyOpFileDir"
  copyOpDirTestDir <- createTempDirectory "/tmp"  "doFileOpSpecCopyOpDirDir"
  copyOpSymlinkTestDir <- createTempDirectory "/tmp"  "doFileOpSpecCopyOpSymlinkDir"
  return $ fromList [ (doFileOpSpecTrashCopyKey, trashCopyTestDir)
                    , (doFileOpSpecTrashCopySourceMissingKey, trashCopySourceMissingTestDir)
                    , (doFileOpSpecCopyOpSourceMissing, copyOpSourceMissingTestDir)
                    , (doFileOpSpecCopyOpFileKey, copyOpFileTestDir)
                    , (doFileOpSpecCopyOpDirKey, copyOpDirTestDir)
                    , (doFileOpSpecCopyOpSymlinkKey, copyOpSymlinkTestDir)
                    ]

spec :: Spec
spec = parallel $ beforeAll createDirsForTest $ afterAll rmrf $ do
  describe "generateFileOps" $
    it "should generate a list of FileOp; with TrashCopyOp first, followed by everything else (sorted by filename)" $ \_ ->
      let cwd = "/boiling/water"
          anotherDirOne = "/aubergine/local/axe2"
          anotherDirTwo = "/under/cover/of/darkness"
          trashCopyDir = "/rising/turtle"
          -- files that will not be touched
          safeOneFilename = "avocado"
          safeOneUuid = "012fe19a-b303-4b1d-bf4d-4422e938f7d4"
          safeOne = (AbsFilePath $ cwd </> safeOneFilename, safeOneUuid)
          safeTwoFilename = "barley"
          safeTwoUuid = "43012c07-6279-4a52-9369-25a422bd2df0"
          safeTwo = (AbsFilePath $ cwd </> safeTwoFilename, safeTwoUuid)
          -- files that will be removed
          --
          -- first file
          toRemoveOneFilename = "crabs"
          toRemoveOneUuid = "c8055958-fffb-40d2-89f6-73b6625667b7"
          toRemoveOne = (AbsFilePath $ cwd </> toRemoveOneFilename, toRemoveOneUuid)
          -- second file
          toRemoveTwoFilename = "allspark"
          toRemoveTwoUuid = "9f7f3317-0f0f-4d4d-887d-18446353c909"
          toRemoveTwo = (AbsFilePath $ cwd </> toRemoveTwoFilename, toRemoveTwoUuid)
          -- files that will be copied
          --
          -- first new file
          toCopyOneFilename = "courgette"
          toCopyOneUuid = safeTwoUuid
          toCopyOne = (AbsFilePath $ cwd </> toCopyOneFilename, toCopyOneUuid)
          -- second new file
          toCopyTwoFilename = "dill"
          toCopyTwoUuid = safeTwoUuid
          toCopyTwo = (AbsFilePath $ cwd </> toCopyTwoFilename, toCopyTwoUuid)
          -- files that will be renamed
          toRenameFilename = "aubergine"
          toRenameUuid = toRemoveTwoUuid
          toRename = (AbsFilePath $ cwd </> toRenameFilename, toRenameUuid)
          -- files that have to be looked up
          ---- copy to cwd
          toLookupFilename = "egg"
          toLookupUuid = "c95581e7-0b48-4b68-b780-13a0e20c979c"
          toLookup = (AbsFilePath $ cwd </> toLookupFilename, toLookupUuid)
          ---- copy to another directory
          toLookupFilenameTwo = "soursop"
          toLookupUuidTwo = "1b6b4834-b011-4c1f-9b17-0d6d5d978466"
          toLookupTwo = ( AbsFilePath $ anotherDirTwo </> toLookupFilenameTwo
                        , toLookupUuidTwo
                        )
          -- files to copy
          toCopyExtOneFilename = "kiln"
          toCopyExtOne = ( AbsFilePath $ anotherDirOne </> toCopyExtOneFilename
                         , safeTwoUuid
                         )
          toCopyExtTwoFilename = "zeppelin"
          toCopyExtTwo =  ( AbsFilePath $ anotherDirTwo </> toCopyExtTwoFilename
                          , safeOneUuid
                          )
          --
          initial = [toRemoveOne, toRemoveTwo, safeOne, safeTwo]
          current = [ toLookup
                    , toCopyOne
                    , toRename
                    , safeOne
                    , safeTwo
                    , toCopyTwo
                    , toCopyExtOne
                    , toCopyExtTwo
                    , toLookupTwo
                    ]
          r = FileOpsReadState "" trashCopyDir
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
                     -- destination directory (anotherDirOne) is alphabetically
                     -- before cwd, so it comes before every FileOp with cwd as
                     -- the destination dir
                     , CopyOp (FileRepr cwd safeTwoFilename)
                              (FileRepr anotherDirOne toCopyExtOneFilename)
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
                     -- destination directory (anotherDirTwo) is alphabetically
                     -- after cwd, so these FileOp with destination directory of
                     -- anotherTwo comes after every FileOp with cwd as the
                     -- destination dir
                     --
                     -- Those destination files in the same destination directory
                     -- are then arranged in alphabetical order
                     , LookupDbCopyOp (FileRepr anotherDirTwo toLookupFilenameTwo)
                                      toLookupUuidTwo
                     , CopyOp (FileRepr cwd safeOneFilename)
                              (FileRepr anotherDirTwo toCopyExtTwoFilename)
                     ]
          actual = runReader (generateFileOps current initial) r
      in actual `shouldBe` expected

  describe "doFileOp" $ do
    describe "for TrashCopyOp" $ do
      it "should move the existing path to a new directory under the trash copy directory" $
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
          HroamerDb.initDb pathToDb
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
                (FileOpsReadState pathToDb pathToTrashCopyDir)
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

      it "should handle exception (instead of crashing) when source is missing" $
        \mapOfTempDirs -> do
          let tempDir = fromJust $
                lookup doFileOpSpecTrashCopySourceMissingKey mapOfTempDirs
              pathToDb = tempDir </> "db"
              srcDir = tempDir </> "groggy"
              srcFile = "captain"
              srcFileRepr = FileRepr srcDir srcFile
              srcUuid = "55147bce-d68c-4762-9166-a0faae4c338d"
              destDir = tempDir </> "toxic"
              destFile = "ducky"
              destFileRepr = FileRepr destDir destFile
          HroamerDb.initDb pathToDb
          HroamerDb.wrapDbConn
            pathToDb
            (\addFileDetailsToDb -> addFileDetailsToDb srcDir (srcFile, srcUuid))
            HroamerDbInt.addFileDetailsToDb
          createDirectory srcDir
          runReaderT
            (doFileOp undefined (TrashCopyOp srcFileRepr destFileRepr srcUuid))
            undefined
          -- check that database state is the same
          HroamerDb.getAllFilesInDir pathToDb srcDir `shouldReturn`
            [(srcFile, srcUuid)]

    describe "for CopyOp" $ do
      it "should quietly handle exception raised by pathIsSymbolicLink when the source file is missing" $
        \mapOfTempDirs -> do
          let tempDir = fromJust $
                lookup doFileOpSpecCopyOpSourceMissing mapOfTempDirs
              pathToDb = tempDir </> "db"
              srcDir = tempDir </> "fiery"
              srcFile = "mia"
              srcUuid = "fce9e325-831d-45b3-a514-771a23e467dc"
              destDir = tempDir </> "toothpaste"
              destFile = "ok"
              srcFileRepr = FileRepr srcDir srcFile
              destFileRepr = FileRepr destDir destFile
          createDirectory srcDir
          HroamerDb.initDb pathToDb
          HroamerDb.wrapDbConn
            pathToDb
            (\addFileDetailsToDb -> addFileDetailsToDb srcDir (srcFile, srcUuid))
            HroamerDbInt.addFileDetailsToDb
          runReaderT
            (doFileOp undefined (CopyOp srcFileRepr destFileRepr))
            undefined
          HroamerDb.getAllFilesInDir pathToDb srcDir `shouldReturn`
            [(srcFile, srcUuid)]

      it "for existing file, it should copy the file to its destination" $
        \mapOfTempDirs -> do
          let tempDir = fromJust $ lookup doFileOpSpecCopyOpFileKey mapOfTempDirs
              pathToDb = tempDir </> "copyOpFileDb"
              cwd = tempDir </> "happy"
              dirOne = tempDir </> "watch-later"
              dirTwo = tempDir </> "jumping"
              --
              srcDir = cwd
              srcFile = "as-a-fiddle"
              srcFileRepr = FileRepr srcDir srcFile
              srcUuid = "57ff0289-e5aa-4356-9dff-606a4c4ee832"
              destDirOne = tempDir </> "alex"
              destFileOne = "is-similar"
              destFileOneRepr = FileRepr destDirOne destFileOne
              destFileOnePath = destDirOne </> destFileOne
              fileContents = "Ho! Ho! Ho! Merry Christmas!"
              copyOpOne = CopyOp srcFileRepr destFileOneRepr
              -- copy from cwd to another dir
              destDirTwo = dirOne
              destFileTwo = "tiling"
              destFileTwoRepr = FileRepr destDirTwo destFileTwo
              destFileTwoPath = destDirTwo </> destFileTwo
              copyOpTwo = CopyOp srcFileRepr destFileTwoRepr
              -- copy from another dir to cwd
              srcDirTwo = dirTwo
              srcFileTwo = "lookup"
              srcFileTwoPath = srcDirTwo </> srcFileTwo
              srcFileTwoRepr = FileRepr srcDirTwo srcFileTwo
              srcFileTwoUuid = "0bfaf388-3eac-42e1-b7a4-5dc54342e703"
              srcFileTwoContents = "gogogogo"
              destDirThree = cwd
              destFileThree = "tomatoes"
              destFileThreePath = destDirThree </> destFileThree
              destFileThreeRepr = FileRepr destDirThree destFileThree
              copyOpThree = CopyOp srcFileTwoRepr destFileThreeRepr
              -- copy from another dir to another dir
              srcDirThree = dirOne
              srcFileThree = "christmas_tree"
              srcFileThreePath = srcDirThree </> srcFileThree
              srcFileThreeContents = "hard work! dedication!"
              srcFileThreeRepr = FileRepr srcDirThree srcFileThree
              srcFileThreeUuid = "68fc3212-6ef1-4351-8a2d-5231e8b7a856"
              destDirFour = dirTwo
              destFileFour = "green-spectacles.doc"
              destFileFourPath = destDirFour </> destFileFour
              destFileFourRepr = FileRepr destDirFour destFileFour
              copyOpFour = CopyOp srcFileThreeRepr destFileFourRepr
          HroamerDb.initDb pathToDb
          mapM_ createDirectory [cwd, dirOne, dirTwo]
          writeFile (srcDir </> srcFile) fileContents
          writeFile srcFileTwoPath srcFileTwoContents
          writeFile srcFileThreePath srcFileThreeContents
          HroamerDb.wrapDbConn
            pathToDb
            (\addFileDetailsToDb -> do
              addFileDetailsToDb srcDir (srcFile, srcUuid)
              addFileDetailsToDb srcDirTwo (srcFileTwo, srcFileTwoUuid)
              addFileDetailsToDb srcDirThree (srcFileThree, srcFileThreeUuid))
            HroamerDbInt.addFileDetailsToDb
          createDirectory destDirOne
          doesPathExist destFileOnePath `shouldReturn` False
          runReaderT
            (doFileOp undefined copyOpOne)
            undefined
          doesFileExist destFileOnePath `shouldReturn` True
          doesFileExist (srcDir </> srcFile) `shouldReturn` True
          readFile destFileOnePath `shouldReturn` fileContents
          -- copy from cwd to another dir
          doesPathExist destFileTwoPath `shouldReturn` False
          runReaderT (doFileOp undefined copyOpTwo) undefined
          doesFileExist destFileTwoPath `shouldReturn` True
          readFile destFileTwoPath `shouldReturn` fileContents
          -- copy from another dir to cwd
          doesPathExist destFileThreePath `shouldReturn` False
          runReaderT (doFileOp undefined copyOpThree) undefined
          doesFileExist destFileThreePath `shouldReturn` True
          readFile destFileThreePath `shouldReturn` srcFileTwoContents
          -- copy from another dir to another dir
          doesPathExist destFileFourPath `shouldReturn` False
          runReaderT (doFileOp undefined copyOpFour) undefined
          doesFileExist destFileFourPath `shouldReturn` True
          readFile destFileFourPath `shouldReturn` srcFileThreeContents

      it "for existing dir, it should copy the dir to its destination" $
        \mapOfTempDirs -> do
          let tempDir = fromJust $ lookup doFileOpSpecCopyOpDirKey mapOfTempDirs
              pathToDb = tempDir </> "dbdbddb"
              srcContainingDir = tempDir </> "ground"
              srcDir = "truth"
              srcFile = "is-here"
              srcFileRepr = FileRepr srcContainingDir srcDir
              srcUuid = "365cde5f-dd45-4854-9ca8-2417ffaba5ac"
              srcDirPath = srcContainingDir </> srcDir
              srcFilePath = srcDirPath </> srcFile
              destContainingDir = tempDir </> "fiery"
              destDir = "water"
              destFileRepr = FileRepr destContainingDir destDir
              destDirPath = destContainingDir </> destDir
              destFilePath = destDirPath </> srcFile
              fileContents = "Make some noise if you... are... ready!!!!"
          HroamerDb.initDb pathToDb
          createDirectoryIfMissing True srcDirPath
          writeFile srcFilePath fileContents
          createDirectory destContainingDir
          HroamerDb.wrapDbConn
            pathToDb
            (\addFileDetailsToDb ->
              addFileDetailsToDb srcContainingDir (srcDir, srcUuid))
            HroamerDbInt.addFileDetailsToDb
          doesPathExist destDirPath `shouldReturn` False
          doesPathExist destFilePath `shouldReturn` False
          runReaderT
            (doFileOp undefined (CopyOp srcFileRepr destFileRepr))
            undefined
          doesFileExist srcFilePath `shouldReturn` True
          doesFileExist destFilePath `shouldReturn` True
          readFile destFilePath `shouldReturn` fileContents

      it "for symlink, it should create a new symlink" $
        \mapOfTempDirs -> do
          let tempDir = fromJust $ lookup doFileOpSpecCopyOpSymlinkKey mapOfTempDirs
              pathToDb = tempDir </> "symlinkdb"
              srcDir = tempDir </> "heylo"
              srcFile = "mylink"
              srcPath = srcDir </> srcFile
              srcUuid = "e951540f-4202-4fd1-b544-eba5b5cba0b3"
              srcFileRepr = FileRepr srcDir srcFile
              symlinkTarget = "/a/fairy/tale/with/some/castle"
              destDir = tempDir </> "aiyo"
              destFile = "pointer"
              destPath = destDir </> destFile
              destFileRepr = FileRepr destDir destFile
          HroamerDb.initDb pathToDb
          HroamerDb.wrapDbConn
            pathToDb
            (\addFileDetailsToDb -> addFileDetailsToDb srcDir (srcFile, srcUuid))
            HroamerDbInt.addFileDetailsToDb
          -- Create the symlink
          mapM_ createDirectory [srcDir, destDir]
          (_, _, _, ph) <- createProcess (proc "ln" ["-s", symlinkTarget, srcPath])
          exitCode <- waitForProcess ph
          case exitCode of
            ExitSuccess -> return ()
            _ -> False `shouldBe` True
          pathIsSymbolicLink srcPath `shouldReturn` True
          doesPathExist destPath `shouldReturn` False
          runReaderT
            (doFileOp undefined (CopyOp srcFileRepr destFileRepr))
            undefined
          pathIsSymbolicLink destPath `shouldReturn` True
          (_, Just stdoutHandle, _, ph) <-
            createProcess (proc "readlink" [destPath]){ std_out=CreatePipe }
          exitCode <- waitForProcess ph
          case exitCode of
            ExitSuccess ->
              hGetContents stdoutHandle `shouldReturn` (symlinkTarget <> "\n")
            _ -> False `shouldBe` True
