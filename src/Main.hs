module Main where

import Control.Exception (catch, IOException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, runReader)
import Control.Monad.Writer.Strict (runWriterT)
import qualified Data.DList
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import Foundation
import Foundation.Collection (mapM, mapM_)
import System.Directory
       (XdgDirectory(XdgData), copyFile, getCurrentDirectory,
        getXdgDirectory, listDirectory, removeFile)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.FilePath.Posix
       (FilePath, (</>), takeDirectory, takeBaseName)
import System.Posix.Signals
       (Handler(Catch), installHandler, keyboardSignal, softwareStop,
        softwareTermination)
import System.Process (createProcess, proc, waitForProcess)

import Hroamer.DataStructures
       (FilePathUUIDPair, FileOpsReadState(FileOpsReadState))
import Hroamer.FileOps (doFileOp, generateFileOps)

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.Path as Path
import qualified Hroamer.StateFile as StateFile
import qualified Hroamer.UnsupportedPaths as UnsupportedPaths
import qualified Hroamer.Utilities as Utils

exitIfCwdIsUnderHroamerDir :: FilePath -> FilePath -> IO ()
exitIfCwdIsUnderHroamerDir appDataDir cwd =
  if Path.isWeakAncestorDir appDataDir cwd
    then do
      TIO.putStrLn $
        "Error: You tried to use hroamer to manage " <> (pack cwd) <> "\n" <>
        "However, you are not allowed to use hroamer to manage " <>
        (pack appDataDir) <>
        " and directories below it.\nExiting."
      exitWith $ ExitFailure 1
    else return ()


createHroamerDirs :: FilePath -> FilePath -> FilePath -> IO ()
createHroamerDirs appDataDir appTmpDir pathToTrashCopyDir = do
  (allDirsOk, errorDList) <- runWriterT $ do
    -- WriterT (DList Text) IO Bool
    createdAppDataDir <- Path.createDirNoForce appDataDir
    createdAppTmpDir <- Path.createDirNoForce appTmpDir
    createdTrashCopyDir <- Path.createDirNoForce pathToTrashCopyDir
    return $ createdAppDataDir && createdAppTmpDir && createdTrashCopyDir
  if not allDirsOk
    then do
      mapM_ TIO.putStrLn $ Data.DList.toList errorDList
      TIO.putStrLn "Exiting."
      exitWith $ ExitFailure 1
    else return ()


ignoreIOException :: IOException -> IO ()
ignoreIOException = const $ return ()


installSignalHandlers :: FilePath -> FilePath -> IO ()
installSignalHandlers dirStateFilePath userDirStateFilePath =
  let signals_to_handle = [keyboardSignal, softwareStop, softwareTermination]
      handler = Catch $
        removeFile dirStateFilePath `catch` ignoreIOException >>
        removeFile userDirStateFilePath `catch` ignoreIOException
  in mapM_ (\signal -> installHandler signal handler Nothing) signals_to_handle


letUserEditFile :: FilePath -> IO ()
letUserEditFile userDirStateFilePath = do
  editorCreateProcess <- Utils.makeEditorCreateProcess userDirStateFilePath
  (_, _, _, editorProcess) <- createProcess editorCreateProcess
  waitForProcess editorProcess
  return ()


userMadeChanges :: FilePath -> FilePath -> IO Bool
userMadeChanges dirStateFilePath userDirStateFilePath = do
  (_, _, _, cmpProcess) <-
    createProcess
      (proc "cmp" ["--silent", dirStateFilePath, userDirStateFilePath])
  cmpExitCode <- waitForProcess cmpProcess
  case cmpExitCode of
    ExitSuccess -> return False
    _ -> return True


main :: IO ()
main = do
  app_data_dir <- getXdgDirectory XdgData "hroamer"
  let app_tmp_dir = app_data_dir </> "tmp"
  -- Directory for storing files 'deleted' using hroamer
  let path_to_trashcopy_dir = app_data_dir </> "trash-copy"
  createHroamerDirs app_data_dir app_tmp_dir path_to_trashcopy_dir
  let path_to_db = app_data_dir </> "hroamer.db"
  HroamerDb.createDbAndTables path_to_db

  cwd <- getCurrentDirectory
  -- Do not allow user to use hroamer to manage files that it creates
  exitIfCwdIsUnderHroamerDir app_data_dir cwd

  (initial_fnames_and_uuids, dirstate_filepath) <-
    processCwd cwd app_tmp_dir path_to_db
  let user_dirstate_filepath =
        (takeDirectory dirstate_filepath) </>
        ("user-" <> takeBaseName dirstate_filepath)
  copyFile dirstate_filepath user_dirstate_filepath
  installSignalHandlers dirstate_filepath user_dirstate_filepath
  letUserEditFile user_dirstate_filepath

  ifOnlyTrue (userMadeChanges dirstate_filepath user_dirstate_filepath) $ do
    list_of_filename_and_uuid <-
      StateFile.read user_dirstate_filepath
    let list_of_filename = fmap fst list_of_filename_and_uuid
    unsupportedPaths <- UnsupportedPaths.getUnsupportedPaths cwd list_of_filename
    let unsupportedPathsDList = UnsupportedPaths.getErrors cwd unsupportedPaths
    if unsupportedPathsDList == Data.DList.empty
      then do
        let r = FileOpsReadState cwd path_to_db path_to_trashcopy_dir
        let file_op_list = runReader
                             (generateFileOps
                               list_of_filename_and_uuid
                               initial_fnames_and_uuids)
                             r
        HroamerDb.wrapDbConn
          path_to_db
          (\f ->
            forM_ file_op_list (\fileop -> runReaderT (doFileOp f fileop) r))
          HroamerDb.updateDirAndFilename
      else mapM_ TIO.putStrLn $ Data.DList.toList unsupportedPathsDList

  -- cleanup
  removeFile dirstate_filepath `catch` ignoreIOException
  removeFile user_dirstate_filepath `catch` ignoreIOException
  where
    ifOnlyTrue :: IO Bool -> IO () -> IO ()
    ifOnlyTrue ioBoolVal action = ioBoolVal >>=
      \yes -> if yes then action else return ()


processCwd :: FilePath
           -> FilePath
           -> FilePath
           -> IO ([FilePathUUIDPair], FilePath)
processCwd cwd app_tmp_dir path_to_db = do
  files__on_system <- listDirectory cwd
  files_and_uuid__in_db <- HroamerDb.getAllFilesInDir path_to_db cwd
  let files__in_db = fmap fst files_and_uuid__in_db
  let (files_only_on_system, files_only_in_db) =
        separateFilesIntoCategories files__on_system files__in_db
  files_and_uuids__only_on_system <-
    mapM
      (\fname -> do
         uuid <- fmap UUID.toText UUID4.nextRandom
         return (fname, uuid))
      (S.toList files_only_on_system)
  HroamerDb.updateDbToMatchDirState
    cwd
    path_to_db
    files_and_uuids__only_on_system
    (S.toList files_only_in_db)

  let files_and_uuids_accurate =
        filter
          (\(fname, _) -> fname `S.notMember` files_only_in_db)
          files_and_uuid__in_db <>
        files_and_uuids__only_on_system
  dirstate_filepath <- StateFile.create cwd app_tmp_dir files_and_uuids_accurate
  return (files_and_uuids_accurate, dirstate_filepath)
  where
    separateFilesIntoCategories :: [FilePath]
                                -> [[Char]]
                                -> (Set [Char], Set [Char])
    separateFilesIntoCategories files_on_system files_in_db =
      let set_system = S.fromList $ fmap toList files_on_system
          set_db = S.fromList files_in_db
      in (set_system `S.difference` set_db, set_db `S.difference` set_system)
