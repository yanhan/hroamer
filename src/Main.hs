module Main where

import Control.Exception (catch, IOException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(runReaderT), runReader)
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


main :: IO ()
main = do
  app_data_dir <- getXdgDirectory XdgData "hroamer"
  let app_tmp_dir = app_data_dir </> "tmp"
  -- Directory for storing files 'deleted' using hroamer
  let path_to_trashcopy_dir = app_data_dir </> "trash-copy"

  (allDirsOk, errorDList) <- runWriterT $ do
    -- WriterT (DList Text) IO Bool
    success_creating_app_data_dir <- Path.createDirNoForce app_data_dir
    success_creating_app_tmp_dir <- Path.createDirNoForce app_tmp_dir
    success_creating_trashcopy_dir <- Path.createDirNoForce path_to_trashcopy_dir
    return $
      success_creating_app_data_dir &&
      success_creating_app_tmp_dir && success_creating_trashcopy_dir

  if not allDirsOk
    then do
      mapM_ TIO.putStrLn $ Data.DList.toList errorDList
      TIO.putStrLn "Exiting."
      exitWith $ ExitFailure 1
    else return ()

  let path_to_db = app_data_dir </> "hroamer.db"
  HroamerDb.createDbAndTables path_to_db

  cwd <- getCurrentDirectory
  -- Do not allow user to use hroamer to manage files that it creates
  if Path.isWeakAncestorDir app_data_dir cwd
    then do
      TIO.putStrLn $
        "Error: You tried to use hroamer to manage " <> (pack cwd) <> "\n" <>
        "However, you are not allowed to use hroamer to manage " <>
        (pack app_data_dir) <>
        " and directories below it.\nExiting."
      exitWith $ ExitFailure 1
    else return ()

  (initial_fnames_and_uuids, dirstate_filepath) <-
    processCwd cwd app_tmp_dir path_to_db
  let user_dirstate_filepath =
        (takeDirectory dirstate_filepath) </>
        ("user-" <> takeBaseName dirstate_filepath)
  copyFile dirstate_filepath user_dirstate_filepath
  let signals_to_handle = [keyboardSignal, softwareStop, softwareTermination]
  let handler = Catch $
        removeFile dirstate_filepath `catch` excHandler >>
        removeFile user_dirstate_filepath `catch` excHandler
  mapM_ (\signal -> installHandler signal handler Nothing) signals_to_handle

  -- Launch text editor to let user edit the file
  editor_createprocess <- Utils.make_editor_createprocess user_dirstate_filepath
  (_, _, _, editor_process) <- createProcess editor_createprocess
  waitForProcess editor_process

  -- Compare for difference between the files
  (_, _, _, cmp_process) <-
    createProcess
      (proc "cmp" ["--silent", dirstate_filepath, user_dirstate_filepath])
  cmp_exit_code <- waitForProcess cmp_process
  case cmp_exit_code of
    ExitSuccess -> return ()
    _ -> do
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
  removeFile dirstate_filepath `catch` excHandler
  removeFile user_dirstate_filepath `catch` excHandler
  where
    excHandler :: IOException -> IO ()
    excHandler = const $ return ()


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
