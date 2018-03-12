module Hroamer.Main
  ( mainIO
  ) where

import Control.Exception (catch, IOException)
import Control.Monad (forM_, join, mapM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT, runReader)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer.Strict (runWriterT)
import qualified Data.DList
import Data.Text (Text, intercalate, pack)
import qualified Data.Text.IO as TIO
import Foundation hiding (intercalate)
import Foundation.Collection (mapM_)
import System.Directory (removeFile)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath.Posix
       (FilePath, (</>), takeDirectory, takeBaseName)
import System.Posix.Signals
       (Handler(Catch), installHandler, keyboardSignal, softwareStop,
        softwareTermination)
import System.Process (createProcess, proc, waitForProcess)

import Hroamer.Core (processCwd)
import Hroamer.DataStructures
       (AbsFilePath(AbsFilePath), FileOpsReadState(FileOpsReadState))
import Hroamer.FileOps (doFileOp, generateFileOps)
import Hroamer.Interfaces
       (MonadDatabase(..), MonadExit(..), MonadFileSystem(..))

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.Path as Path
import qualified Hroamer.StateFile as StateFile
import qualified Hroamer.UnsupportedPaths as UnsupportedPaths
import qualified Hroamer.Utilities as Utils

checkIfCwdIsUnderHroamerDir :: MonadWriter [Text] m => FilePath -> FilePath -> m ()
checkIfCwdIsUnderHroamerDir appDataDir cwd =
  when (Path.isWeakAncestorDir appDataDir cwd) $
    tell
      [
        "Error: You tried to use hroamer to manage " <> pack cwd <> "\n" <>
        "However, you are not allowed to use hroamer to manage " <>
        pack appDataDir <>
        " and directories below it.\nExiting."
      ]


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

newtype AppM a = AppM { runAppM :: IO a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadExit, MonadFileSystem
           , MonadDatabase
           )

mainIO :: IO ()
mainIO =  runAppM main


main :: (MonadIO m, MonadFileSystem m, MonadDatabase m, MonadExit m) => m ()
main = do
  app_data_dir <- getXdgDir
  let app_tmp_dir = app_data_dir </> "tmp"
  -- Directory for storing files 'deleted' using hroamer
  let path_to_trashcopy_dir = app_data_dir </> "trash-copy"
  createHroamerDirs app_data_dir app_tmp_dir path_to_trashcopy_dir
  let path_to_db = app_data_dir </> "hroamer.db"
  initDb path_to_db

  cwd <- getCwd
  (_, startLogs) <- runWriterT $ do
    when (Path.hasSpace cwd) $
     tell
       ["Error: cannot manage current directory because it has space characters"]
    -- Do not allow user to use hroamer to manage files that it creates
    checkIfCwdIsUnderHroamerDir app_data_dir cwd
  case startLogs of
    (_:_) -> do
      liftIO $ TIO.putStrLn $ intercalate "\n" startLogs
      exitWith $ ExitFailure 1
    [] -> return ()

  (initial_fnames_and_uuids, dirstate_filepath) <-
    liftIO $ processCwd cwd app_tmp_dir path_to_db
  let user_dirstate_filepath =
        takeDirectory dirstate_filepath </>
        ("user-" <> takeBaseName dirstate_filepath)
  copyFile dirstate_filepath user_dirstate_filepath
  liftIO $ installSignalHandlers dirstate_filepath user_dirstate_filepath
  liftIO $ letUserEditFile user_dirstate_filepath

  liftIO $ ifOnlyTrue (userMadeChanges dirstate_filepath user_dirstate_filepath) $ do
    list_of_paths_and_uuid <- join $ mapM (\(path, uuid) -> do
      resolvedPath <- runReaderT (Path.resolvePath path) cwd
      return (resolvedPath, uuid)) <$> StateFile.read user_dirstate_filepath
    let list_of_paths = fmap fst list_of_paths_and_uuid
    unsupportedPaths <-
      runReaderT (UnsupportedPaths.getUnsupportedPaths list_of_paths) cwd
    let unsupportedPathsDList = UnsupportedPaths.getErrors cwd unsupportedPaths
    if unsupportedPathsDList == Data.DList.empty
      then do
        let r = FileOpsReadState path_to_db path_to_trashcopy_dir
            initialPathsAndUuids = fmap (\(filename, uuid) ->
              (AbsFilePath (cwd </> filename), uuid))
              initial_fnames_and_uuids
        let file_op_list = runReader
                             (generateFileOps
                               list_of_paths_and_uuid
                               initialPathsAndUuids)
                             r
        HroamerDb.wrapDbConn
          path_to_db
          (\f ->
            forM_ file_op_list (\fileop -> runReaderT (doFileOp f fileop) r))
          HroamerDb.updateDirAndFilename
      else mapM_ TIO.putStrLn $ Data.DList.toList unsupportedPathsDList

  -- cleanup
  liftIO $ removeFile dirstate_filepath `catch` ignoreIOException
  liftIO $ removeFile user_dirstate_filepath `catch` ignoreIOException
  where
    ifOnlyTrue :: IO Bool -> IO () -> IO ()
    ifOnlyTrue ioBoolVal action = ioBoolVal >>=
      \yes -> when yes action
