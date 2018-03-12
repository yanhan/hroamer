module Hroamer.Main
  ( mainIO
  ) where

import Control.Exception (catch)
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
import System.Exit (ExitCode(ExitFailure))
import System.FilePath.Posix
       (FilePath, (</>), takeDirectory, takeBaseName)

import Hroamer.Core (processCwd)
import Hroamer.DataStructures
       (AbsFilePath(AbsFilePath), FileOpsReadState(FileOpsReadState))
import Hroamer.Exception (ignoreIOException)
import Hroamer.FileOps (doFileOp, generateFileOps)
import Hroamer.Interfaces
       (MonadDatabase(..), MonadExit(..), MonadFileSystem(..),
        MonadScreenIO(..), MonadSignal(..), MonadUserControl(..))

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.Path as Path
import qualified Hroamer.StateFile as StateFile
import qualified Hroamer.UnsupportedPaths as UnsupportedPaths

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

newtype AppM a = AppM { runAppM :: IO a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadExit, MonadFileSystem
           , MonadDatabase, MonadScreenIO, MonadSignal, MonadUserControl
           )

mainIO :: IO ()
mainIO =  runAppM main


main :: ( MonadIO m
        , MonadFileSystem m
        , MonadDatabase m
        , MonadExit m
        , MonadScreenIO m
        , MonadSignal m
        , MonadUserControl m
        ) => m ()
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
      printToStdout $ intercalate "\n" startLogs
      exitWith $ ExitFailure 1
    [] -> return ()

  (initial_fnames_and_uuids, dirstate_filepath) <-
    liftIO $ processCwd cwd app_tmp_dir path_to_db
  let user_dirstate_filepath =
        takeDirectory dirstate_filepath </>
        ("user-" <> takeBaseName dirstate_filepath)
  copyFile dirstate_filepath user_dirstate_filepath
  installSignalHandlers dirstate_filepath user_dirstate_filepath
  letUserEditFile user_dirstate_filepath

  userMadeChanges dirstate_filepath user_dirstate_filepath >>= \foundChanges ->
    when foundChanges $ liftIO $ do
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
