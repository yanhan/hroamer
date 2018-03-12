module Hroamer.Interfaces
  ( MonadDatabase(..)
  , MonadExit(..)
  , MonadFileSystem(..)
  , MonadSignal(..)
  , MonadScreenIO(..)
  , MonadUserControl(..)
  ) where

import Control.Exception (catch)
import Control.Monad (mapM_, unless)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer.Strict (runWriterT)
import qualified Data.DList
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Foundation
import qualified System.Directory
import System.Directory
       (XdgDirectory(XdgData), getCurrentDirectory, getXdgDirectory,
        removeFile)
import qualified System.Exit
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath.Posix (FilePath)
import System.Posix.Signals
       (Handler(Catch), installHandler, keyboardSignal, softwareStop,
        softwareTermination)
import System.Process (createProcess, proc, waitForProcess)

import qualified Hroamer.Database as HroamerDb
import Hroamer.Exception (ignoreIOException)
import qualified Hroamer.Path as Path
import qualified Hroamer.Utilities as Utils

class (Monad m) => MonadFileSystem m where
  copyFile :: FilePath -> FilePath -> m ()
  createHroamerDirs :: FilePath -> FilePath -> FilePath -> m ()
  getCwd :: m FilePath
  getXdgDir :: m FilePath

  default copyFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    FilePath -> FilePath -> m ()

  copyFile src dest = lift $ copyFile src dest

  default createHroamerDirs :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    FilePath -> FilePath -> FilePath -> m ()
  createHroamerDirs appDataDir appTmpDir pathToTrashCopyDir =
    lift $ createHroamerDirs appDataDir appTmpDir pathToTrashCopyDir

  default getCwd :: (MonadTrans t, MonadFileSystem m', m ~ t m') => m FilePath
  getCwd = lift getCwd

  default getXdgDir :: (MonadTrans t, MonadFileSystem m', m ~ t m') => m FilePath
  getXdgDir = lift getXdgDir

instance MonadFileSystem IO where
  copyFile = System.Directory.copyFile

  createHroamerDirs appDataDir appTmpDir pathToTrashCopyDir = do
    (allDirsOk, errorDList) <- runWriterT $ do
      -- WriterT (DList Text) IO Bool
      createdAppDataDir <- Path.createDirNoForce appDataDir
      createdAppTmpDir <- Path.createDirNoForce appTmpDir
      createdTrashCopyDir <- Path.createDirNoForce pathToTrashCopyDir
      return $ createdAppDataDir && createdAppTmpDir && createdTrashCopyDir
    unless allDirsOk $ do
      mapM_ TIO.putStrLn $ Data.DList.toList errorDList
      TIO.putStrLn "Exiting."
      System.Exit.exitWith $ ExitFailure 1

  getCwd = getCurrentDirectory
  getXdgDir = getXdgDirectory XdgData "hroamer"


class (Monad m) => MonadDatabase m where
  initDb :: FilePath -> m ()

  default initDb :: (MonadTrans t, MonadDatabase m', m ~ t m') => FilePath -> m ()
  initDb = lift . initDb

instance MonadDatabase IO where
  initDb = HroamerDb.initDb


class (Monad m) => MonadExit m where
  exitWith :: ExitCode -> m a

  default exitWith :: (MonadTrans t, MonadExit m', m ~ t m') => ExitCode -> m a
  exitWith = lift . exitWith

instance MonadExit IO where
  exitWith = System.Exit.exitWith


class (Monad m) => MonadSignal m where
  installSignalHandlers :: FilePath -> FilePath -> m ()

  default installSignalHandlers :: (MonadTrans t, MonadSignal m', m ~ t m') =>
    FilePath -> FilePath -> m ()

  installSignalHandlers dirStateFilePath userDirStateFilePath = lift $
    installSignalHandlers dirStateFilePath userDirStateFilePath

instance MonadSignal IO where
  installSignalHandlers dirStateFilePath userDirStateFilePath =
    let signals_to_handle = [keyboardSignal, softwareStop, softwareTermination]
        handler = Catch $
          removeFile dirStateFilePath `catch` ignoreIOException >>
          removeFile userDirStateFilePath `catch` ignoreIOException
    in mapM_ (\signal -> installHandler signal handler Nothing) signals_to_handle


class (Monad m) => MonadScreenIO m where
  printToStdout :: Text -> m ()

  default printToStdout :: (MonadTrans t, MonadScreenIO m', m ~ t m') => Text -> m ()

  printToStdout = lift . printToStdout

instance MonadScreenIO IO where
  printToStdout = TIO.putStrLn


class (Monad m) => MonadUserControl m where
  letUserEditFile :: FilePath -> m ()
  userMadeChanges :: FilePath -> FilePath -> m Bool

  default letUserEditFile :: (MonadTrans t, MonadUserControl m', m ~ t m') =>
    FilePath -> m ()

  letUserEditFile = lift . letUserEditFile

  default userMadeChanges :: (MonadTrans t, MonadUserControl m', m ~ t m') =>
    FilePath -> FilePath -> m Bool

  userMadeChanges dirStateFilePath userDirStateFilePath =
    lift $ userMadeChanges dirStateFilePath userDirStateFilePath

instance MonadUserControl IO where
  letUserEditFile userDirStateFilePath = do
    editorCreateProcess <- Utils.makeEditorCreateProcess userDirStateFilePath
    (_, _, _, editorProcess) <- createProcess editorCreateProcess
    waitForProcess editorProcess
    return ()

  userMadeChanges dirStateFilePath userDirStateFilePath = do
    (_, _, _, cmpProcess) <-
      createProcess
        (proc "cmp" ["--silent", dirStateFilePath, userDirStateFilePath])
    cmpExitCode <- waitForProcess cmpProcess
    case cmpExitCode of
      ExitSuccess -> return False
      _ -> return True
