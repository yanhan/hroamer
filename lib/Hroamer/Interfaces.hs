module Hroamer.Interfaces
  ( DatabaseOps(..)
  , FileSystemOps(..)
  , InstallSignalHandlers(..)
  , PathOps(..)
  , ScreenIO(..)
  , SystemExit(..)
  , UuidOps(..)
  , UserControl(..)
  ) where

import Control.Exception (catch)
import Control.Monad (mapM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer.Strict (runWriterT)
import qualified Data.DList
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID4
import Database.SQLite.Simple (Connection)
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
import Hroamer.DataStructures (AbsFilePath(AbsFilePath), FilePathUUIDPair)
import Hroamer.Exception (ignoreIOException)
import Hroamer.UnsupportedPaths (UPaths)
import qualified Hroamer.Path as Path
import qualified Hroamer.StateFile as StateFile
import qualified Hroamer.Utilities as Utils
import qualified Hroamer.UnsupportedPaths as UnsupportedPaths

class (Monad m) => FileSystemOps m where
  copyFile :: FilePath -> FilePath -> m ()
  createHroamerDirs :: FilePath -> FilePath -> FilePath -> m ()
  createStateFile :: FilePath -> FilePath -> [FilePathUUIDPair] -> m FilePath
  getCwd :: m FilePath
  getXdgDir :: m FilePath
  listDirectory :: FilePath -> m [FilePath]
  parseStateFile :: FilePath -> m [FilePathUUIDPair]
  rmIgnoreIOException :: FilePath -> m ()

  default copyFile :: (MonadTrans t, FileSystemOps m', m ~ t m') =>
    FilePath -> FilePath -> m ()

  copyFile src dest = lift $ copyFile src dest

  default createHroamerDirs :: (MonadTrans t, FileSystemOps m', m ~ t m') =>
    FilePath -> FilePath -> FilePath -> m ()
  createHroamerDirs appDataDir appTmpDir pathToTrashCopyDir =
    lift $ createHroamerDirs appDataDir appTmpDir pathToTrashCopyDir

  default createStateFile :: (MonadTrans t, FileSystemOps m', m ~ t m') =>
    FilePath -> FilePath -> [FilePathUUIDPair] -> m FilePath
  createStateFile cwd appTmpDir filesAndUuidsAccurate =
    lift $ createStateFile cwd appTmpDir filesAndUuidsAccurate

  default getCwd :: (MonadTrans t, FileSystemOps m', m ~ t m') => m FilePath
  getCwd = lift getCwd

  default getXdgDir :: (MonadTrans t, FileSystemOps m', m ~ t m') => m FilePath
  getXdgDir = lift getXdgDir

  default listDirectory :: (MonadTrans t, FileSystemOps m', m ~ t m') =>
    FilePath -> m [FilePath]
  listDirectory = lift . listDirectory

  default parseStateFile :: (MonadTrans t, FileSystemOps m', m ~ t m') =>
    FilePath -> m [FilePathUUIDPair]
  parseStateFile = lift . parseStateFile

  default rmIgnoreIOException :: (MonadTrans t, FileSystemOps m', m ~ t m') =>
    FilePath -> m ()
  rmIgnoreIOException = lift . rmIgnoreIOException

instance FileSystemOps IO where
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

  createStateFile = StateFile.create

  getCwd = getCurrentDirectory
  getXdgDir = getXdgDirectory XdgData "hroamer"

  listDirectory = System.Directory.listDirectory

  parseStateFile = StateFile.read

  rmIgnoreIOException file = liftIO $ removeFile file `catch` ignoreIOException


class Monad m => PathOps m where
  getUnsupportedPaths :: FilePath -> [AbsFilePath] -> m UPaths
  resolvePath :: FilePath -> FilePath -> m AbsFilePath

  default getUnsupportedPaths :: (MonadTrans t, PathOps m', m ~ t m') =>
    FilePath -> [AbsFilePath] -> m UPaths
  getUnsupportedPaths cwd absFilePaths =
    lift $ getUnsupportedPaths cwd absFilePaths

  default resolvePath :: (MonadTrans t, PathOps m', m ~ t m') =>
    FilePath -> FilePath -> m AbsFilePath
  resolvePath cwd file = lift $ resolvePath cwd file

instance PathOps IO where
  getUnsupportedPaths = UnsupportedPaths.getUnsupportedPaths
  resolvePath = Path.resolvePath


class (Monad m) => DatabaseOps m where
  getAllFilesInDir :: FilePath -> FilePath -> m [([Char], Text)]
  initDb :: FilePath -> m ()
  updateDbToMatchDirState ::
    FilePath -> FilePath -> [([Char], Text)] -> [[Char]] -> m ()
  wrapDbConn :: FilePath -> (a -> IO b) -> (Connection -> a) -> m b

  default getAllFilesInDir :: (MonadTrans t, DatabaseOps m', m ~ t m') =>
    FilePath -> FilePath -> m [([Char], Text)]
  getAllFilesInDir pathToDb dirName = lift $ getAllFilesInDir pathToDb dirName

  default initDb :: (MonadTrans t, DatabaseOps m', m ~ t m') => FilePath -> m ()
  initDb = lift . initDb

  default updateDbToMatchDirState :: (MonadTrans t, DatabaseOps m', m ~ t m') =>
    FilePath -> FilePath -> [([Char], Text)] -> [[Char]] -> m ()
  updateDbToMatchDirState cwd pathToDb filesAndUuidOnlyOnSystem filesOnlyInDb =
    lift $
      updateDbToMatchDirState
        cwd
        pathToDb
        filesAndUuidOnlyOnSystem
        filesOnlyInDb

  default wrapDbConn :: (MonadTrans t, DatabaseOps m', m ~ t m') =>
    FilePath -> (a -> IO b) -> (Connection -> a) -> m b
  wrapDbConn pathToDb workFunction dbFunction = lift $
    wrapDbConn pathToDb workFunction dbFunction

instance DatabaseOps IO where
  getAllFilesInDir = HroamerDb.getAllFilesInDir
  initDb = HroamerDb.initDb
  updateDbToMatchDirState = HroamerDb.updateDbToMatchDirState
  wrapDbConn = HroamerDb.wrapDbConn


class (Monad m) => SystemExit m where
  exitWith :: ExitCode -> m a

  default exitWith :: (MonadTrans t, SystemExit m', m ~ t m') => ExitCode -> m a
  exitWith = lift . exitWith

instance SystemExit IO where
  exitWith = System.Exit.exitWith


class (Monad m) => InstallSignalHandlers m where
  installSignalHandlers :: FilePath -> FilePath -> m ()

  default installSignalHandlers :: (MonadTrans t, InstallSignalHandlers m', m ~ t m') =>
    FilePath -> FilePath -> m ()

  installSignalHandlers dirStateFilePath userDirStateFilePath = lift $
    installSignalHandlers dirStateFilePath userDirStateFilePath

instance InstallSignalHandlers IO where
  installSignalHandlers dirStateFilePath userDirStateFilePath =
    let signals_to_handle = [keyboardSignal, softwareStop, softwareTermination]
        handler = Catch $
          removeFile dirStateFilePath `catch` ignoreIOException >>
          removeFile userDirStateFilePath `catch` ignoreIOException
    in mapM_ (\signal -> installHandler signal handler Nothing) signals_to_handle


class (Monad m) => ScreenIO m where
  printToStdout :: Text -> m ()

  default printToStdout :: (MonadTrans t, ScreenIO m', m ~ t m') => Text -> m ()

  printToStdout = lift . printToStdout

instance ScreenIO IO where
  printToStdout = TIO.putStrLn


class Monad m => UuidOps m where
  nextRandomUuid :: m UUID

  default nextRandomUuid :: (MonadTrans t, UuidOps m', m ~ t m') => m UUID
  nextRandomUuid = lift nextRandomUuid

instance UuidOps IO where
  nextRandomUuid = UUID4.nextRandom


class (Monad m) => UserControl m where
  letUserEditFile :: FilePath -> m ()
  userMadeChanges :: FilePath -> FilePath -> m Bool

  default letUserEditFile :: (MonadTrans t, UserControl m', m ~ t m') =>
    FilePath -> m ()

  letUserEditFile = lift . letUserEditFile

  default userMadeChanges :: (MonadTrans t, UserControl m', m ~ t m') =>
    FilePath -> FilePath -> m Bool

  userMadeChanges dirStateFilePath userDirStateFilePath =
    lift $ userMadeChanges dirStateFilePath userDirStateFilePath

instance UserControl IO where
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
