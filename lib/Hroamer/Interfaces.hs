module Hroamer.Interfaces
  ( MonadDatabase(..)
  , MonadFileSystem(..)
  ) where

import Control.Monad (mapM_, when)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer.Strict (runWriterT)
import qualified Data.DList
import qualified Data.Text.IO as TIO
import Foundation
import System.Directory
       (XdgDirectory(XdgData), getCurrentDirectory, getXdgDirectory)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.FilePath.Posix (FilePath)

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.Path as Path

class (Monad m) => MonadFileSystem m where
  createHroamerDirs :: FilePath -> FilePath -> FilePath -> m ()
  getCwd :: m FilePath
  getXdgDir :: m FilePath

  default createHroamerDirs :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    FilePath -> FilePath -> FilePath -> m ()
  createHroamerDirs appDataDir appTmpDir pathToTrashCopyDir =
    lift $ createHroamerDirs appDataDir appTmpDir pathToTrashCopyDir

  default getCwd :: (MonadTrans t, MonadFileSystem m', m ~ t m') => m FilePath
  getCwd = lift getCwd

  default getXdgDir :: (MonadTrans t, MonadFileSystem m', m ~ t m') => m FilePath
  getXdgDir = lift getXdgDir

instance MonadFileSystem IO where
  createHroamerDirs appDataDir appTmpDir pathToTrashCopyDir = do
    (allDirsOk, errorDList) <- runWriterT $ do
      -- WriterT (DList Text) IO Bool
      createdAppDataDir <- Path.createDirNoForce appDataDir
      createdAppTmpDir <- Path.createDirNoForce appTmpDir
      createdTrashCopyDir <- Path.createDirNoForce pathToTrashCopyDir
      return $ createdAppDataDir && createdAppTmpDir && createdTrashCopyDir
    when allDirsOk $ do
      mapM_ TIO.putStrLn $ Data.DList.toList errorDList
      TIO.putStrLn "Exiting."
      exitWith $ ExitFailure 1

  getCwd = getCurrentDirectory
  getXdgDir = getXdgDirectory XdgData "hroamer"


class (Monad m) => MonadDatabase m where
  initDb :: FilePath -> m ()

  default initDb :: (MonadTrans t, MonadDatabase m', m ~ t m') => FilePath -> m ()
  initDb = lift . initDb

instance MonadDatabase IO where
  initDb = HroamerDb.initDb
