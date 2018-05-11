module Hroamer.Core
  ( processCwd
  ) where

import Control.Monad (mapM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import Foundation
import System.FilePath.Posix (FilePath)

import Hroamer.DataStructures (FilePathUUIDPair)
import Hroamer.Interfaces
       (DatabaseOps(..), FileSystemOps(..), UuidOps(..))
import Hroamer.Path (hasSpace)

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.StateFile as StateFile

processCwd :: ( MonadIO m
              , DatabaseOps m
              , FileSystemOps m
              , UuidOps m
              ) => FilePath
                -> FilePath
                -> FilePath
                -> m ([FilePathUUIDPair], FilePath)
processCwd cwd appTmpDir pathToDb = do
  filesOnSystem <- fmap (fmap (filter (not . hasSpace))) listDirectory cwd
  filesAndUuidInDb <- getAllFilesInDir pathToDb cwd
  let filesInDb = fmap fst filesAndUuidInDb
  let (filesOnlyOnSystem, filesOnlyInDb) =
        separateFilesIntoCategories filesOnSystem filesInDb
  filesAndUuidsOnlyOnSystem <-
    mapM
      (\fname -> do
         uuid <- fmap UUID.toText nextRandomUuid
         return (fname, uuid))
      (S.toList filesOnlyOnSystem)
  updateDbToMatchDirState
    cwd
    pathToDb
    filesAndUuidsOnlyOnSystem
    (S.toList filesOnlyInDb)

  let filesAndUuidsAccurate =
        filter
          (\(fname, _) -> fname `S.notMember` filesOnlyInDb)
          filesAndUuidInDb <>
        filesAndUuidsOnlyOnSystem
  dirStateFilePath <- liftIO $
    StateFile.create cwd appTmpDir filesAndUuidsAccurate
  return (filesAndUuidsAccurate, dirStateFilePath)
  where
    separateFilesIntoCategories :: [FilePath]
                                -> [[Char]]
                                -> (Set [Char], Set [Char])
    separateFilesIntoCategories filesOnSystem filesInDb =
      let setSystem = S.fromList $ fmap toList filesOnSystem
          setDb = S.fromList filesInDb
      in (setSystem `S.difference` setDb, setDb `S.difference` setSystem)
