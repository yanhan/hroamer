module Hroamer.Core
  ( processCwd
  ) where

import Control.Monad (mapM)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import Foundation
import System.Directory (listDirectory)
import System.FilePath.Posix (FilePath)

import Hroamer.DataStructures (FilePathUUIDPair)

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.StateFile as StateFile

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
