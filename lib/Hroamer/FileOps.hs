module Hroamer.FileOps
  ( FileOp(..)
  , generateFileOps
  ) where

import Control.Monad.Reader (Reader)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Foundation

import Hroamer.DataStructures (FileOpsReadState, FilePathUUIDPair)
import Hroamer.FileOps.Internal
       (FileOp(..), genCopyOps, genTrashCopyOps)

generateFileOps
  :: [FilePathUUIDPair]
  -> [FilePathUUIDPair]
  -> Reader FileOpsReadState [FileOp]
generateFileOps list_of_filename_and_uuid initial_filenames_and_uuids = do
  let initial_filename_uuid_set = S.fromList initial_filenames_and_uuids
  let current_filename_uuid_set = S.fromList list_of_filename_and_uuid
  trashCopyOps <- genTrashCopyOps
                    initial_filename_uuid_set
                    current_filename_uuid_set
  -- At this point, UUIDs in `initial_filenames_and_uuids` are unique. Otherwise
  -- they would have violated the UNIQUE constraint on the `files.uuid` column.
  -- Hence, we can safely construct a Map indexed by UUID
  let uuid_to_trashcopyop =
        M.fromList $
        fmap (\op@(TrashCopyOp _ _ uuid) -> (uuid, op)) trashCopyOps
  let list_of_filename_uuid_to_copy =
        sortBy (\(fname_a, _) (fname_b, _) -> fname_a `compare` fname_b) $
        S.toList $
        S.difference current_filename_uuid_set initial_filename_uuid_set
  -- Map of UUID -> filename; for files that are in the current directory when
  -- the program started.
  let initial_uuid_to_filename =
        M.fromList $ fmap swap initial_filenames_and_uuids
  copyOps <- genCopyOps
               uuid_to_trashcopyop
               initial_uuid_to_filename
               list_of_filename_uuid_to_copy
  return $ trashCopyOps <> copyOps
