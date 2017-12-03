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
generateFileOps listOfFilenamesAndUuids initialFilenamesAndUuids = do
  let initialFilenamesAndUuidsSet = S.fromList initialFilenamesAndUuids
  let currentFilenamesAndUuidsSet = S.fromList listOfFilenamesAndUuids
  trashCopyOps <- genTrashCopyOps
                    initialFilenamesAndUuidsSet
                    currentFilenamesAndUuidsSet
  -- At this point, UUIDs in `initialFilenamesAndUuids` are unique. Otherwise
  -- they would have violated the UNIQUE constraint on the `files.uuid` column.
  -- Hence, we can safely construct a Map indexed by UUID
  let uuidToTrashCopyFileRepr =
        M.fromList $
        fmap
          (\(TrashCopyOp _ newSrcFileRepr uuid) -> (uuid, newSrcFileRepr))
          trashCopyOps
  let listOfFilenameUuidToCopy =
        sortBy (\(fnameA, _) (fnameB, _) -> fnameA `compare` fnameB) $
        S.toList $
        S.difference currentFilenamesAndUuidsSet initialFilenamesAndUuidsSet
  -- Map of UUID -> filename; for files that are in the current directory when
  -- the program started.
  let initialUuidToFilename =
        M.fromList $ fmap swap initialFilenamesAndUuids
  copyOps <- genCopyOps
               uuidToTrashCopyFileRepr
               initialUuidToFilename
               listOfFilenameUuidToCopy
  return $ trashCopyOps <> copyOps
