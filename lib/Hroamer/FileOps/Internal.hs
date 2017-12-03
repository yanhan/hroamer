module Hroamer.FileOps.Internal
  ( FileOp(..)
  , genCopyOps
  , genTrashCopyOps
  ) where

import Control.Monad.Reader (Reader, asks)
import Data.Either (either)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Foundation
import System.FilePath.Posix ((</>), FilePath)

import Hroamer.DataStructures
       (FileRepr(FileRepr), FileOpsReadState(rsCwd, rsTrashCopyDir),
        FilePathUUIDPair)

data FileOp
  = CopyOp { srcFileRepr :: FileRepr
          ,  destFileRepr :: FileRepr }
  | LookupDbCopyOp FileRepr -- dest
                   Text -- uuid
  | TrashCopyOp FileRepr -- src
                FileRepr -- dest
                Text -- uuid
  deriving (Eq, Show)

genTrashCopyOps
  :: Set FilePathUUIDPair
  -> Set FilePathUUIDPair
  -> Reader FileOpsReadState [FileOp]
genTrashCopyOps initial_filenames_uuids current_filenames_uuids = do
  cwd <- asks rsCwd
  path_to_trashcopy_dir <- asks rsTrashCopyDir
  let filenames_uuids_to_trashcopy =
        S.difference initial_filenames_uuids current_filenames_uuids
  let list_of_filename_uuid_to_trashcopy =
        sortBy (\(fname_a, _) (fname_b, _) -> fname_a `compare` fname_b) $
        S.toList filenames_uuids_to_trashcopy
  return $
    fmap
      (\(fname, uuid) ->
         let dest_filerepr =
               FileRepr (dirToTrashCopyTo path_to_trashcopy_dir uuid) fname
         in TrashCopyOp (FileRepr cwd fname) dest_filerepr uuid)
      list_of_filename_uuid_to_trashcopy
  where
    dirToTrashCopyTo :: FilePath -> Text -> FilePath
    dirToTrashCopyTo path_to_trashcopy_dir uuid =
      path_to_trashcopy_dir </> (toList uuid)


genCopyOps
  :: Map Text FileOp
  -> Map Text FilePath
  -> [FilePathUUIDPair]
  -> Reader FileOpsReadState [FileOp]
genCopyOps uuid_to_trashcopyop initial_uuid_to_filename list_of_filename_uuid_to_copy = do
  cwd <- asks rsCwd
  return $ fmap
    (\(fname, uuid) ->
       let dest_filerepr = FileRepr cwd fname
           x = (do
             maybeToLeft
               (\(TrashCopyOp _ new_src_filerepr _) ->
                  CopyOp new_src_filerepr dest_filerepr)
               (M.lookup uuid uuid_to_trashcopyop)
             -- Source file is not to be trash copied.
             -- See if we can find it in the initial set of files.
             maybeToLeft
               (\src_filename ->
                 CopyOp (FileRepr cwd src_filename) dest_filerepr)
               (M.lookup uuid initial_uuid_to_filename)
             -- Need to perform database lookup
             return $ LookupDbCopyOp dest_filerepr uuid)
       in either id id x)
    list_of_filename_uuid_to_copy
  where
    maybeToLeft :: (a -> FileOp) -> Maybe a -> Either FileOp ()
    maybeToLeft f (Just x) = Left $ f x
    maybeToLeft _ Nothing = Right ()
