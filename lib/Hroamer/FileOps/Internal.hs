module Hroamer.FileOps.Internal
  ( FileOp(..)
  , dirToTrashCopyTo
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


dirToTrashCopyTo :: FilePath -> Text -> FilePath
dirToTrashCopyTo pathToTrashCopyDir uuid =
  pathToTrashCopyDir </> (toList uuid)


genTrashCopyOps
  :: Set FilePathUUIDPair
  -> Set FilePathUUIDPair
  -> Reader FileOpsReadState [FileOp]
genTrashCopyOps initialFilenamesAndUuids currentFilenamesAndUuids = do
  cwd <- asks rsCwd
  pathToTrashCopyDir <- asks rsTrashCopyDir
  let filenamesAndUuidsToTrashCopy =
        S.difference initialFilenamesAndUuids currentFilenamesAndUuids
  let listOfFilenamesAndUuidsToTrashCopy =
        sortBy (compare `on` fst) $
        S.toList filenamesAndUuidsToTrashCopy
  return $
    fmap
      (\(fname, uuid) ->
         let destFileRepr =
               FileRepr (dirToTrashCopyTo pathToTrashCopyDir uuid) fname
         in TrashCopyOp (FileRepr cwd fname) destFileRepr uuid)
      listOfFilenamesAndUuidsToTrashCopy


genCopyOps
  :: Map Text FileRepr
  -> Map Text FilePath
  -> [FilePathUUIDPair]
  -> Reader FileOpsReadState [FileOp]
genCopyOps uuidToTrashCopyFileRepr initialUuidToFilename listOfFilenameUuidToCopy = do
  cwd <- asks rsCwd
  return $ fmap
    (\(fname, uuid) ->
       let destFileRepr = FileRepr cwd fname
           x = (do
             maybeToLeft
               (\newSrcFileRepr -> CopyOp newSrcFileRepr destFileRepr)
               (M.lookup uuid uuidToTrashCopyFileRepr)
             -- Source file is not to be trash copied.
             -- See if we can find it in the initial set of files.
             maybeToLeft
               (\srcFilename ->
                 CopyOp (FileRepr cwd srcFilename) destFileRepr)
               (M.lookup uuid initialUuidToFilename)
             -- Need to perform database lookup
             return $ LookupDbCopyOp destFileRepr uuid)
       in either id id x)
    listOfFilenameUuidToCopy
  where
    maybeToLeft :: (a -> FileOp) -> Maybe a -> Either FileOp ()
    maybeToLeft f (Just x) = Left $ f x
    maybeToLeft _ Nothing = Right ()
