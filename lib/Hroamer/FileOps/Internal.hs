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
import System.FilePath.Posix
       ((</>), FilePath, takeDirectory, takeFileName)

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
genTrashCopyOps initialPathsAndUuids currentPathsAndUuids = do
  cwd <- asks rsCwd
  pathToTrashCopyDir <- asks rsTrashCopyDir
  let pathsAndUuidsToTrashCopy =
        S.difference initialPathsAndUuids currentPathsAndUuids
  let listOfPathsAndUuidsToTrashCopy =
        sortBy (compare `on` fst) $
        S.toList pathsAndUuidsToTrashCopy
  return $
    fmap
      (\(filePath, uuid) ->
         let srcDir = takeDirectory filePath
             filename = takeFileName filePath
             destFileRepr =
               FileRepr (dirToTrashCopyTo pathToTrashCopyDir uuid) filename
         in TrashCopyOp (FileRepr srcDir filename) destFileRepr uuid)
      listOfPathsAndUuidsToTrashCopy


genCopyOps
  :: Map Text FileRepr
  -> Map Text FilePath
  -> [FilePathUUIDPair]
  -> Reader FileOpsReadState [FileOp]
genCopyOps uuidToTrashCopyFileRepr initialUuidToPath listOfPathUuidToCopy = do
  cwd <- asks rsCwd
  return $ fmap
    (\(filePath, uuid) ->
       let destDir = takeDirectory filePath
           filename = takeFileName filePath
           destFileRepr = FileRepr destDir filename
           x = (do
             maybeToLeft
               (\newSrcFileRepr -> CopyOp newSrcFileRepr destFileRepr)
               (M.lookup uuid uuidToTrashCopyFileRepr)
             -- Source file is not to be trash copied.
             -- See if we can find it in the initial set of files.
             maybeToLeft
               (\srcPath ->
                 let srcDir = takeDirectory srcPath
                     srcFilename = takeFileName srcPath
                 in CopyOp (FileRepr srcDir srcFilename) destFileRepr)
               (M.lookup uuid initialUuidToPath)
             -- Need to perform database lookup
             return $ LookupDbCopyOp destFileRepr uuid)
       in either id id x)
    listOfPathUuidToCopy
  where
    maybeToLeft :: (a -> FileOp) -> Maybe a -> Either FileOp ()
    maybeToLeft f (Just x) = Left $ f x
    maybeToLeft _ Nothing = Right ()
