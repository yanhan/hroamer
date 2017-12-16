module Hroamer.FileOps
  ( FileOp(..)
  , doFileOp
  , generateFileOps
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (Reader, ReaderT(runReaderT), ask, asks)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Foundation
import System.Directory
       (copyFile, createDirectory, doesDirectoryExist, doesPathExist,
        renamePath)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (createProcess, proc, waitForProcess)

import Hroamer.DataStructures
       (FileOpsReadState(rsCwd, rsPathToDb), FilePathUUIDPair,
        FileRepr(FileRepr), filerepr_to_filepath)
import Hroamer.Database (FilesTableRow(..))
import Hroamer.FileOps.Internal
       (FileOp(..), genCopyOps, genTrashCopyOps)

import qualified Hroamer.Database as HroamerDb

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

-- Assume both src and dest are in the same directory
doFileOp :: (FilesTableRow -> IO ()) -> FileOp ->  ReaderT FileOpsReadState IO ()

doFileOp dbUpdateDirAndFileName (TrashCopyOp src_filerepr dest_filerepr uuid) = do
  let (FileRepr dest_dir dest_filename) = dest_filerepr
  let src_filepath = filerepr_to_filepath src_filerepr
  let dest_filepath = filerepr_to_filepath dest_filerepr
  liftIO $ createDirectory dest_dir
  liftIO $ renamePath src_filepath dest_filepath
  liftIO $ TIO.putStrLn $ "trash-copy " <> (pack src_filepath)
  liftIO $
    dbUpdateDirAndFileName
      FilesTableRow {dir = dest_dir, filename = dest_filename, uuid = uuid}

-- YH TODO: This is causing us to open another connection to the db.
-- Refactor the code so that we don't have to do that.
doFileOp dbUpdateDirAndFileName (LookupDbCopyOp destFileRepr uuid) = do
  cwd <- asks rsCwd
  pathToDb <- asks rsPathToDb
  r <- ask
  liftIO $
    HroamerDb.wrapDbConn
      pathToDb
      (\dbGetRowFromUUID -> do
        row <- dbGetRowFromUUID uuid
        case row of
          [] -> return ()
          (FilesTableRow {dir = srcDir, filename = srcFilename} : _ ) ->
            let srcFileRepr = FileRepr srcDir srcFilename
            in runReaderT
                 (doFileOp dbUpdateDirAndFileName (CopyOp srcFileRepr destFileRepr))
                 r)
      HroamerDb.getRowFromUUID


doFileOp _ (CopyOp src_filerepr dest_filerepr) = do
  let (FileRepr dest_dir dest_filename) = dest_filerepr
  let src_filepath = filerepr_to_filepath src_filerepr
  let dest_filepath = filerepr_to_filepath dest_filerepr
  src_exists <- liftIO $ doesPathExist src_filepath
  src_is_dir <- liftIO $ doesDirectoryExist src_filepath
  liftIO $
    if src_exists && src_is_dir
      then do
        (_, _, _, ph) <-
          createProcess (proc "cp" ["-R", src_filepath, dest_filepath])
        exitcode <- waitForProcess ph
        case exitcode of
          ExitSuccess ->
            TIO.putStrLn $
              "cp -R " <> (pack src_filepath) <> " " <> (pack dest_filepath)
          _ ->
            TIO.putStrLn $
              "Failed to copy " <> (pack src_filepath) <> " to " <>
              (pack dest_filepath)
      else
        if src_exists
           then do
             copyFile src_filepath dest_filepath
             TIO.putStrLn $
               "cp " <> (pack src_filepath) <> " " <> (pack dest_filepath)
           else
             return ()
