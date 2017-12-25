{-# LANGUAGE ScopedTypeVariables #-}

module Hroamer.FileOps
  ( FileOp(..)
  , doFileOp
  , generateFileOps
  ) where

import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (Reader, ReaderT(runReaderT), ask, asks)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Foundation
import System.Directory
       (copyFile, createDirectory, doesDirectoryExist, doesPathExist,
        pathIsSymbolicLink, renamePath)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath.Posix (FilePath)
import System.Process (createProcess, proc, waitForProcess)

import Hroamer.DataStructures
       (FileOpsReadState(rsCwd, rsPathToDb), FilePathUUIDPair,
        FileRepr(FileRepr), fileReprToFilePath)
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
        sortBy (compare `on` fst) $
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
doFileOp :: (FilesTableRow -> IO ()) -> FileOp -> ReaderT FileOpsReadState IO ()

doFileOp dbUpdateDirAndFileName (TrashCopyOp srcFileRepr destFileRepr uuid) =
  liftIO $ do
    let (FileRepr destDir destFilename) = destFileRepr
    createDirectory destDir
    trashCopy destDir destFilename `catch` \(exc::IOException) ->
      TIO.putStrLn $
        "Failed to trash-copy " <>
        (pack srcFilePath) <>
        " because " <>
        (pack . toList $ show exc) <>
        ". Continuing..."
  where
    srcFilePath :: FilePath
    srcFilePath = fileReprToFilePath srcFileRepr

    trashCopy :: FilePath -> FilePath -> IO ()
    trashCopy destDir destFilename = do
      let destFilePath = fileReprToFilePath destFileRepr
      renamePath srcFilePath destFilePath
      TIO.putStrLn $ "trash-copy " <> (pack srcFilePath)
      dbUpdateDirAndFileName
        FilesTableRow {dir = destDir, filename = destFilename, uuid = uuid}

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


doFileOp _ (CopyOp srcFileRepr destFileRepr) = do
  let (FileRepr destDir destFilename) = destFileRepr
  let srcFilePath = fileReprToFilePath srcFileRepr
  let destFilePath = fileReprToFilePath destFileRepr
  isSymLink <- liftIO $ pathIsSymbolicLink srcFilePath
  if isSymLink
    then liftIO $ do
      -- doesPathExist and doesDirectoryExist will try to resolve symlinks,
      -- but we do not want that. So we just assume the symlink is there and
      -- copy it
      (_, _, _, ph) <- createProcess (proc "cp" ["-P", srcFilePath, destFilePath])
      exitCode <- waitForProcess ph
      case exitCode of
        ExitSuccess -> TIO.putStrLn $
          "cp -P " <> pack srcFilePath <> " " <> pack destFilePath
        _ -> TIO.putStrLn $
          "Failed to copy " <> pack srcFilePath <> " to " <> pack destFilePath
    else do
      srcExists <- liftIO $ doesPathExist srcFilePath
      srcIsDir <- liftIO $ doesDirectoryExist srcFilePath
      liftIO $
        case (srcExists, srcIsDir) of
          (True, True) -> do
            (_, _, _, ph) <-
              createProcess (proc "cp" ["-R", srcFilePath, destFilePath])
            exitcode <- waitForProcess ph
            case exitcode of
              ExitSuccess ->
                TIO.putStrLn $
                  "cp -R " <> (pack srcFilePath) <> " " <> (pack destFilePath)
              _ ->
                TIO.putStrLn $
                  "Failed to copy " <> (pack srcFilePath) <> " to " <>
                  (pack destFilePath)
          (True, _) -> do
             copyFile srcFilePath destFilePath
             TIO.putStrLn $
               "cp " <> (pack srcFilePath) <> " " <> (pack destFilePath)
          _ -> return ()
