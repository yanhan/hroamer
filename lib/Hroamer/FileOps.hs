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
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Foundation
import System.Directory
       (copyFile, createDirectory, doesDirectoryExist, doesPathExist,
        pathIsSymbolicLink, renamePath)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath.Posix (FilePath)
import System.Process (createProcess, proc, waitForProcess)

import Hroamer.DataStructures
       (AbsFilePath(AbsFilePath), AbsFilePathUUIDPair,
        FileOpsReadState(rsCwd, rsPathToDb), FileRepr(FileRepr),
        fileReprToFilePath)
import Hroamer.Database (FilesTableRow(..))
import Hroamer.FileOps.Internal
       (FileOp(..), genCopyOps, genTrashCopyOps)

import qualified Hroamer.Database as HroamerDb

generateFileOps
  :: [AbsFilePathUUIDPair]
  -> [AbsFilePathUUIDPair]
  -> Reader FileOpsReadState [FileOp]
generateFileOps listOfPathsAndUuids initialPathsAndUuids = do
  let initialPathsAndUuidsSet = S.fromList initialPathsAndUuids
  let currentPathsAndUuidsSet = S.fromList listOfPathsAndUuids
  trashCopyOps <- genTrashCopyOps
                    initialPathsAndUuidsSet
                    currentPathsAndUuidsSet
  -- At this point, UUIDs in `initialPathsAndUuids` are unique. Otherwise
  -- they would have violated the UNIQUE constraint on the `files.uuid` column.
  -- Hence, we can safely construct a Map indexed by UUID
  let uuidToTrashCopyFileRepr =
        M.fromList $
        fmap
          (\(TrashCopyOp _ newSrcFileRepr uuid) -> (uuid, newSrcFileRepr))
          trashCopyOps
  let listOfPathsAndUuidsToCopy =
        sortBy (compare `on` fst) $
        S.toList $
        S.difference currentPathsAndUuidsSet initialPathsAndUuidsSet
  -- Map of UUID -> path to file; for files that are in the current directory
  -- when the program started.
  let initialUuidToPath =
        M.fromList $ fmap swap initialPathsAndUuids
  copyOps <- genCopyOps
               uuidToTrashCopyFileRepr
               initialUuidToPath
               listOfPathsAndUuidsToCopy
  return $ trashCopyOps <> copyOps

doFileOp :: (FilesTableRow -> IO ()) -> FileOp -> ReaderT FileOpsReadState IO ()

doFileOp dbUpdateDirAndFileName (TrashCopyOp srcFileRepr destFileRepr uuid) =
  liftIO $ do
    let (FileRepr destDir destFilename) = destFileRepr
    createDirectory destDir
    trashCopy destDir destFilename `catch` \(exc::IOException) ->
      TIO.putStrLn $
        "Failed to " <>
        trashCopyMessage <>
        " because\n  " <>
        (pack . toList $ show exc) <>
        "\n  Continuing..."
  where
    srcFilePath :: FilePath
    srcFilePath = fileReprToFilePath srcFileRepr

    trashCopyMessage :: Text
    trashCopyMessage = "trash-copy " <> pack srcFilePath

    trashCopy :: FilePath -> FilePath -> IO ()
    trashCopy destDir destFilename = do
      let destFilePath = fileReprToFilePath destFileRepr
      renamePath srcFilePath destFilePath
      TIO.putStrLn trashCopyMessage
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
  isSymlink <- liftIO $ pathIsSymbolicLink srcFilePath `catch`
    \(e::IOException) -> return False
  if isSymlink
    then liftIO $ do
      -- doesPathExist and doesDirectoryExist will try to resolve symlinks,
      -- but we do not want that. So we just assume the symlink is there and
      -- copy it
      (_, _, _, ph) <- createProcess (proc "cp" ["-P", srcFilePath, destFilePath])
      exitCode <- waitForProcess ph
      case exitCode of
        ExitSuccess -> TIO.putStrLn $
          "cp -P " <> pack srcFilePath <> " " <> pack destFilePath
        _ -> TIO.putStrLn $ failedToCopyMessage srcFilePath destFilePath
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
                  "cp -R " <> pack srcFilePath <> " " <> pack destFilePath
              _ ->
                TIO.putStrLn $ failedToCopyMessage srcFilePath destFilePath
          (True, _) ->
            copyPlainFile srcFilePath destFilePath `catch`
              \(e::IOException) -> TIO.putStrLn $
                failedToCopyMessage srcFilePath destFilePath
          _ -> return ()
  where
    failedToCopyMessage :: FilePath -> FilePath -> Text
    failedToCopyMessage srcFilePath destFilePath =
      "Failed to copy " <> pack srcFilePath <> " to " <> pack destFilePath

    copyPlainFile :: FilePath -> FilePath -> IO ()
    copyPlainFile srcFilePath destFilePath = do
      copyFile srcFilePath destFilePath
      TIO.putStrLn $ "cp " <> (pack srcFilePath) <> " " <> (pack destFilePath)
