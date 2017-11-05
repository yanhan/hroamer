module Main where

import Conduit (decodeUtf8C, lineC, peekForeverE, sinkList)
import Control.Exception (catch, IOException)
import Control.Monad (forM_, sequence)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Conduit ((.|), await, runConduitRes, yield)
import Data.Conduit.Binary (sourceFile)
import Data.Either (either)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Database.SQLite.Simple as D
import Foundation
import Foundation.Collection (mapM, mapM_, zip)
import System.Directory
       (XdgDirectory(XdgData), copyFile, createDirectory,
        doesDirectoryExist, doesPathExist, getCurrentDirectory,
        getXdgDirectory, getHomeDirectory, listDirectory, removeFile,
        renameDirectory, renameFile)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.FilePath.Posix
       (FilePath, (</>), dropTrailingPathSeparator, takeDirectory,
        takeBaseName)
import System.IO.Temp (writeTempFile)
import System.Posix.Signals
       (Handler(Catch), installHandler, keyboardSignal, softwareStop,
        softwareTermination)
import System.Process (createProcess, proc, waitForProcess)
import Text.Parsec (runParserT)

import Hroamer.DataStructures (FilePathUUIDPair)
import Hroamer.Database (FilesTableRow(..))

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.Parser as Parser
import qualified Hroamer.Path as Path
import qualified Hroamer.UnsupportedPaths as UnsupportedPaths
import qualified Hroamer.Utilities as Utils

-- A file, broken down into its directory and filename
data FileRepr = FileRepr FilePath FilePath -- dir  file
  deriving (Eq, Show)

filerepr_to_filepath :: FileRepr -> FilePath
filerepr_to_filepath (FileRepr dir fname) = dir </> fname


main :: IO ()
main = do
  home_dir <- getHomeDirectory
  app_data_dir <- getXdgDirectory XdgData "hroamer"
  success_creating_app_data_dir <- Path.createDirNoForce app_data_dir

  let app_tmp_dir = app_data_dir </> "tmp"
  success_creating_app_tmp_dir <- Path.createDirNoForce app_tmp_dir

  -- Directory for storing files 'deleted' using hroamer
  let path_to_trashcopy_dir = app_data_dir </> "trash-copy"
  success_creating_trashcopy_dir <- Path.createDirNoForce path_to_trashcopy_dir

  if not success_creating_app_data_dir ||
     not success_creating_app_tmp_dir || not success_creating_trashcopy_dir
    then do
      TIO.putStrLn "Exiting."
      exitWith $ ExitFailure 1
    else return ()

  let path_to_db = app_data_dir </> "hroamer.db"
  HroamerDb.createDbAndTables path_to_db

  cwd <- getCurrentDirectory
  -- Do not allow user to use hroamer to manage files that it creates
  if Path.isWeakAncestorDir app_data_dir cwd
    then do
      TIO.putStrLn $
        "Error: You tried to use hroamer to manage " <> (pack cwd) <> "\n" <>
        "However, you are not allowed to use hroamer to manage " <>
        (pack app_data_dir) <>
        " and directories below it.\nExiting."
      exitWith $ ExitFailure 1
    else return ()

  (initial_fnames_and_uuids, dirstate_filepath) <-
    processCwd cwd app_tmp_dir path_to_db
  let user_dirstate_filepath =
        (takeDirectory dirstate_filepath) </>
        ("user-" <> takeBaseName dirstate_filepath)
  copyFile dirstate_filepath user_dirstate_filepath
  let signals_to_handle = [keyboardSignal, softwareStop, softwareTermination]
  let handler = Catch $
        removeFile dirstate_filepath `catch` excHandler >>
        removeFile user_dirstate_filepath `catch` excHandler
  mapM_ (\signal -> installHandler signal handler Nothing) signals_to_handle

  -- Launch text editor to let user edit the file
  editor_createprocess <- Utils.make_editor_createprocess user_dirstate_filepath
  (_, _, _, editor_process) <- createProcess editor_createprocess
  waitForProcess editor_process

  -- Compare for difference between the files
  (_, _, _, cmp_process) <-
    createProcess
      (proc "cmp" ["--silent", dirstate_filepath, user_dirstate_filepath])
  cmp_exit_code <- waitForProcess cmp_process
  case cmp_exit_code of
    ExitSuccess -> return ()
    _ -> do
      list_of_filename_and_uuid <-
        getFilenameAndUUIDInUserDirStateFile user_dirstate_filepath
      let list_of_filename = fmap fst list_of_filename_and_uuid
      unsupportedPaths <- UnsupportedPaths.getUnsupportedPaths cwd list_of_filename
      if UnsupportedPaths.noUnsupportedPaths unsupportedPaths
        then do
          file_op_list <-
            generateFileOps
              path_to_trashcopy_dir
              cwd
              path_to_db
              list_of_filename_and_uuid
              initial_fnames_and_uuids
          D.withConnection
            path_to_db
            (\dbconn -> forM_ file_op_list (doFileOp cwd dbconn))
        else UnsupportedPaths.printErrors cwd unsupportedPaths

  -- cleanup
  removeFile dirstate_filepath `catch` excHandler
  removeFile user_dirstate_filepath `catch` excHandler
  where
    excHandler :: IOException -> IO ()
    excHandler = const $ return ()


writeStateFile :: FilePath -> FilePath -> [FilePathUUIDPair] -> IO FilePath
writeStateFile cwd app_tmp_dir files_and_uuid__accurate = do
  let files_and_uuid_sorted =
        sortBy (\(fn1, _) (fn2, _) -> compare fn1 fn2) files_and_uuid__accurate
  lines_to_write_to_file <-
    sequence $
    fmap
      (\(fn, uuid) -> do
         fn_perhaps_with_trailing_slash <- Path.appendSlashToDir cwd fn
         return $ pack fn_perhaps_with_trailing_slash <> " | " <> uuid)
      files_and_uuid_sorted
  writeTempFile
    app_tmp_dir
    "dirst"
    (constructTextFileHeader cwd <>
     (toList $ intercalate "\n" lines_to_write_to_file))
  where
    constructTextFileHeader :: FilePath -> [Char]
    constructTextFileHeader cwd = "\" pwd: " <> (toList cwd) <> "\n"


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
  dirstate_filepath <- writeStateFile cwd app_tmp_dir files_and_uuids_accurate
  return (files_and_uuids_accurate, dirstate_filepath)
  where
    separateFilesIntoCategories :: [FilePath]
                                -> [[Char]]
                                -> (Set [Char], Set [Char])
    separateFilesIntoCategories files_on_system files_in_db =
      let set_system = S.fromList $ fmap toList files_on_system
          set_db = S.fromList files_in_db
      in (set_system `S.difference` set_db, set_db `S.difference` set_system)


data FileOp
  = CopyOp { srcFileRepr :: FileRepr
          ,  destFileRepr :: FileRepr
          ,  srcIsDir :: Bool}
  | TrashCopyOp FileRepr -- src
                FileRepr -- dest
                Text -- uuid
                Bool -- src_is_directory
  | NoFileOp
  deriving (Eq, Show)


-- Assume both src and dest are in the same directory
doFileOp :: FilePath -> D.Connection -> FileOp -> IO ()

-- Do nothing
doFileOp _ _ NoFileOp = return ()

doFileOp cwd dbconn (TrashCopyOp src_filerepr dest_filerepr uuid src_is_dir) = do
  let (FileRepr dest_dir dest_filename) = dest_filerepr
  let src_filepath = filerepr_to_filepath src_filerepr
  let dest_filepath = filerepr_to_filepath dest_filerepr
  createDirectory dest_dir
  if src_is_dir
    then renameDirectory src_filepath dest_filepath
    else renameFile src_filepath dest_filepath
  TIO.putStrLn $ "trash-copy " <> (pack src_filepath)
  -- what
  HroamerDb.updateDirAndFilename dbconn
    FilesTableRow {dir = dest_dir, filename = dest_filename, uuid = uuid}

doFileOp cwd _ (CopyOp src_filerepr dest_filerepr src_is_dir) = do
  let (FileRepr dest_dir dest_filename) = dest_filerepr
  let src_filepath = filerepr_to_filepath src_filerepr
  let dest_filepath = filerepr_to_filepath dest_filerepr
  if src_is_dir
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
    else do
      copyFile src_filepath dest_filepath
      TIO.putStrLn $ "cp " <> (pack src_filepath) <> " " <> (pack dest_filepath)


getFilenameAndUUIDInUserDirStateFile :: FilePath -> IO [FilePathUUIDPair]
getFilenameAndUUIDInUserDirStateFile user_dirstate_filepath = do
  list_of_maybe_fname_uuid <-
    runConduitRes $
    sourceFile user_dirstate_filepath .| decodeUtf8C .|
    peekForeverE parseLineConduit .|
    sinkList
  return $
    fmap
      (\x ->
         let (fn, uuid) = fromJust x
         in ((dropTrailingPathSeparator . toList) fn, uuid))
      list_of_maybe_fname_uuid
  where
    parseLineConduit =
      lineC
        (do mx <- await
            case mx of
              Just x ->
                let parse_result =
                      runIdentity $ runParserT Parser.parseUserDirStateFile () "" x
                in either (const (return ())) onlyYieldJust parse_result
              Nothing -> return ())

    onlyYieldJust j@(Just _) = yield j
    onlyYieldJust _ = return ()


genTrashCopyOps
  :: FilePath
  -> FilePath
  -> Set FilePathUUIDPair
  -> Set FilePathUUIDPair
  -> IO [FileOp]
genTrashCopyOps path_to_trashcopy_dir cwd initial_filenames_uuids current_filenames_uuids = do
  let filenames_uuids_to_trashcopy =
        S.difference initial_filenames_uuids current_filenames_uuids
  let list_of_filename_uuid_to_trashcopy =
        sortBy (\(fname_a, _) (fname_b, _) -> fname_a `compare` fname_b) $
        S.toList filenames_uuids_to_trashcopy
  mapM
    (\(fname, uuid) -> do
       let dest_filerepr =
             FileRepr (dirToTrashCopyTo path_to_trashcopy_dir uuid) fname
       src_is_dir <- doesDirectoryExist $ cwd </> fname
       return $ TrashCopyOp (FileRepr cwd fname) dest_filerepr uuid src_is_dir)
    list_of_filename_uuid_to_trashcopy


genCopyOps
  :: FilePath
  -> D.Connection
  -> Map Text FileOp
  -> Map Text FilePath
  -> [FilePathUUIDPair]
  -> IO [FileOp]
genCopyOps cwd dbconn uuid_to_trashcopyop initial_uuid_to_filename list_of_filename_uuid_to_copy =
  mapM
    (\(fname, uuid) ->
       let dest_filerepr = FileRepr cwd fname
       in case M.lookup uuid uuid_to_trashcopyop of
            Just (TrashCopyOp _ new_src_filerepr _ src_is_dir) -> do
              return $ CopyOp new_src_filerepr dest_filerepr src_is_dir
            Nothing
              -- Source file is not to be trash copied.
              -- See if we can find it in the initial set of files.
             ->
              case M.lookup uuid initial_uuid_to_filename of
                Just src_filename ->
                  makeCopyOpOrNoFileOp (FileRepr cwd src_filename) dest_filerepr
                Nothing
                  -- Need to perform database lookup
                 -> do
                  r <-
                    liftIO $
                    D.query
                      dbconn
                      "SELECT dir, filename, uuid FROM files WHERE uuid = ?"
                      [uuid]
                  case r of
                    (FilesTableRow {dir = src_dir, filename = src_filename}:_) ->
                      makeCopyOpOrNoFileOp
                        (FileRepr src_dir src_filename)
                        dest_filerepr
                    [] -> return NoFileOp)
    list_of_filename_uuid_to_copy
  where
    makeCopyOpOrNoFileOp :: FileRepr -> FileRepr -> IO FileOp
    makeCopyOpOrNoFileOp src_filerepr@(FileRepr src_dir src_filename) dest_filerepr = do
      let src_filepath = src_dir </> src_filename
      src_is_dir <- doesDirectoryExist src_filepath
      if src_is_dir
        then return $ CopyOp src_filerepr dest_filerepr True
        else do
          src_exists <- doesPathExist src_filepath
          if src_exists
            then return $ CopyOp src_filerepr dest_filerepr False
            else return $ NoFileOp


dirToTrashCopyTo :: FilePath -> Text -> FilePath
dirToTrashCopyTo path_to_trashcopy_dir uuid =
  path_to_trashcopy_dir </> (toList uuid)


generateFileOps
  :: FilePath
  -> FilePath
  -> FilePath
  -> [FilePathUUIDPair]
  -> [FilePathUUIDPair]
  -> IO [FileOp]
generateFileOps path_to_trashcopy_dir cwd path_to_db list_of_filename_and_uuid initial_filenames_and_uuids = do
  let initial_filename_uuid_set = S.fromList initial_filenames_and_uuids
  let current_filename_uuid_set = S.fromList list_of_filename_and_uuid
  trashCopyOps <-
    genTrashCopyOps
      path_to_trashcopy_dir
      cwd
      initial_filename_uuid_set
      current_filename_uuid_set
  -- At this point, UUIDs in `initial_filenames_and_uuids` are unique. Otherwise
  -- they would have violated the UNIQUE constraint on the `files.uuid` column.
  -- Hence, we can safely construct a Map indexed by UUID
  let uuid_to_trashcopyop =
        M.fromList $
        fmap (\op@(TrashCopyOp _ _ uuid _) -> (uuid, op)) trashCopyOps
  let list_of_filename_uuid_to_copy =
        sortBy (\(fname_a, _) (fname_b, _) -> fname_a `compare` fname_b) $
        S.toList $
        S.difference current_filename_uuid_set initial_filename_uuid_set
  -- Map of UUID -> filename; for files that are in the current directory when
  -- the program started.
  let initial_uuid_to_filename =
        M.fromList $ fmap swap initial_filenames_and_uuids
  list_of_copyop <-
    D.withConnection
      path_to_db
      (\dbconn ->
         genCopyOps
           cwd
           dbconn
           uuid_to_trashcopyop
           initial_uuid_to_filename
           list_of_filename_uuid_to_copy)
  return $ trashCopyOps <> filter (/= NoFileOp) list_of_copyop
