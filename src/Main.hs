module Main where

import Control.Exception (catch, IOException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
       (Reader, ReaderT(ReaderT, runReaderT), ask, asks, runReader)
import Control.Monad.Writer.Strict (runWriterT)
import qualified Data.DList
import Data.Either (either)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import Foundation
import Foundation.Collection (mapM, mapM_, zip)
import System.Directory
       (XdgDirectory(XdgData), copyFile, createDirectory,
        doesDirectoryExist, doesPathExist, getCurrentDirectory,
        getXdgDirectory, listDirectory, removeFile, renamePath)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.FilePath.Posix
       (FilePath, (</>), takeDirectory, takeBaseName)
import System.Posix.Signals
       (Handler(Catch), installHandler, keyboardSignal, softwareStop,
        softwareTermination)
import System.Process (createProcess, proc, waitForProcess)

import Hroamer.DataStructures (FilePathUUIDPair)
import Hroamer.Database (FilesTableRow(..))

import qualified Hroamer.Database as HroamerDb
import qualified Hroamer.Path as Path
import qualified Hroamer.StateFile as StateFile
import qualified Hroamer.UnsupportedPaths as UnsupportedPaths
import qualified Hroamer.Utilities as Utils

-- A file, broken down into its directory and filename
data FileRepr = FileRepr FilePath FilePath -- dir  file
  deriving (Eq, Show)

filerepr_to_filepath :: FileRepr -> FilePath
filerepr_to_filepath (FileRepr dir fname) = dir </> fname


data FileOpsReadState =
  FileOpsReadState { rsCwd :: FilePath
                   , rsPathToDb :: FilePath
                   , rsTrashCopyDir :: FilePath
                   }


main :: IO ()
main = do
  app_data_dir <- getXdgDirectory XdgData "hroamer"
  let app_tmp_dir = app_data_dir </> "tmp"
  -- Directory for storing files 'deleted' using hroamer
  let path_to_trashcopy_dir = app_data_dir </> "trash-copy"

  (allDirsOk, errorDList) <- runWriterT $ do
    -- WriterT (DList Text) IO Bool
    success_creating_app_data_dir <- Path.createDirNoForce app_data_dir
    success_creating_app_tmp_dir <- Path.createDirNoForce app_tmp_dir
    success_creating_trashcopy_dir <- Path.createDirNoForce path_to_trashcopy_dir
    return $
      success_creating_app_data_dir &&
      success_creating_app_tmp_dir && success_creating_trashcopy_dir

  if not allDirsOk
    then do
      mapM_ TIO.putStrLn $ Data.DList.toList errorDList
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
        StateFile.read user_dirstate_filepath
      let list_of_filename = fmap fst list_of_filename_and_uuid
      unsupportedPaths <- UnsupportedPaths.getUnsupportedPaths cwd list_of_filename
      let unsupportedPathsDList = UnsupportedPaths.getErrors cwd unsupportedPaths
      if unsupportedPathsDList == Data.DList.empty
        then do
          file_op_list <- runReaderT
            (generateFileOps list_of_filename_and_uuid initial_fnames_and_uuids)
            (FileOpsReadState cwd path_to_db path_to_trashcopy_dir)
          HroamerDb.wrapDbConn
            path_to_db
            (\f -> forM_ file_op_list (doFileOp cwd path_to_db f))
            HroamerDb.updateDirAndFilename
        else mapM_ TIO.putStrLn $ Data.DList.toList unsupportedPathsDList


  -- cleanup
  removeFile dirstate_filepath `catch` excHandler
  removeFile user_dirstate_filepath `catch` excHandler
  where
    excHandler :: IOException -> IO ()
    excHandler = const $ return ()


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


data FileOp
  = CopyOp { srcFileRepr :: FileRepr
          ,  destFileRepr :: FileRepr }
  | LookupDbCopyOp FileRepr -- dest
                   Text -- uuid
  | TrashCopyOp FileRepr -- src
                FileRepr -- dest
                Text -- uuid
  deriving (Eq, Show)


-- Assume both src and dest are in the same directory
doFileOp :: FilePath -> FilePath -> (FilesTableRow -> IO ()) -> FileOp ->  IO ()

doFileOp cwd _ dbUpdateDirAndFileName (TrashCopyOp src_filerepr dest_filerepr uuid) = do
  let (FileRepr dest_dir dest_filename) = dest_filerepr
  let src_filepath = filerepr_to_filepath src_filerepr
  let dest_filepath = filerepr_to_filepath dest_filerepr
  createDirectory dest_dir
  renamePath src_filepath dest_filepath
  TIO.putStrLn $ "trash-copy " <> (pack src_filepath)
  dbUpdateDirAndFileName
    FilesTableRow {dir = dest_dir, filename = dest_filename, uuid = uuid}

-- YH TODO: This is causing us to open another connection to the db.
-- Refactor the code so that we don't have to do that.
doFileOp cwd pathToDb dbUpdateDirAndFileName (LookupDbCopyOp destFileRepr uuid) = do
  HroamerDb.wrapDbConn
    pathToDb
    (\dbGetRowFromUUID -> do
      row <- dbGetRowFromUUID uuid
      case row of
        [] -> return ()
        (FilesTableRow {dir = srcDir, filename = srcFilename} : _ ) ->
          let srcFileRepr = FileRepr srcDir srcFilename
          in doFileOp cwd pathToDb dbUpdateDirAndFileName (CopyOp srcFileRepr destFileRepr))
    HroamerDb.getRowFromUUID


doFileOp cwd _ _ (CopyOp src_filerepr dest_filerepr) = do
  let (FileRepr dest_dir dest_filename) = dest_filerepr
  let src_filepath = filerepr_to_filepath src_filerepr
  let dest_filepath = filerepr_to_filepath dest_filerepr
  src_exists <- doesPathExist src_filepath
  src_is_dir <- doesDirectoryExist src_filepath
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
           TIO.putStrLn $ "cp " <> (pack src_filepath) <> " " <> (pack dest_filepath)
         else
           return ()


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
  -> Reader FilePath [FileOp]
genCopyOps uuid_to_trashcopyop initial_uuid_to_filename list_of_filename_uuid_to_copy = do
  cwd <- ask
  return $ fmap
    (\(fname, uuid) ->
       let dest_filerepr = FileRepr cwd fname
           x = do
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
                 return $ LookupDbCopyOp dest_filerepr uuid
       in either id id x)
    list_of_filename_uuid_to_copy
  where
    maybeToLeft :: (a -> FileOp) -> Maybe a -> Either FileOp ()
    maybeToLeft f (Just x) = Left $ f x
    maybeToLeft _ Nothing = Right ()


generateFileOps
  :: [FilePathUUIDPair]
  -> [FilePathUUIDPair]
  -> ReaderT FileOpsReadState IO [FileOp]
generateFileOps list_of_filename_and_uuid initial_filenames_and_uuids = do
  let initial_filename_uuid_set = S.fromList initial_filenames_and_uuids
  let current_filename_uuid_set = S.fromList list_of_filename_and_uuid
  r <- ask
  let trashCopyOps =
        runReader
          (genTrashCopyOps initial_filename_uuid_set current_filename_uuid_set)
          r
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
  cwd <- asks rsCwd
  let copyOps = runReader (genCopyOps
                            uuid_to_trashcopyop
                            initial_uuid_to_filename
                            list_of_filename_uuid_to_copy)
                          cwd
  return $ trashCopyOps <> copyOps
