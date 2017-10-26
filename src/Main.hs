module Main where

import Conduit (decodeUtf8C, lineC, peekForeverE, sinkList)
import Control.Applicative ((<*), (*>))
import Control.Exception (catch, IOException)
import Control.Monad (forM_, join, sequence)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char
import Data.Conduit ((.|), ConduitM, await, runConduitRes, yield)
import Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.List as CL
import Data.Either (either)
import Data.Functor.Identity (runIdentity)
import Data.Functor.Identity (Identity)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Tuple
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import Data.Void (Void)
import qualified Database.SQLite.Simple as D
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow, field)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
import Foundation hiding ((<|>))
import Foundation.Collection (mapM, mapM_, zip, zipWith)
import System.Directory
       (XdgDirectory(XdgData), copyFile, createDirectory,
        doesDirectoryExist, doesFileExist, doesPathExist,
        getCurrentDirectory, getModificationTime, getXdgDirectory,
        getHomeDirectory, listDirectory, removeDirectoryRecursive,
        removeFile, renameDirectory, renameFile)
import System.Environment (lookupEnv)
import System.Exit
       (ExitCode(ExitFailure, ExitSuccess), die, exitWith)
import System.FilePath.Posix
       (FilePath, (</>), addTrailingPathSeparator,
        dropTrailingPathSeparator, takeDirectory, takeBaseName)
import System.IO.Temp (writeTempFile)
import System.Posix.Signals
       (Handler(Catch), addSignal, emptySignalSet, installHandler,
        keyboardSignal, siginfoSignal, softwareStop, softwareTermination)
import System.Process
       (CreateProcess, createProcess, proc, shell, waitForProcess)
import Text.Parsec
       ((<|>), ParsecT, anyChar, char, count, eof, hexDigit, lookAhead,
        manyTill, runParserT, string, try)
import Text.Parsec.Char (alphaNum)

import qualified Hroamer.Utilities as Utils
import qualified Hroamer.UnsupportedPaths as UnsupportedPaths

-- A file, broken down into its directory and filename
data FileRepr = FileRepr FilePath FilePath -- dir  file
  deriving (Eq, Show)

filerepr_to_filepath :: FileRepr -> FilePath
filerepr_to_filepath (FileRepr dir fname) = dir </> fname


main :: IO ()
main = do
  home_dir <- getHomeDirectory
  app_data_dir <- getXdgDirectory XdgData "hroamer"
  success_creating_app_data_dir <- createDirNoForce app_data_dir

  let app_tmp_dir = app_data_dir </> "tmp"
  success_creating_app_tmp_dir <- createDirNoForce app_tmp_dir

  -- Directory for storing files 'deleted' using hroamer
  let path_to_trashcopy_dir = app_data_dir </> "trash-copy"
  success_creating_trashcopy_dir <- createDirNoForce path_to_trashcopy_dir

  if not success_creating_app_data_dir ||
     not success_creating_app_tmp_dir || not success_creating_trashcopy_dir
    then do
      TIO.putStrLn "Exiting."
      exitWith $ ExitFailure 1
    else return ()

  let path_to_db = app_data_dir </> "hroamer.db"
  createDbAndTables path_to_db

  cwd <- getCurrentDirectory
  -- Do not allow user to use hroamer to manage files that it creates
  if isWeakAncestorDir app_data_dir cwd
    then do
      TIO.putStrLn $
        "Error: You tried to use hroamer to manage " <> (pack cwd) <> "\n" <>
        "However, you are not allowed to use hroamer to manage " <>
        (pack app_data_dir) <>
        " and directories below it.\nExiting."
      exitWith $ ExitFailure 1
    else return ()

  (initial_fname_to_uuid, dirstate_filepath) <-
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
      let dupFilenames = UnsupportedPaths.getDuplicateFilenames list_of_filename
      if S.null dupFilenames
        then do
          (abs_paths, files_not_in_cwd, invalid_paths) <-
            UnsupportedPaths.getUnsupportedPaths cwd list_of_filename
          -- TODO: Separate the error messages by a newline if necessary
          if not $ S.null abs_paths
            then do
              TIO.putStrLn
                "Error: Absolute paths not supported. We found that you entered these:"
              mapM_
                (\s -> TIO.putStrLn $ "- " <> (pack s))
                (List.sort $ S.toList abs_paths)
            else return ()

          if not $ S.null files_not_in_cwd
            then do
              TIO.putStrLn $
                "Error: the following paths are housed in a directory that is not the current directory " <>
                (pack cwd)
              mapM_
                (\s -> TIO.putStrLn $ "- " <> (pack s))
                (List.sort $ S.toList files_not_in_cwd)
            else return ()

          if not $ S.null invalid_paths
            then do
              TIO.putStrLn "Error: the following paths are invalid:"
              mapM_
                (\s -> TIO.putStrLn $ "- " <> (pack s))
                (List.sort $ S.toList invalid_paths)
            else return ()

          if S.null abs_paths && S.null files_not_in_cwd && S.null invalid_paths
            then do
              file_op_list <-
                generateFileOps
                  path_to_trashcopy_dir
                  cwd
                  path_to_db
                  list_of_filename_and_uuid
                  initial_fname_to_uuid
              D.withConnection
                path_to_db
                (\dbconn -> forM_ file_op_list (doFileOp cwd dbconn))
            else return ()
        else do
          TIO.putStrLn "Error - the following filenames are duplicated:"
          mapM_ (\s -> TIO.putStrLn $ "- " <> (pack s)) $
            List.sort (S.toList dupFilenames)

  -- cleanup
  removeFile dirstate_filepath `catch` excHandler
  removeFile user_dirstate_filepath `catch` excHandler
  where
    excHandler :: IOException -> IO ()
    excHandler = const $ return ()


isWeakAncestorDir suspected_ancestor "/" = suspected_ancestor == "/"
isWeakAncestorDir suspected_ancestor dir_of_interest =
  if suspected_ancestor == dir_of_interest
    then True
    else isWeakAncestorDir suspected_ancestor $ takeDirectory dir_of_interest


createDbAndTables :: FilePath -> IO ()
createDbAndTables path_to_db = do
  db_exists <- doesFileExist path_to_db
  if not db_exists
    then D.withConnection
           path_to_db
           (\conn -> do
              D.execute_
                conn
                "CREATE TABLE IF NOT EXISTS files(dir TEXT, filename TEXT, uuid CHAR(36), CONSTRAINT files__idx_dir_filename UNIQUE(dir, filename) ON CONFLICT ROLLBACK, CONSTRAINT files__uuid UNIQUE(uuid) ON CONFLICT ROLLBACK);"
              D.execute_ conn "CREATE INDEX files__idx_dir ON files(dir);")
    else return ()


deleteFileFromDb :: FilePath -> D.Connection -> [Char] -> IO ()
deleteFileFromDb cwd conn filename =
  D.execute
    conn
    "DELETE FROM files WHERE dir = ? AND filename = ?;"
    [cwd, filename]


addFileDetailsToDb :: FilePath -> D.Connection -> ([Char], Text) -> IO ()
addFileDetailsToDb dir conn (filename, uuid) =
  D.execute conn
    "INSERT INTO files(dir, filename, uuid) VALUES(?, ?, ?);"
    (FilesTableRow dir filename uuid)


instance FromRow Int where
  fromRow = field


appendSlashToDir :: FilePath -> FilePath -> IO FilePath
appendSlashToDir dirname filename = do
  isdir <- doesDirectoryExist $ dirname </> filename
  if isdir
    then return $ addTrailingPathSeparator filename
    else return filename


constructTextFileHeader :: FilePath -> [Char]
constructTextFileHeader cwd = "\" pwd: " <> (toList cwd) <> "\n"


processCwd :: FilePath
           -> FilePath
           -> FilePath
           -> IO (Map FilePath Text, FilePath)
processCwd cwd app_tmp_dir path_to_db = do
  files__on_system <- listDirectory cwd
  files_and_uuid__in_db <- selectFromDbAllFilesInDir path_to_db cwd
  let file_to_uuid__in_db = M.fromList files_and_uuid__in_db
  let files__in_db = fmap fst files_and_uuid__in_db
  let (files_on_both, files_only_on_system, files_only_in_db) =
        separateFilesIntoCategories files__on_system files__in_db
  let l_files_only_on_system = S.toList files_only_on_system
  uuid__only_on_system <-
    mapM
      (fmap UUID.toText)
      (List.take (List.length l_files_only_on_system) $
       List.repeat UUID4.nextRandom)
  let file_to_uuid__only_on_system =
        zip l_files_only_on_system uuid__only_on_system
  -- DB operations
  D.withConnection
    path_to_db
    (\conn -> do
       D.execute_ conn "BEGIN TRANSACTION;"
       mapM_ (addFileDetailsToDb cwd conn) file_to_uuid__only_on_system
       mapM_ (deleteFileFromDb cwd conn) (S.toList files_only_in_db)
       D.execute_ conn "COMMIT;")

  let file_to_uuid__accurate =
        M.unionWith
          (\_ y -> y)
          (M.filterWithKey
             (\k _ -> k `S.notMember` files_only_in_db)
             file_to_uuid__in_db)
          (M.fromList file_to_uuid__only_on_system)
  let files_and_uuid_sorted =
        sortBy
          (\x y ->
             case (x, y) of
               ((fn1, _), (fn2, _)) -> compare fn1 fn2) $
        (M.toList file_to_uuid__accurate)
  lines_to_write_to_file <-
    sequence $
    fmap
      (\(fn, h) -> do
         fn_perhaps_with_trailing_slash <- appendSlashToDir cwd fn
         return $ pack fn_perhaps_with_trailing_slash <> " | " <> h)
      files_and_uuid_sorted
  dirstate_filepath <-
    writeTempFile
      app_tmp_dir
      "dirst"
      (constructTextFileHeader cwd <>
       (toList $ intercalate "\n" lines_to_write_to_file))
  return (file_to_uuid__accurate, dirstate_filepath)
  where
    separateFilesIntoCategories :: [FilePath]
                                -> [[Char]]
                                -> (Set [Char], Set [Char], Set [Char])
    separateFilesIntoCategories files_on_system files_in_db =
      let set_system = S.fromList $ fmap toList files_on_system
          set_db = S.fromList files_in_db
      in ( set_system `S.intersection` set_db
         , set_system `S.difference` set_db
         , set_db `S.difference` set_system)

    selectFromDbAllFilesInDir path_to_db dirname =
      D.withConnection
        path_to_db
        (\conn ->
           D.query
             conn
             "SELECT filename, uuid FROM files WHERE dir = ?;"
             [dirname] :: IO [([Char], Text)])


parseUserDirStateFile :: ParsecT Text () Identity (Maybe (Text, Text))
parseUserDirStateFile = try commentLine <|> normalLine
  where
    commentLine = do
      char '"'
      manyTill anyChar eof
      return Nothing

    normalLine = do
      l <- manyTill anyChar (try . lookAhead $ (sepBarParser *> uuidParser) <* eof)
      sepBarParser
      s <- uuidParser
      return $ Just $ (pack l, pack s)

    sepBarParser = string " | "

    uuidParser = do
      s1 <- count 8 alphaNum
      char '-'
      s2 <- count 4 alphaNum
      char '-'
      s3 <- count 4 alphaNum
      char '-'
      s4 <- count 4 alphaNum
      char '-'
      s5 <- count 12 alphaNum
      return $ s1 <> "-" <> s2 <> "-" <> s3 <> "-" <> s4 <> "-" <> s5


data FilesTableRow = FilesTableRow FilePath FilePath Text deriving (Show)

instance FromRow FilesTableRow where
  fromRow = FilesTableRow <$> field <*> field <*> field

instance ToRow FilesTableRow where
  toRow (FilesTableRow dir filename file_uuid) = toRow (dir, filename, file_uuid)


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
  D.execute
    dbconn
    "UPDATE files SET dir=?, filename=? WHERE uuid=?;"
    (FilesTableRow dest_dir dest_filename uuid)

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


getFilenameAndUUIDInUserDirStateFile :: FilePath -> IO [(FilePath, Text)]
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
                      runIdentity $ runParserT parseUserDirStateFile () "" x
                in either (const (return ())) onlyYieldJust parse_result
              Nothing -> return ())

    onlyYieldJust j@(Just _) = yield j
    onlyYieldJust _ = return ()


dirToTrashCopyTo :: FilePath -> Text -> FilePath
dirToTrashCopyTo path_to_trashcopy_dir uuid =
  path_to_trashcopy_dir </> (toList uuid)


generateFileOps
  :: FilePath
  -> FilePath
  -> FilePath
  -> [(FilePath, Text)]
  -> Map FilePath Text
  -> IO [FileOp]
generateFileOps path_to_trashcopy_dir cwd path_to_db list_of_filename_and_uuid initial_filename_to_uuid = do
  let initial_filename_uuid_set = S.fromList $ M.toList initial_filename_to_uuid
  let current_filename_uuid_set = S.fromList list_of_filename_and_uuid
  let set_of_filename_uuid_to_trashcopy =
        S.difference initial_filename_uuid_set current_filename_uuid_set

  let list_of_filename_uuid_to_trashcopy =
        sortBy (\(fname_a, _) (fname_b, _) -> fname_a `compare` fname_b) $
        S.toList set_of_filename_uuid_to_trashcopy
  list_of_trashcopyop <-
    mapM
      (\(fname, uuid) -> do
         let dest_filerepr =
               FileRepr (dirToTrashCopyTo path_to_trashcopy_dir uuid) fname
         src_is_dir <- doesDirectoryExist $ cwd </> fname
         return $ TrashCopyOp (FileRepr cwd fname) dest_filerepr uuid src_is_dir)
      list_of_filename_uuid_to_trashcopy
  -- At this point, UUIDs in `initial_filename_to_uuid` are unique. Otherwise
  -- they would have violated the UNIQUE constraint on the `files.uuid` column.
  -- Hence, we can safely construct a Map indexed by UUID
  let trashcopy_uuid_to_trashcopyop =
        M.fromList $
        fmap (\op@(TrashCopyOp _ _ uuid _) -> (uuid, op)) list_of_trashcopyop

  let list_of_filename_uuid_to_copy =
        sortBy (\(fname_a, _) (fname_b, _) -> fname_a `compare` fname_b) $
        S.toList $
        S.difference current_filename_uuid_set initial_filename_uuid_set
  -- Map of UUID -> filename; for files that are in the current directory when
  -- the program started.

  let initial_uuid_to_filename =
        M.fromList $ fmap swap $ M.toList initial_filename_to_uuid

  list_of_copyop <-
    D.withConnection
      path_to_db
      (\dbconn ->
         mapM
           (\(fname, uuid) ->
              let dest_filerepr = FileRepr cwd fname
              in case M.lookup uuid trashcopy_uuid_to_trashcopyop of
                   Just (TrashCopyOp _ new_src_filerepr _ src_is_dir) -> do
                     return $ CopyOp new_src_filerepr dest_filerepr src_is_dir
                   Nothing ->
                     -- Source file is not to be trash copied.
                     -- See if we can find it in the initial set of files.
                     case M.lookup uuid initial_uuid_to_filename of
                       Just src_filename ->
                         makeCopyOpOrNoFileOp
                           (FileRepr cwd src_filename)
                           dest_filerepr
                       Nothing -> do
                         -- Need to perform database lookup
                         r <-
                           liftIO $
                           D.query
                             dbconn
                             "SELECT dir, filename, uuid FROM files WHERE uuid = ?"
                             [uuid]
                         case r of
                           (FilesTableRow src_dir src_filename _:_) ->
                             makeCopyOpOrNoFileOp
                               (FileRepr src_dir src_filename)
                               dest_filerepr
                           [] -> return NoFileOp)
           list_of_filename_uuid_to_copy)

  return $ list_of_trashcopyop <> filter (/= NoFileOp) list_of_copyop
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


createDirNoForce :: FilePath -> IO Bool
createDirNoForce app_data_dir = do
  path_exists <- doesPathExist app_data_dir
  if path_exists
    then do
      is_dir <- doesDirectoryExist app_data_dir
      if is_dir
        then return True
        else do
          putStrLn $
            "Error: `" <> (show app_data_dir) <>
            "` exists but is not a directory."
          return False
    else do
      createDirectory app_data_dir
      return True
