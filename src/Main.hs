{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Conduit (decodeUtf8C, lineC, peekForeverE, sinkList)
import Control.Applicative ((<*), (*>))
import Control.Exception (catch, IOException)
import Control.Monad (forM_, join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Crypto.Hash (Context, Digest, HashAlgorithm, SHA1, hash, hashFinalize, hashInit, hashUpdate, hashFinalize)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char
import Data.Conduit ((.|), ConduitM, await, runConduitRes, yield)
import Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.List as CL
import Data.Either (either)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Functor.Identity (Identity)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Void (Void)
import qualified Database.SQLite.Simple as D
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow, field)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
import Foundation hiding ((<|>))
import Foundation.Collection (mapM, mapM_, zip, zipWith)
import Prelude (print)
import System.Directory (XdgDirectory(XdgData), copyFile, createDirectory, doesDirectoryExist, doesFileExist, doesPathExist, getCurrentDirectory, getModificationTime, getXdgDirectory, getHomeDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), die, exitWith)
import System.FilePath.Posix (FilePath, (</>), addTrailingPathSeparator, dropTrailingPathSeparator, takeDirectory, takeBaseName)
import System.IO.Temp (writeTempFile)
import System.Posix.Signals (Handler(Catch), addSignal, emptySignalSet, installHandler, keyboardSignal, siginfoSignal, softwareStop, softwareTermination)
import System.Process (createProcess, proc, waitForProcess)
import Text.Parsec ((<|>), ParsecT, anyChar, char, count, eof, hexDigit, lookAhead, manyTill, runParserT, string, try)

main :: IO ()
main = do
  home_dir <- getHomeDirectory
  app_data_dir <- getXdgDirectory XdgData "hroamer"
  app_data_dir_exists <- doesDirectoryExist app_data_dir
  createAppDataDir app_data_dir app_data_dir_exists

  let app_tmp_dir = app_data_dir </> "tmp"
  app_tmp_dir_exists <- doesDirectoryExist app_tmp_dir
  createAppTmpDir app_tmp_dir app_tmp_dir_exists

  let path_to_db = app_data_dir </> "hroamer.db"
  createDbAndTables path_to_db

  cwd <- getCurrentDirectory
  dirstate_filepath <- processCwd cwd app_tmp_dir path_to_db
  let user_dirstate_filepath = (takeDirectory dirstate_filepath) </>
                                 ("user-" <> takeBaseName dirstate_filepath)
  copyFile dirstate_filepath user_dirstate_filepath

  let signals_to_handle = [keyboardSignal, softwareStop, softwareTermination]
  let handler = Catch $
                  removeFile dirstate_filepath `catch` excHandler >>
                    removeFile user_dirstate_filepath `catch` excHandler
  mapM_ (\signal -> installHandler signal handler Nothing) signals_to_handle

  -- Launch vim to let user edit
  (_, _, _, editor_process) <- createProcess (proc "vim" [user_dirstate_filepath])
  waitForProcess editor_process

  -- Compare for difference between the files
  (_, _, _, cmp_process) <- createProcess
    (proc "cmp" ["--silent", dirstate_filepath, user_dirstate_filepath])
  cmp_exit_code <- waitForProcess cmp_process
  case cmp_exit_code of
    ExitSuccess -> return ()
    _ -> do
      list_of_filename_and_hash <- getFilenameAndHashInUserDirStateFile user_dirstate_filepath
      file_op_list <- generateFileOps cwd path_to_db list_of_filename_and_hash
      D.withConnection path_to_db (\conn -> do
          forM_ file_op_list (doFileOp conn)
        )

  -- cleanup
  removeFile dirstate_filepath `catch` excHandler
  removeFile user_dirstate_filepath `catch` excHandler
  where
    excHandler :: IOException -> IO ()
    excHandler = const $ return ()


createDbAndTables :: FilePath -> IO ()
createDbAndTables path_to_db = do
  db_exists <- doesFileExist path_to_db
  if not db_exists
    then D.withConnection path_to_db (\conn -> do
      D.execute_ conn "CREATE TABLE IF NOT EXISTS files(dir TEXT, filename TEXT, hash TEXT, CONSTRAINT files__idx_dir_filename UNIQUE(dir, filename) ON CONFLICT ROLLBACK, CONSTRAINT files__hash UNIQUE(hash) ON CONFLICT ROLLBACK);"
      D.execute_ conn "CREATE INDEX files__idx_dir ON files(dir);"
    )
    else return ()

deleteFileFromDb :: FilePath -> D.Connection -> [Char] -> IO ()
deleteFileFromDb cwd conn filename =
  D.execute conn "DELETE FROM files WHERE dir = ? AND filename = ?;"
  [cwd, filename]

addFileDetailsToDb :: FilePath -> D.Connection -> ([Char], [Char]) -> IO ()
addFileDetailsToDb dir conn (filename, file_hash) =
  D.execute conn
    "INSERT INTO files(dir, filename, hash) VALUES(?, ?, ?);"
    [dir, filename, file_hash]

instance FromRow Int where
  fromRow = field

addFileDetailsToDbIfNotFound :: D.Connection -> FilePath -> FilePath -> IO ()
addFileDetailsToDbIfNotFound conn dir filename = do
  [nr_rows] <- D.query conn "SELECT COUNT(1) FROM files WHERE dir = ? AND filename = ?"
                 [dir, filename] :: IO [Int]
  if nr_rows == 0
     then do
       filehash <- computeHash $ dir </> filename
       addFileDetailsToDb dir conn (filename, filehash)
     else return ()

appendSlashToDir :: FilePath -> FilePath -> IO FilePath
appendSlashToDir dirname filename = do
  isdir <- doesDirectoryExist $ dirname </> filename
  if isdir
    then return $ addTrailingPathSeparator filename
    else return filename

computeHash :: FilePath -> IO [Char]
computeHash path_to_file = do
  mod_time <- getModificationTime path_to_file
  let mod_time_string = formatTime
                          defaultTimeLocale "%Y-%m-%d %H:%M:%S" mod_time
  let digest = hash . encodeUtf8 $
                 (pack path_to_file <> pack mod_time_string) :: Digest SHA1
  return . toList $ show digest

constructTextFileHeader :: FilePath -> [Char]
constructTextFileHeader cwd = "\" pwd: " <> (toList cwd) <> "\n"

processCwd :: FilePath -> FilePath -> FilePath -> IO FilePath
processCwd cwd app_tmp_dir path_to_db = do
  files__on_system <- join $ fmap (mapM (appendSlashToDir cwd)) $ listDirectory cwd
  files_and_hashes__in_db <- selectFromDbAllFilesInDir path_to_db cwd
  let file_to_hash__in_db = M.fromList files_and_hashes__in_db
  let files__in_db = fmap fst files_and_hashes__in_db
  let (files_on_both, files_only_on_system, files_only_in_db) =
        separateFilesIntoCategories files__on_system files__in_db
  let l_files_only_on_system = S.toList files_only_on_system
  hash__only_on_system <- mapM computeHash l_files_only_on_system
  let file_to_hash__only_on_system = zip l_files_only_on_system
                                        hash__only_on_system
  -- DB operations
  D.withConnection path_to_db (\conn -> do
      D.execute_ conn "BEGIN TRANSACTION;"
      mapM_ (addFileDetailsToDb cwd conn) file_to_hash__only_on_system
      mapM_ (deleteFileFromDb cwd conn) (S.toList files_only_in_db)
      D.execute_ conn "COMMIT;"
    )

  let file_to_hash__accurate = M.unionWith (\_ y -> y)
        (M.filterWithKey (\k _ -> k `S.notMember` files_only_in_db) file_to_hash__in_db)
        (M.fromList file_to_hash__only_on_system)
  let files_and_hashes_sorted = sortBy
        (\x y -> case (x, y) of
                   ((fn1, _), (fn2, _)) -> compare fn1 fn2) $
        (M.toList file_to_hash__accurate)
  let lines_to_write_to_file = fmap (\(fn, h) -> pack fn <> " | " <> pack h)
                                 files_and_hashes_sorted
  dirstate_filepath <- writeTempFile app_tmp_dir "dirst"
    (constructTextFileHeader cwd <>
      (toList $ intercalate "\n" lines_to_write_to_file))
  return dirstate_filepath
  where
    separateFilesIntoCategories :: [FilePath] -> [[Char]] -> (Set [Char], Set [Char], Set [Char])
    separateFilesIntoCategories files_on_system files_in_db =
      let set_system = S.fromList $ fmap toList files_on_system
          set_db = S.fromList files_in_db
       in (set_system `S.intersection` set_db,
           set_system `S.difference` set_db,
           set_db `S.difference` set_system)

    selectFromDbAllFilesInDir path_to_db dirname =
      D.withConnection path_to_db (\conn ->
        D.query conn "SELECT filename, hash FROM files WHERE dir = ?;" [dirname] :: IO [([Char], [Char])]
      )

parseUserDirStateFile :: ParsecT Text () Identity (Maybe (Text, Text))
parseUserDirStateFile = try commentLine <|> normalLine
  where
    commentLine = do
      char '"'
      manyTill anyChar eof
      return Nothing

    normalLine = do
      l <- manyTill anyChar (try . lookAhead $ (sepBarParser *> sha1Parser) <* eof)
      sepBarParser
      s <- sha1Parser
      return $ Just $ (pack l, pack s)

    sepBarParser = string " | "
    sha1Parser = count 40 hexDigit

data FilesTableRow = FilesTableRow FilePath FilePath Text deriving (Show)

instance FromRow FilesTableRow where
  fromRow = FilesTableRow <$> field <*> field <*> field

instance ToRow FilesTableRow where
  toRow (FilesTableRow dir filename file_hash) = toRow (dir, filename, file_hash)

data FileOp = CopyOp { srcDir :: FilePath
                     , srcFilename :: FilePath
                     , destDir :: FilePath
                     , destFilename :: FilePath
                     , fileHash :: Text }
            | NoFileOp

-- Assume both src and dest are in the same directory
doFileOp :: D.Connection -> FileOp -> IO ()

-- Do nothing
doFileOp _ NoFileOp = return ()

doFileOp dbconn (CopyOp src_dir src_filename dest_dir dest_filename filehash) = do
  let path_to_src = src_dir </> src_filename
  let path_to_dest = dropTrailingPathSeparator $ dest_dir </> dest_filename
  src_is_dir <- doesDirectoryExist path_to_src
  dest_is_dir <- doesDirectoryExist path_to_dest
  if dest_is_dir
     then do
       -- Remove the destination directory
       removeDirectoryRecursive path_to_dest
       TIO.putStrLn $ "rm -rf " <> (pack path_to_dest)
       if src_is_dir
          then do
            TIO.putStrLn $
              "cp -R " <> (pack path_to_src) <> " " <> (pack path_to_dest)
            (_, _, _, ph) <- createProcess (proc "cp" ["-R", path_to_src, path_to_dest])
            file_op_exit_code <- waitForProcess ph
            case file_op_exit_code of
              ExitSuccess -> do
                TIO.putStrLn $
                  "cp -R " <> (pack path_to_src) <> " " <> (pack path_to_dest)
                addFileDetailsToDbIfNotFound dbconn dest_dir dest_filename

              _ -> return()
          else do
            TIO.putStrLn $ "cp " <> (pack path_to_src) <> " " <> (pack path_to_dest)
            (_, _, _, ph) <- createProcess (proc "cp" [path_to_src, path_to_dest])
            file_op_exit_code <- waitForProcess ph
            case file_op_exit_code of
              ExitSuccess -> do
                TIO.putStrLn $ "cp " <> (pack path_to_src) <> " " <> (pack path_to_dest)
                addFileDetailsToDbIfNotFound dbconn dest_dir dest_filename

              _ -> return ()
     else do
       -- Destination is not a directory. And there may be nothing there.
       dest_exists <- doesFileExist path_to_dest
       if dest_exists
          then do
            (removeFile path_to_dest >>
              (TIO.putStrLn $ "rm " <> (pack path_to_dest)) >>
              addFileDetailsToDbIfNotFound dbconn dest_dir dest_filename
              ) `catch` (const $ return () :: IOException -> IO ())
          else return ()
       if src_is_dir
          then do
            (_, _, _, ph) <- createProcess (proc "cp" ["-R", path_to_src, path_to_dest])
            file_op_exit_code <- waitForProcess ph
            case file_op_exit_code of
              ExitSuccess -> do
                TIO.putStrLn $ "cp -R " <> (pack path_to_src) <> " " <> (pack path_to_dest)
                dest_hash <- computeHash path_to_dest
                if dest_exists
                   then return ()
                   else addFileDetailsToDb dest_dir dbconn (dest_filename, dest_hash)
              _ -> return ()
          else do
            copyFile path_to_src path_to_dest
            TIO.putStrLn $ "cp " <> (pack path_to_src) <> " " <> (pack path_to_dest)
            dest_hash <- computeHash path_to_dest
            if dest_exists
               then return ()
               else addFileDetailsToDb dest_dir dbconn (dest_filename, dest_hash)


getFilenameAndHashInUserDirStateFile :: FilePath -> IO [Maybe (Text, Text)]
getFilenameAndHashInUserDirStateFile user_dirstate_filepath =
  runConduitRes $ sourceFile user_dirstate_filepath .| decodeUtf8C .|
    peekForeverE parseLineConduit .| sinkList
  where
    parseLineConduit = lineC (do
        mx <- await
        case mx of
          Just x ->
            let parse_result = runIdentity $ runParserT parseUserDirStateFile () "" x
            in either (const (return ())) onlyYieldJust parse_result
          Nothing -> return ()
      )

    onlyYieldJust j@(Just _) = yield j
    onlyYieldJust _ = return ()

generateFileOps :: FilePath -> FilePath -> [Maybe (Text, Text)] -> IO [FileOp]
generateFileOps cwd path_to_db list_of_filename_and_hash =
  D.withConnection path_to_db (\conn ->
    mapM (getFileOp conn) list_of_filename_and_hash
  )
  where
    getFileOp :: D.Connection -> Maybe (Text, Text) -> IO FileOp
    getFileOp dbconn (Just (filename_text, file_hash)) = do
      let filename = toList filename_text
      r <- liftIO $ D.query dbconn
        "SELECT dir, filename, hash FROM files WHERE hash = ?" [file_hash]
      case r of
        (FilesTableRow dir_in_table fname_in_table hash_in_table : _) ->
          if dir_in_table == cwd && fname_in_table == filename
             then return NoFileOp
             else return $
               CopyOp dir_in_table fname_in_table cwd filename hash_in_table
        [] -> return NoFileOp

createAppTmpDir :: FilePath -> Bool -> IO ()
createAppTmpDir app_tmp_dir False = do
  path_exists <- doesPathExist app_tmp_dir
  if path_exists
     then removeFile app_tmp_dir
     else return ()
  createDirectory app_tmp_dir

createAppTmpDir app_tmp_dir True = return ()


createAppDataDir :: FilePath -> Bool -> IO ()
createAppDataDir app_data_dir False = do
  path_exists <- doesPathExist app_data_dir
  if path_exists
     then do
       putStrLn $ "Error: `" <> (show app_data_dir) <> "` exists but is not a directory. Exiting."
       exitWith $ ExitFailure 1
     else createDirectory app_data_dir

createAppDataDir app_data_dir True = return ()
