{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Exception (catch, IOException)
import Control.Monad (join)
import Crypto.Hash (Context, Digest, HashAlgorithm, SHA1, hash, hashFinalize, hashInit, hashUpdate, hashFinalize)
import qualified Data.ByteString.Char8 as B8
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Functor.Identity (Identity)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Database.SQLite.Simple as D
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow, field)
import Foundation
import Foundation.Collection (mapM, mapM_, zip, zipWith)
import System.Directory (XdgDirectory(XdgData), copyFile, createDirectory, doesDirectoryExist, doesFileExist, doesPathExist, getCurrentDirectory, getModificationTime, getXdgDirectory, getHomeDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), die, exitWith)
import System.FilePath.Posix (FilePath, (</>), addTrailingPathSeparator, takeDirectory, takeBaseName)
import System.IO.Temp (writeTempFile)
import System.Posix.Signals (Handler(Catch), addSignal, emptySignalSet, installHandler, keyboardSignal, siginfoSignal, softwareStop, softwareTermination)
import System.Process (createProcess, proc, waitForProcess)

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

  dirstate_filepath <- processCwd app_tmp_dir path_to_db
  let user_dirstate_filepath = (takeDirectory dirstate_filepath) </>
                                 ("user-" <> takeBaseName dirstate_filepath)
  copyFile dirstate_filepath user_dirstate_filepath

  let signals_to_handle = [keyboardSignal, softwareStop, softwareTermination]
  let handler = Catch $
                  removeFile dirstate_filepath `catch` excHandler >>
                    removeFile user_dirstate_filepath `catch` excHandler
  mapM_ (\signal -> installHandler signal handler Nothing) signals_to_handle

  -- Launch vim to let user edit
  (_, _, _, editor_process) <- createProcess (proc "vim" [dirstate_filepath])
  waitForProcess editor_process

  -- Compare for difference between the files
  (_, _, _, cmp_process) <- createProcess
    (proc "cmp" ["--silent", dirstate_filepath, user_dirstate_filepath])
  cmp_exit_code <- waitForProcess cmp_process
  case cmp_exit_code of
    ExitSuccess -> return ()
    _ -> putStrLn "Changed!"

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
addFileDetailsToDb cwd conn (filename, file_hash) =
  D.execute conn
    "INSERT INTO files(dir, filename, hash) VALUES(?, ?, ?);"
    [cwd, filename, file_hash]

processCwd :: FilePath -> FilePath -> IO FilePath
processCwd app_tmp_dir path_to_db = do
  cwd <- getCurrentDirectory
  files__on_system <- join $ fmap (mapM (appendSlashToDirs cwd)) $ listDirectory cwd
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
    (toList $ intercalate "\n" lines_to_write_to_file)
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

    appendSlashToDirs :: FilePath -> FilePath -> IO FilePath
    appendSlashToDirs dirname filename = do
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
