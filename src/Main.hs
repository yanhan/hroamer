module Main where

import Control.Exception (catch, IOException)
import Control.Monad (join)
import Crypto.Hash (Context, Digest, HashAlgorithm, SHA1, hash, hashFinalize, hashInit, hashUpdate, hashFinalize)
import qualified Data.ByteString.Char8 as B8
import Data.Monoid ((<>))
import Data.Functor.Identity (Identity)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Database.SQLite.Simple as D
import Foundation
import Foundation.Collection (mapM, mapM_, zip, zipWith)
import Prelude (print)
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
  create_app_data_dir app_data_dir app_data_dir_exists

  let app_tmp_dir = app_data_dir </> "tmp"
  app_tmp_dir_exists <- doesDirectoryExist app_tmp_dir
  create_app_tmp_dir app_tmp_dir app_tmp_dir_exists

  let path_to_db = app_data_dir </> "hroamer.db"
  create_db_and_tables path_to_db

  dirstate_filepath <- process_cwd app_tmp_dir path_to_db
  let user_dirstate_filepath = (takeDirectory dirstate_filepath) </>
                                 ("user-" <> takeBaseName dirstate_filepath)
  copyFile dirstate_filepath user_dirstate_filepath

  let signals_to_handle = [keyboardSignal, softwareStop, softwareTermination]
  let handler = Catch $
                  removeFile dirstate_filepath `catch` exc_handler >>
                    removeFile user_dirstate_filepath `catch` exc_handler
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
  removeFile dirstate_filepath `catch` exc_handler
  removeFile user_dirstate_filepath `catch` exc_handler
  where
    exc_handler :: IOException -> IO ()
    exc_handler = const $ return ()


create_db_and_tables :: FilePath -> IO ()
create_db_and_tables path_to_db = do
  db_exists <- doesFileExist path_to_db
  if not db_exists
    then D.withConnection path_to_db (\conn -> do
      D.execute_ conn "CREATE TABLE IF NOT EXISTS files(dir TEXT, filename TEXT, hash TEXT, CONSTRAINT files__idx_dir_filename UNIQUE(dir, filename) ON CONFLICT ROLLBACK, CONSTRAINT files__hash UNIQUE(hash) ON CONFLICT ROLLBACK);"
      D.execute_ conn "CREATE INDEX files__idx_dir ON files(dir);"
    )
    else return ()


process_cwd :: FilePath -> FilePath -> IO FilePath
process_cwd app_tmp_dir path_to_db = do
  cwd <- getCurrentDirectory
  all_files <- join $ fmap (mapM (append_slash_to_dirs cwd)) $ listDirectory cwd
  hashes <- mapM compute_hash all_files
  let files_and_hashes_sorted = sortBy
        (\x y -> case (x, y) of
                   ((fn1, _), (fn2, _)) -> compare fn1 fn2) $
        (zip (fmap toList all_files) hashes :: [([Char], [Char])])
  let x = fmap (\(fn, h) -> pack fn <> " | " <> pack h) files_and_hashes_sorted
  mapM_ TIO.putStrLn x
  dirstate_filepath <- writeTempFile app_tmp_dir "dirst"
    (toList $ intercalate "\n" x)
  return dirstate_filepath
  where
    append_slash_to_dirs :: FilePath -> FilePath -> IO FilePath
    append_slash_to_dirs dirname filename = do
      isdir <- doesDirectoryExist $ dirname </> filename
      if isdir
        then return $ addTrailingPathSeparator filename
        else return filename

    compute_hash :: FilePath -> IO [Char]
    compute_hash path_to_file = do
      mod_time <- getModificationTime path_to_file
      let mod_time_string = formatTime
                              defaultTimeLocale "%Y-%m-%d %H:%M:%S" mod_time
      let digest = hash . encodeUtf8 $
                     (pack path_to_file <> pack mod_time_string) :: Digest SHA1
      return . toList $ show digest

create_app_tmp_dir :: FilePath -> Bool -> IO ()
create_app_tmp_dir app_tmp_dir False = do
  path_exists <- doesPathExist app_tmp_dir
  if path_exists
     then removeFile app_tmp_dir
     else return ()
  createDirectory app_tmp_dir

create_app_tmp_dir app_tmp_dir True = return ()


create_app_data_dir :: FilePath -> Bool -> IO ()
create_app_data_dir app_data_dir False = do
  path_exists <- doesPathExist app_data_dir
  if path_exists
     then do
       putStrLn $ "Error: `" <> (show app_data_dir) <> "` exists but is not a directory. Exiting."
       exitWith $ ExitFailure 1
     else createDirectory app_data_dir

create_app_data_dir app_data_dir True = return ()
