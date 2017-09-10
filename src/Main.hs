module Main where

import Control.Exception (catch, IOException)
import Crypto.Hash (Context, Digest, HashAlgorithm, SHA1, hash, hashFinalize, hashInit, hashUpdate, hashFinalize)
import Data.ByteArray (convert)
import qualified Data.ByteString.Char8 as B8
import Data.Monoid ((<>))
import Data.Functor.Identity (Identity)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Database.SQLite.Simple as D
import Foundation
import Foundation.Collection (zipWith)
--import qualified GHC.Base
import Prelude (concat, mapM, mapM_, print, readFile)
import System.Directory (XdgDirectory(XdgData), copyFile, createDirectory, doesDirectoryExist, doesPathExist, getCurrentDirectory, getModificationTime, getXdgDirectory, getHomeDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), die, exitWith)
import System.FilePath.Posix (FilePath, (</>), takeDirectory, takeBaseName)
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
create_db_and_tables path_to_db = D.withConnection path_to_db
  (\conn -> D.execute_ conn "CREATE TABLE IF NOT EXISTS files(path_to_file TEXT, hash TEXT, CONSTRAINT files__path_to_file UNIQUE(path_to_file) ON CONFLICT ROLLBACK, CONSTRAINT files__hash UNIQUE(hash) ON CONFLICT ROLLBACK);")


process_cwd :: FilePath -> FilePath -> IO FilePath
process_cwd app_tmp_dir path_to_db = do
  cwd <- getCurrentDirectory
  all_files <- listDirectory cwd
  hashes <- mapM compute_hash all_files
  let x = zipWith (\x y -> x <> " | " <> y) (fmap pack all_files) hashes :: [Text]
  mapM_ TIO.putStrLn x
  dirstate_filepath <- writeTempFile app_tmp_dir "dirst"
    (toList $ intercalate "\n" x)
  return dirstate_filepath
  where
    compute_hash :: FilePath -> IO Text
    compute_hash path_to_file = do
      mod_time <- getModificationTime path_to_file
      let mod_time_string = formatTime
                              defaultTimeLocale "%Y-%m-%d %H:%M:%S" mod_time
      let digest = hash . encodeUtf8 $
                     (pack path_to_file <> pack mod_time_string) :: Digest SHA1
      return . decodeUtf8 . B8.pack . toList . show $ digest

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
