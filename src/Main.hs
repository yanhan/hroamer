{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

  dirstate_filepath <- process_cwd app_tmp_dir path_to_db
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

newtype SelectCount = SelectCount Int deriving (Show, FromField)
instance FromRow SelectCount where
  fromRow = SelectCount <$> field

process_cwd :: FilePath -> FilePath -> IO FilePath
process_cwd app_tmp_dir path_to_db = do
  cwd <- getCurrentDirectory
  all_files <- join $ fmap (mapM (appendSlashToDirs cwd)) $ listDirectory cwd
  all_files_in_db <- selectFromDbAllFilesInDir path_to_db cwd
  mapM_ (\(x,y) -> TIO.putStrLn . pack $ x <> " *** " <> y) all_files_in_db
  hashes <- mapM computeHash all_files
  let files_and_hashes_sorted = sortBy
        (\x y -> case (x, y) of
                   ((fn1, _), (fn2, _)) -> compare fn1 fn2) $
        (zip (fmap toList all_files) hashes :: [([Char], [Char])])
  let lines_to_write_to_file = fmap (\(fn, h) -> pack fn <> " | " <> pack h)
                                 files_and_hashes_sorted
  mapM_ (addFileDetailsToDb cwd) files_and_hashes_sorted
  dirstate_filepath <- writeTempFile app_tmp_dir "dirst"
    (toList $ intercalate "\n" lines_to_write_to_file)
  return dirstate_filepath
  where
    addFileDetailsToDb :: FilePath -> ([Char], [Char]) -> IO ()
    addFileDetailsToDb cwd (filename, file_hash) =
      D.withConnection path_to_db (\conn -> do
        r <- D.query conn "SELECT COUNT(1) FROM files WHERE dir = ? AND filename = ?"
               [cwd, filename] :: IO [SelectCount]
        case r of
          (SelectCount cnt:xs) ->
            if cnt > 0
              then return ()
              else D.execute conn "INSERT INTO files(dir, filename, hash) VALUES(?, ?, ?)" [cwd, filename, file_hash]
          _ -> error "Query string \"SELECT COUNT(1) FROM files WHERE dir = ? AND filename = ?\" has no results. Not supposed to happen!"
      )

    selectFromDbAllFilesInDir path_to_db dirname =
      D.withConnection path_to_db (\conn ->
        D.query conn "SELECT filename, hash FROM files WHERE dir = ?" [dirname] :: IO [([Char], [Char])]
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
