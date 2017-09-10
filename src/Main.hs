module Main where

import Control.Exception (catch)
import Foundation
import Prelude (mapM_, print, readFile)
import System.Directory (XdgDirectory(XdgData), copyFile, createDirectory, doesDirectoryExist, doesPathExist, getCurrentDirectory, getXdgDirectory, getHomeDirectory, listDirectory, removeFile)
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

  cwd <- getCurrentDirectory
  all_files <- listDirectory cwd
  dirstate_filepath <- writeTempFile app_tmp_dir "dirst" (intercalate "\n" all_files)
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
  (_, _, _, cmp_process) <- createProcess (proc "cmp" ["--silent", dirstate_filepath, user_dirstate_filepath])
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
