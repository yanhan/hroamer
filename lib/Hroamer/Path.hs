module Hroamer.Path
  ( appendSlashToDir
  , createDirNoForce
  , isWeakAncestorDir
  ) where

import Foundation
import System.Directory
       (createDirectory, doesDirectoryExist, doesPathExist)
import System.FilePath.Posix
       (FilePath, (</>), addTrailingPathSeparator, takeDirectory)

isWeakAncestorDir :: FilePath -> FilePath -> Bool
isWeakAncestorDir suspected_ancestor "/" = suspected_ancestor == "/"
isWeakAncestorDir suspected_ancestor dir_of_interest =
  if suspected_ancestor == dir_of_interest
    then True
    else isWeakAncestorDir suspected_ancestor $ takeDirectory dir_of_interest

appendSlashToDir :: FilePath -> FilePath -> IO FilePath
appendSlashToDir dirname filename = do
  isdir <- doesDirectoryExist $ dirname </> filename
  if isdir
    then return $ addTrailingPathSeparator filename
    else return filename

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
