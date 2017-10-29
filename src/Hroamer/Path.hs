module Hroamer.Path
  ( appendSlashToDir
  , isWeakAncestorDir
  ) where

import Foundation
import System.Directory (doesDirectoryExist)
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
