module Hroamer.Path
  ( appendSlashToDir
  , createDirNoForce
  , hasSpace
  , isWeakAncestorDir
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Strict (WriterT, tell)
import Data.Char (isSpace)
import Data.DList (DList, singleton)
import Data.Text (Text, pack)
import Foundation hiding (singleton)
import System.Directory
       (createDirectory, doesDirectoryExist, doesPathExist)
import System.FilePath.Posix
       (FilePath, (</>), addTrailingPathSeparator,
        dropTrailingPathSeparator, takeDirectory)

isWeakAncestorDir :: FilePath -> FilePath -> Bool
isWeakAncestorDir suspected_ancestor dir_of_interest =
  helper
    (dropLeadingSlashesSaveOne $ dropTrailingPathSeparator suspected_ancestor)
    (dropLeadingSlashesSaveOne $ dropTrailingPathSeparator dir_of_interest)
  where
    helper :: FilePath -> FilePath -> Bool
    helper suspected_ancestor dir_of_interest
      | suspected_ancestor == dir_of_interest = True
      | otherwise =
        let nextDirUpwards = takeDirectory dir_of_interest
        in if nextDirUpwards == dir_of_interest
             then False
             else helper suspected_ancestor nextDirUpwards

    dropLeadingSlashesSaveOne :: FilePath -> FilePath
    dropLeadingSlashesSaveOne "" = ""
    dropLeadingSlashesSaveOne s@('/':_) = "/" <> dropWhile (== '/') s
    dropLeadingSlashesSaveOne s = s


appendSlashToDir :: FilePath -> FilePath -> IO FilePath
appendSlashToDir dirname filename = do
  isdir <- doesDirectoryExist $ dirname </> filename
  if isdir
    then return $ addTrailingPathSeparator filename
    else return filename

createDirNoForce :: FilePath -> WriterT (DList Text) IO Bool
createDirNoForce app_data_dir = do
  path_exists <- liftIO $ doesPathExist app_data_dir
  if path_exists
    then do
      is_dir <- liftIO $ doesDirectoryExist app_data_dir
      if is_dir
        then return True
        else do
          tell
            (singleton $
             "Error: `" <> (pack app_data_dir) <>
             "` exists but is not a directory.")
          return False
    else do
      liftIO $ createDirectory app_data_dir
      return True

hasSpace :: FilePath -> Bool
hasSpace filename = maybe False (const True) $ find isSpace filename
