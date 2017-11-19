module Hroamer.UnsupportedPaths.Internal
  ( UPaths(..)
  , absolutePathsErrorTitle
  , duplicatePathsErrorTitle
  , formatPathsForErrorMessage
  , invalidPathsErrorTitle
  , relativePathsErrorTitle
  ) where

import qualified Data.List as List
import Data.Set (Set)
import Data.Text (Text, pack)
import Foundation
import System.FilePath.Posix (FilePath)

-- Types of paths that are not supported by hroamer
data UPaths = UPaths
  { duplicatePaths :: Set FilePath
  , absPaths :: Set FilePath
  , filesNotInCwd :: Set FilePath
  , invalidPaths :: Set FilePath
  } deriving (Eq, Show)

duplicatePathsErrorTitle :: Text
duplicatePathsErrorTitle = "Error: the following filenames are duplicated:"

absolutePathsErrorTitle :: Text
absolutePathsErrorTitle =
  "Error: Absolute paths not supported. We found that you entered these:"

relativePathsErrorTitle :: Text -> Text
relativePathsErrorTitle cwd =
  "Error: the following paths are housed in a directory that is not the current directory " <>
  cwd

invalidPathsErrorTitle :: Text
invalidPathsErrorTitle = "Error: the following paths are invalid:"

formatPathsForErrorMessage :: [FilePath] -> [Text]
formatPathsForErrorMessage = List.sort . fmap (("- " <>) . pack)
