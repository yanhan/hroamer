module Hroamer.UnsupportedPaths.Internal
  ( UPaths(..)
  , duplicatePathsErrorTitle
  , formatPathsForErrorMessage
  , invalidPathsErrorTitle
  ) where

import qualified Data.List as List
import Data.Set (Set)
import Data.Text (Text, pack)
import Foundation
import System.FilePath.Posix (FilePath)

-- Types of paths that are not supported by hroamer
data UPaths = UPaths
  { duplicatePaths :: Set FilePath
  , invalidPaths :: Set FilePath
  } deriving (Eq, Show)

duplicatePathsErrorTitle :: Text
duplicatePathsErrorTitle = "Error: the following filenames are duplicated:"

invalidPathsErrorTitle :: Text
invalidPathsErrorTitle = "Error: the following paths are invalid:"

formatPathsForErrorMessage :: [FilePath] -> [Text]
formatPathsForErrorMessage = List.sort . fmap (("- " <>) . pack)
