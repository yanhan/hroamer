module Hroamer.UnsupportedPaths.Internal
  ( UPaths(..)
  ) where

import Data.Set (Set)
import Foundation
import System.FilePath.Posix (FilePath)

-- Types of paths that are not supported by hroamer
data UPaths = UPaths
  { duplicatePaths :: Set FilePath
  , absPaths :: Set FilePath
  , filesNotInCwd :: Set FilePath
  , invalidPaths :: Set FilePath
  } deriving (Eq, Show)
