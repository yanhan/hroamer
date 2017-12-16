module Hroamer.DataStructures
  ( FilePathUUIDPair
  , FileOpsReadState(..)
  , FileRepr(..)
  , fileReprToFilePath
  ) where

import Data.Text (Text)
import Foundation
import System.FilePath.Posix ((</>), FilePath)

type FilePathUUIDPair = (FilePath, Text)

-- A file, broken down into its directory and filename
data FileRepr = FileRepr FilePath FilePath -- dir  file
  deriving (Eq, Show)

fileReprToFilePath :: FileRepr -> FilePath
fileReprToFilePath (FileRepr dir fname) = dir </> fname

data FileOpsReadState =
  FileOpsReadState { rsCwd :: FilePath
                   , rsPathToDb :: FilePath
                   , rsTrashCopyDir :: FilePath
                   }
