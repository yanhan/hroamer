module Hroamer.DataStructures
  ( FilePathUUIDPair
  , FileOpsReadState(..)
  , FileRepr(..)
  , filerepr_to_filepath
  ) where

import Data.Text (Text)
import Foundation
import System.FilePath.Posix ((</>), FilePath)

type FilePathUUIDPair = (FilePath, Text)

-- A file, broken down into its directory and filename
data FileRepr = FileRepr FilePath FilePath -- dir  file
  deriving (Eq, Show)

filerepr_to_filepath :: FileRepr -> FilePath
filerepr_to_filepath (FileRepr dir fname) = dir </> fname

data FileOpsReadState =
  FileOpsReadState { rsCwd :: FilePath
                   , rsPathToDb :: FilePath
                   , rsTrashCopyDir :: FilePath
                   }
