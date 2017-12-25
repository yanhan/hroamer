{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hroamer.DataStructures
  ( AbsFilePath(..)
  , AbsFilePathUUIDPair
  , FilePathUUIDPair
  , FileOpsReadState(..)
  , FileRepr(..)
  , fileReprToFilePath
  ) where

import Data.Text (Text)
import Foundation
import System.FilePath.Posix ((</>), FilePath)

-- newtype wrapper for FilePath that are absolute paths
newtype AbsFilePath = AbsFilePath { toFilePath :: FilePath }
  deriving (Eq, IsString, Ord)

type AbsFilePathUUIDPair = (AbsFilePath, Text)

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
