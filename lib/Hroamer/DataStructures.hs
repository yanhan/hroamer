module Hroamer.DataStructures
  ( FilePathUUIDPair
  ) where

import Data.Text (Text)
import System.FilePath.Posix (FilePath)

type FilePathUUIDPair = (FilePath, Text)
