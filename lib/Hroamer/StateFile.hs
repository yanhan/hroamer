module Hroamer.StateFile
  ( create
  ) where

import Control.Monad (sequence)
import Data.Text (pack)
import Foundation
import System.FilePath.Posix (FilePath)
import System.IO.Temp (writeTempFile)

import Hroamer.DataStructures (FilePathUUIDPair)
import qualified Hroamer.Path as Path

create :: FilePath -> FilePath -> [FilePathUUIDPair] -> IO FilePath
create cwd appTmpDir filesAndUuidAccurate = do
  let filesAndUuidSorted =
        sortBy (\(fn1, _) (fn2, _) -> compare fn1 fn2) filesAndUuidAccurate
  linesToWriteToFile <-
    sequence $
    fmap
      (\(fn, uuid) -> do
         fnPerhapsWithTrailingSlash <- Path.appendSlashToDir cwd fn
         return $ pack fnPerhapsWithTrailingSlash <> " | " <> uuid)
      filesAndUuidSorted
  writeTempFile
    appTmpDir
    "dirst"
    (constructTextFileHeader cwd <>
     (toList $ intercalate "\n" linesToWriteToFile))
  where
    constructTextFileHeader :: FilePath -> [Char]
    constructTextFileHeader cwd = "\" pwd: " <> (toList cwd) <> "\n"
