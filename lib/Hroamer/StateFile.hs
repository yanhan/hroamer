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
create cwd app_tmp_dir files_and_uuid__accurate = do
  let files_and_uuid_sorted =
        sortBy (\(fn1, _) (fn2, _) -> compare fn1 fn2) files_and_uuid__accurate
  lines_to_write_to_file <-
    sequence $
    fmap
      (\(fn, uuid) -> do
         fn_perhaps_with_trailing_slash <- Path.appendSlashToDir cwd fn
         return $ pack fn_perhaps_with_trailing_slash <> " | " <> uuid)
      files_and_uuid_sorted
  writeTempFile
    app_tmp_dir
    "dirst"
    (constructTextFileHeader cwd <>
     (toList $ intercalate "\n" lines_to_write_to_file))
  where
    constructTextFileHeader :: FilePath -> [Char]
    constructTextFileHeader cwd = "\" pwd: " <> (toList cwd) <> "\n"
