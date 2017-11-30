module Hroamer.StateFile
  ( create
  , read
  ) where

import Conduit (decodeUtf8C, lineC, peekForeverE, sinkList)
import Control.Monad (sequence)
import Data.Conduit ((.|), await, runConduitRes, yield)
import Data.Conduit.Binary (sourceFile)
import Data.Maybe (fromJust)
import Data.Text (pack)
import Foundation
import System.FilePath.Posix (FilePath, dropTrailingPathSeparator)
import System.IO.Temp (writeTempFile)
import Text.Parsec (runParser)

import Hroamer.DataStructures (FilePathUUIDPair)
import qualified Hroamer.Parser as Parser
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


read :: FilePath -> IO [FilePathUUIDPair]
read user_dirstate_filepath = do
  list_of_maybe_fname_uuid <-
    runConduitRes $
    sourceFile user_dirstate_filepath .| decodeUtf8C .|
    peekForeverE parseLineConduit .|
    sinkList
  return $
    fmap
      (\x ->
         let (fn, uuid) = fromJust x
         in ((dropTrailingPathSeparator . toList) fn, uuid))
      list_of_maybe_fname_uuid
  where
    parseLineConduit =
      lineC
        (do maybeLine <- await
            case maybeLine of
              Just line ->
                let parse_result =
                      runParser Parser.parseDirStateLine () "" line
                in either (const $ return ()) onlyYieldJust parse_result
              Nothing -> return ())
    onlyYieldJust j@(Just _) = yield j
    onlyYieldJust _ = return ()
