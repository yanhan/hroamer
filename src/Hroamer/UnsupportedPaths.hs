module Hroamer.UnsupportedPaths
  ( getDuplicateFilenames
  , getUnsupportedPaths
  , printUnsupportedPathsErrors
  ) where

import Control.Monad (foldM, mapM_)
import qualified Data.List as List
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as S
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Foundation
import System.Directory (canonicalizePath)
import System.FilePath.Posix
       ((</>), FilePath, isAbsolute, isValid, takeDirectory)

getDuplicateFilenames :: [FilePath] -> Set FilePath
getDuplicateFilenames =
  fst .
  foldr
    (\fname (dup_fnames, all_fnames) ->
       if member fname all_fnames
         then (insert fname dup_fnames, all_fnames)
         else (dup_fnames, insert fname all_fnames))
    (empty, empty)

getUnsupportedPaths :: FilePath
                    -> [FilePath]
                    -> IO (Set FilePath, Set FilePath, Set FilePath)
getUnsupportedPaths cwd =
  foldM
    (\acc@(abs_paths, files_not_in_cwd, invalid_paths) fname ->
       if isAbsolute fname
         then return (insert fname abs_paths, files_not_in_cwd, invalid_paths)
         else do
           path <- canonicalizePath $ cwd </> fname
           if not $ isValid path
             then return $
                  (abs_paths, files_not_in_cwd, insert fname invalid_paths)
             else if takeDirectory path /= cwd
                    then return $
                         ( abs_paths
                         , insert fname files_not_in_cwd
                         , invalid_paths)
                    else return acc)
    (empty, empty, empty)

printUnsupportedPathsErrors :: FilePath
                            -> (Set FilePath, Set FilePath, Set FilePath)
                            -> IO ()
printUnsupportedPathsErrors cwd (abs_paths, files_not_in_cwd, invalid_paths)
  -- TODO: Separate the error messages by a newline if necessary
 = do
  if not $ S.null abs_paths
    then do
      TIO.putStrLn
        "Error: Absolute paths not supported. We found that you entered these:"
      mapM_
        (\s -> TIO.putStrLn $ "- " <> (pack s))
        (List.sort $ S.toList abs_paths)
    else return ()
  if not $ S.null files_not_in_cwd
    then do
      TIO.putStrLn $
        "Error: the following paths are housed in a directory that is not the current directory " <>
        (pack cwd)
      mapM_
        (\s -> TIO.putStrLn $ "- " <> (pack s))
        (List.sort $ S.toList files_not_in_cwd)
    else return ()
  if not $ S.null invalid_paths
    then do
      TIO.putStrLn "Error: the following paths are invalid:"
      mapM_
        (\s -> TIO.putStrLn $ "- " <> (pack s))
        (List.sort $ S.toList invalid_paths)
    else return ()
