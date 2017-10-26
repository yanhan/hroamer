module Hroamer.UnsupportedPaths
  ( getDuplicateFilenames
  , getUnsupportedPaths
  ) where

import Control.Monad (foldM)
import Data.Set (Set, empty, insert, member)
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
