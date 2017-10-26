module Hroamer.UnsupportedPaths
  ( getDuplicateFilenames
  ) where

import Data.Set (Set, empty, insert, member)
import Foundation
import System.FilePath.Posix (FilePath)

getDuplicateFilenames :: [FilePath] -> Set FilePath
getDuplicateFilenames =
  fst .
  foldr
    (\fname (dup_fnames, all_fnames) ->
       if member fname all_fnames
         then (insert fname dup_fnames, all_fnames)
         else (dup_fnames, insert fname all_fnames))
    (empty, empty)
