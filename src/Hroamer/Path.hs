module Hroamer.Path
  ( isWeakAncestorDir
  ) where

import Foundation
import System.FilePath.Posix (FilePath, takeDirectory)

isWeakAncestorDir :: FilePath -> FilePath -> Bool
isWeakAncestorDir suspected_ancestor "/" = suspected_ancestor == "/"
isWeakAncestorDir suspected_ancestor dir_of_interest =
  if suspected_ancestor == dir_of_interest
    then True
    else isWeakAncestorDir suspected_ancestor $ takeDirectory dir_of_interest
