module Hroamer.UnsupportedPaths
  ( getDuplicateFilenames
  , getUnsupportedPaths
  , noUnsupportedPaths
  , printUnsupportedPathsErrors
  ) where

import Control.Monad (mapM_)
import Control.Monad.State
       (StateT, evalStateT, execStateT, get, lift, modify)
import qualified Data.List as List
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as S
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Foundation
import System.Directory (canonicalizePath)
import System.FilePath.Posix
       ((</>), FilePath, isAbsolute, isValid, takeDirectory)

-- Types of paths that are not supported by hroamer
data UPaths = UPaths
  { absPaths :: Set FilePath
  , filesNotInCwd :: Set FilePath
  , invalidPaths :: Set FilePath
  }

noUnsupportedPaths :: UPaths -> Bool
noUnsupportedPaths uPaths =
  S.null (absPaths uPaths) &&
  S.null (filesNotInCwd uPaths) && S.null (invalidPaths uPaths)

getDuplicateFilenames :: [FilePath] -> Set FilePath
getDuplicateFilenames =
  fst .
  foldr
    (\fname (dup_fnames, all_fnames) ->
       if member fname all_fnames
         then (insert fname dup_fnames, all_fnames)
         else (dup_fnames, insert fname all_fnames))
    (empty, empty)

getUnsupportedPaths :: FilePath -> [FilePath] -> IO UPaths
getUnsupportedPaths cwd files = execStateT sta (UPaths empty empty empty)
  where
    sta :: StateT UPaths IO ()
    sta =
      mapM_
        (\fname ->
           if isAbsolute fname
             then modify (\s -> s {absPaths = insert fname (absPaths s)})
             else do
               path <- lift $ canonicalizePath $ cwd </> fname
               if not $ isValid path
                 then modify
                        (\s -> s {invalidPaths = insert fname (invalidPaths s)})
                 else if takeDirectory path /= cwd
                        then modify
                               (\s ->
                                  s
                                  { filesNotInCwd =
                                      insert fname (filesNotInCwd s)
                                  })
                        else return ())
        files

printUnsupportedPathsErrors :: FilePath -> UPaths -> IO ()
printUnsupportedPathsErrors cwd uPaths
 -- the state is the number of errors so far and we use it to determine
 -- whether we need to print a separating newline between different error
 -- messages
 = evalStateT printErrors 0
  where
    printOneError :: Set FilePath -> Text -> StateT Int IO ()
    printOneError files msg = do
      if not $ S.null files
        then do
          nrErrors <- get
          if nrErrors > 0
            then lift $ TIO.putStrLn ""
            else return ()
          modify (+ 1)
          lift $ TIO.putStrLn msg
          lift $
            mapM_
              (\s -> TIO.putStrLn $ "- " <> (pack s))
              (List.sort $ S.toList files)
        else return ()
    printErrors :: StateT Int IO ()
    printErrors = do
      printOneError
        (absPaths uPaths)
        "Error: Absolute paths not supported. We found that you entered these:"
      printOneError
        (filesNotInCwd uPaths)
        ("Error: the following paths are housed in a directory that is not the current directory " <>
         pack cwd)
      printOneError
        (invalidPaths uPaths)
        "Error: the following paths are invalid:"
