module Hroamer.UnsupportedPaths
  ( getUnsupportedPaths
  , noUnsupportedPaths
  , printErrors
  ) where

import Control.Monad (mapM_)
import Control.Monad.State
       (StateT, evalStateT, execStateT, get, lift, modify, put)
import qualified Data.List as List
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as S
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Foundation
import System.Directory (canonicalizePath)
import System.FilePath.Posix
       ((</>), FilePath, isAbsolute, isValid, takeDirectory)

import Hroamer.UnsupportedPaths.Internal
       (UPaths(UPaths, duplicatePaths, absPaths, filesNotInCwd,
               invalidPaths))

noUnsupportedPaths :: UPaths -> Bool
noUnsupportedPaths uPaths =
  S.null (absPaths uPaths) &&
  S.null (filesNotInCwd uPaths) && S.null (invalidPaths uPaths)

getUnsupportedPaths :: FilePath -> [FilePath] -> IO UPaths
getUnsupportedPaths cwd files = do
  (_, uPaths) <- execStateT sta (empty, (UPaths empty empty empty empty))
  return uPaths
  where
    sta :: StateT (Set FilePath, UPaths) IO ()
    sta =
      mapM_
        (\fname -> do
           (filesSeen, uPaths) <- get
           canonPath <- lift $ canonicalizePath $ cwd </> fname
           let filesSeen' = insert fname filesSeen
           -- `case () of _` trick with guards is learnt from:
           -- https://wiki.haskell.org/Case
           -- https://stackoverflow.com/a/40836465
           case () of
             _
               | member fname filesSeen ->
                 put
                   ( filesSeen
                   , uPaths
                     {duplicatePaths = insert fname (duplicatePaths uPaths)})
               | isAbsolute fname ->
                 put
                   ( filesSeen'
                   , uPaths {absPaths = insert fname (absPaths uPaths)})
               | not $ isValid canonPath ->
                 put
                   ( filesSeen'
                   , uPaths {invalidPaths = insert fname (invalidPaths uPaths)})
               | takeDirectory canonPath /= cwd ->
                 put
                   ( filesSeen'
                   , uPaths
                     {filesNotInCwd = insert fname (filesNotInCwd uPaths)})
               | otherwise -> put (filesSeen', uPaths))
        files

printErrors :: FilePath -> UPaths -> IO ()
printErrors cwd uPaths
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
        (duplicatePaths uPaths)
        "Error: the following filenames are duplicated:"
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
