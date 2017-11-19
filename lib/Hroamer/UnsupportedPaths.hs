module Hroamer.UnsupportedPaths
  ( getErrors
  , getUnsupportedPaths
  , noUnsupportedPaths
  ) where

import Control.Monad (mapM_)
import Control.Monad.State
       (State, StateT, evalState, evalStateT, execStateT, get, lift,
        modify, put)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import qualified Data.DList as DList
import Data.DList (DList, singleton)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as S
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Foundation hiding (singleton)
import System.Directory (canonicalizePath)
import System.FilePath.Posix
       ((</>), FilePath, isAbsolute, isValid, takeDirectory)

import Hroamer.UnsupportedPaths.Internal
       (UPaths(UPaths, duplicatePaths, absPaths, filesNotInCwd,
               invalidPaths),
        absolutePathsErrorTitle, duplicatePathsErrorTitle,
        formatPathsForErrorMessage, invalidPathsErrorTitle,
        relativePathsErrorTitle)

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

getErrors :: FilePath -> UPaths -> DList Text
getErrors cwd uPaths
 -- the state is the number of errors so far and we use it to determine
 -- whether we need to print a separating newline between different error
 -- messages
 = snd $ evalState (runWriterT getErrorsInternal) 0
  where
    getOneError :: Set FilePath -> Text -> WriterT (DList Text) (State Int) ()
    getOneError files msg = do
      if not $ S.null files
        then do
          nrErrors <- lift get
          if nrErrors > 0
            then tell $ singleton ""
            else return ()
          lift $ modify (+ 1)
          tell $ singleton msg
          tell . DList.fromList $
            (formatPathsForErrorMessage $ S.toList files)
        else return ()
    getErrorsInternal :: WriterT (DList Text) (State Int) ()
    getErrorsInternal = do
      getOneError
        (duplicatePaths uPaths) duplicatePathsErrorTitle
      getOneError
        (absPaths uPaths) absolutePathsErrorTitle
      getOneError
        (filesNotInCwd uPaths) (relativePathsErrorTitle $ pack cwd)
      getOneError
        (invalidPaths uPaths) invalidPathsErrorTitle
