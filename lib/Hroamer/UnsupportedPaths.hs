module Hroamer.UnsupportedPaths
  ( getErrors
  , getUnsupportedPaths
  ) where

import Control.Exception (catch)
import Control.Monad (mapM_)
import Control.Monad.IO.Class (liftIO)
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
import System.Directory (canonicalizePath, pathIsSymbolicLink)
import System.FilePath.Posix
       ((</>), FilePath, isValid, takeDirectory)

import Hroamer.DataStructures (AbsFilePath(toFilePath))
import Hroamer.Path (isWeakAncestorDir)
import Hroamer.UnsupportedPaths.Internal
       (UPaths(UPaths, ancestorPaths, duplicatePaths, invalidPaths),
        ancestorPathsErrorTitle, duplicatePathsErrorTitle,
        formatPathsForErrorMessage, invalidPathsErrorTitle)

excHandlerReturnFalse :: IOException -> IO Bool
excHandlerReturnFalse = const $ return False

getUnsupportedPaths :: FilePath -> [AbsFilePath] -> IO UPaths
getUnsupportedPaths cwd absFilePaths = do
  (_, uPaths) <- liftIO $
    execStateT sta (empty, (UPaths empty empty empty))
  return uPaths
  where
    sta :: StateT (Set FilePath, UPaths) IO ()
    sta = do
      mapM_
        (\absFilePath -> do
           (filesSeen, uPaths) <- get
           let filePath = toFilePath absFilePath
               filesSeen' = insert filePath filesSeen
           -- `case () of _` trick with guards is learnt from:
           -- https://wiki.haskell.org/Case
           -- https://stackoverflow.com/a/40836465
           case () of
             _
               | isWeakAncestorDir filePath cwd ->
                   put ( filesSeen'
                       , uPaths {
                           ancestorPaths = insert filePath (ancestorPaths uPaths)
                         }
                       )
               | member filePath filesSeen ->
                 put
                   ( filesSeen
                   , uPaths
                     {duplicatePaths = insert filePath (duplicatePaths uPaths)})
               | not $ isValid filePath ->
                 put
                   ( filesSeen'
                   , uPaths {
                       invalidPaths = insert filePath (invalidPaths uPaths)})
               | otherwise -> put (filesSeen', uPaths))
        absFilePaths

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
        (ancestorPaths uPaths) ancestorPathsErrorTitle
      getOneError
        (duplicatePaths uPaths) duplicatePathsErrorTitle
      getOneError
        (invalidPaths uPaths) invalidPathsErrorTitle
