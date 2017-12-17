module Hroamer.Utilities
  ( makeEditorCreateProcess
  ) where

import Foundation
import System.Environment (lookupEnv)
import System.FilePath.Posix (FilePath)
import System.Process (CreateProcess, proc, shell)

makeEditorCreateProcess :: FilePath -> IO CreateProcess
makeEditorCreateProcess file = do
  maybeEditorEnvVar <- lookupEnv "EDITOR"
  case maybeEditorEnvVar of
    Just ""
      -- Fallback to vim
     -> return launchVim
    Just editorEnvVar -> return $ shell $ editorEnvVar <> " " <> file
    Nothing
      -- Fallback to vim
     -> return launchVim
  where
    launchVim = proc "vim" [file]
