module Hroamer.Utilities
  ( makeEditorCreateProcess
  ) where

import Foundation
import System.Environment (lookupEnv)
import System.FilePath.Posix (FilePath)
import System.Process (CreateProcess, proc, shell)

makeEditorCreateProcess :: FilePath -> IO CreateProcess
makeEditorCreateProcess file = do
  maybe_editor_env_var <- lookupEnv "EDITOR"
  case maybe_editor_env_var of
    Just ""
      -- Fallback to vim
     -> return launch_vim
    Just editor_env_var -> return $ shell $ editor_env_var <> " " <> file
    Nothing
      -- Fallback to vim
     -> return launch_vim
  where
    launch_vim = proc "vim" [file]
