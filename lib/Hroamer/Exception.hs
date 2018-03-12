module Hroamer.Exception
  ( ignoreIOException
  ) where

import Control.Exception (IOException)
import Foundation

ignoreIOException :: IOException -> IO ()
ignoreIOException = const $ return ()
