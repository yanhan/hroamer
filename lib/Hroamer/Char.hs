module Hroamer.Char
  ( isNotSpace
  ) where

import Data.Char (isSpace)
import Foundation

isNotSpace :: Char -> Bool
isNotSpace = not . isSpace
