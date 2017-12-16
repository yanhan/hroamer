module Hroamer.UnsupportedPaths.InternalSpec
  ( spec
  ) where

import Foundation
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

import Hroamer.UnsupportedPaths.Internal
       (formatPathsForErrorMessage)

spec :: Spec
spec = parallel $
  describe "formatPathsForErrorMessage" $ do
    it "should sort the FilePaths, convert them to Text and prepend them with a '- '" $
      let paths = ["number-theory-hw01.pdf", "a.txt", "main.c", "charges.txt"]
      in formatPathsForErrorMessage paths `shouldBe`
         ["- a.txt", "- charges.txt", "- main.c", "- number-theory-hw01.pdf"]
