module Hroamer.UnsupportedPathsSpec
  ( spec
  ) where

import qualified Data.Set as S
import Data.Set (empty)
import Foundation
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Hroamer.UnsupportedPaths
       (getUnsupportedPaths, noUnsupportedPaths)
import Hroamer.UnsupportedPaths.Internal (UPaths(UPaths))

spec :: Spec
spec = do
  describe "noUnsupportedPaths" $ do
    it "will return False if there are absolute paths" $ do
      let absPaths = S.fromList ["/bin/echo"]
      noUnsupportedPaths (UPaths empty absPaths empty empty) `shouldBe` False

    it "will return False if there are relative paths" $ do
      let relativePaths = S.fromList ["../cleaning-list.txt"]
      noUnsupportedPaths (UPaths empty empty relativePaths empty) `shouldBe` False

    it "will return False if there are invalid paths" $ do
      let invalidPaths = S.fromList ["afz"]
      noUnsupportedPaths (UPaths empty empty empty invalidPaths) `shouldBe` False

    it "will return True if there are duplicate paths" $ do
      let duplicatePaths = S.fromList ["dupfile"]
      noUnsupportedPaths (UPaths duplicatePaths empty empty empty) `shouldBe` True

  describe "getUnsupportedPaths" $ do
    it "will construct a UPaths data structure with duplicate, absolute, relative and invalid paths" $ do
      let paths =
            [ "../chop"
            , "/usr/bin/head"
            , "normal-file-in-dir"
            , "../../fireball"
            , "funny.mp4"
            , "steam.exe"
            , "funny.mp4"
            , "flying-meatballs.png"
            , "../../fireball"
            , "we\0k"
            , "we1k"
            ]
      let duplicatePaths = S.fromList ["funny.mp4", "../../fireball"]
      let relativePaths = S.fromList ["../chop", "../../fireball"]
      let absolutePaths = S.fromList ["/usr/bin/head"]
      let invalidPaths = S.fromList ["we\0k"]
      let expectedUPaths = UPaths duplicatePaths absolutePaths relativePaths invalidPaths
      getUnsupportedPaths "/bin" paths `shouldReturn` expectedUPaths
