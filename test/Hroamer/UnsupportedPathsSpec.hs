module Hroamer.UnsupportedPathsSpec
  ( spec
  ) where

import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.Set as S
import Data.Set (empty)
import Data.Text (pack)
import Foundation
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Hroamer.UnsupportedPaths
       (getErrors, getUnsupportedPaths, noUnsupportedPaths)
import Hroamer.UnsupportedPaths.Internal
       (UPaths(UPaths), absolutePathsErrorTitle, duplicatePathsErrorTitle,
        invalidPathsErrorTitle, relativePathsErrorTitle)

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

  describe "getErrors" $ do
    let txPaths = List.sort . fmap (("- " <>) . pack)

    it "when there are multiple categories of errors, it will construct a message that separates each category by an empty line and sort the filenames in each category of error" $ do
      let duplicatePaths = ["main.c", "jobs.txt"]
      let absolutePaths = ["/home/thomas/search"]
      let relativePaths = ["../flight-details.png"]
      let invalidPaths = ["saturd\0y"]
      let upaths =
            UPaths
              (S.fromList duplicatePaths)
              (S.fromList absolutePaths)
              (S.fromList relativePaths)
              (S.fromList invalidPaths)
      let cwd = "/home/thomas"
      DList.toList (getErrors cwd upaths) `shouldBe`
        [duplicatePathsErrorTitle] <>
        txPaths duplicatePaths <>
        ["", absolutePathsErrorTitle] <>
        txPaths absolutePaths <>
        ["", relativePathsErrorTitle $ pack cwd] <>
        txPaths relativePaths <>
        ["", invalidPathsErrorTitle] <>
        txPaths invalidPaths

    it "will not construct a message with empty lines if there is only one category of error" $ do
      let relativePaths =
            ["../homework01.txt", "../records.sql", "../cute-cats.png"]
      let upaths = UPaths empty empty (S.fromList relativePaths) empty
      let cwd = "/home/jake/docs"
      DList.toList (getErrors cwd upaths) `shouldBe`
        [relativePathsErrorTitle $ pack cwd] <>
        txPaths relativePaths

    it "will return an empty DList if there are no errors" $
      getErrors "/opt/local/x" (UPaths empty empty empty empty) `shouldBe`
        DList.empty
