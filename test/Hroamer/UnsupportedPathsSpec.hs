module Hroamer.UnsupportedPathsSpec
  ( spec
  ) where

import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.Set as S
import Data.Map.Strict (Map, fromList, lookup)
import Data.Maybe (fromJust)
import Data.Set (empty)
import Data.Text (Text, pack)
import Foundation hiding (fromList)
import System.Directory (createDirectory, doesPathExist)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath.Posix ((</>), FilePath)
import System.IO.Temp (createTempDirectory)
import System.Process (createProcess, proc, waitForProcess)
import Test.Hspec
       (Spec, afterAll, beforeAll, describe, it, parallel, shouldBe,
        shouldReturn)

import Hroamer.DataStructures (AbsFilePath(AbsFilePath))
import Hroamer.UnsupportedPaths (getErrors, getUnsupportedPaths)
import Hroamer.UnsupportedPaths.Internal
       (UPaths(UPaths), duplicatePathsErrorTitle,
        formatPathsForErrorMessage, invalidPathsErrorTitle)
import TestHelpers (rmrf)

dontCanonicalizeSymlinksKey :: Text
dontCanonicalizeSymlinksKey = "dontCanonicalizeSymlinksKey"

createTempDirs :: IO (Map Text FilePath)
createTempDirs = do
  dontCanonicalizeSymlinksDir <-
    createTempDirectory "/tmp"  "dontCanonicalizeSymlinksDir"
  return $
    fromList [(dontCanonicalizeSymlinksKey, dontCanonicalizeSymlinksDir)]

createSymlink :: [Char] -> [Char] -> IO ()
createSymlink target linkName = do
  (_, _, _, ph) <- createProcess $
    proc "ln" ["-s", toList target, toList linkName]
  waitForProcess ph
  return ()

spec :: Spec
spec = parallel $ beforeAll createTempDirs $ afterAll rmrf $ do
  describe "getUnsupportedPaths" $ do
    it "will construct a UPaths data structure with duplicate and invalid paths" $ \_ -> do
      let cwd = "/medium/large/small"
          anotherDir = "/snoring/loudly"
          paths =
            [ AbsFilePath "/medium/large/chop"
            , AbsFilePath "/usr/bin/head"
            , AbsFilePath $ cwd </> "normal-file-in-dir"
            , AbsFilePath "/medium/fireball"
            , AbsFilePath $ cwd </> "funny.mp4"
            , AbsFilePath $ cwd </> "steam.exe"
            , AbsFilePath $ cwd </> "funny.mp4"
            , AbsFilePath $ cwd </> "flying-meatballs.png"
            , AbsFilePath "/medium/fireball"
            , AbsFilePath $ cwd </> "we\0k"
            , AbsFilePath $ cwd </> "we1k"
            , AbsFilePath $ anotherDir </> "hashing"
            , AbsFilePath $ anotherDir </> "cupcakes"
            , AbsFilePath $ anotherDir </> "cupcakes"
            ]
      let duplicatePaths = S.fromList [ cwd </> "funny.mp4"
                                      , "/medium/fireball"
                                      , anotherDir </> "cupcakes"
                                      ]
      let invalidPaths = S.fromList [cwd </> "we\0k"]
      let expectedUPaths = UPaths duplicatePaths invalidPaths
      getUnsupportedPaths paths `shouldReturn` expectedUPaths

  describe "getErrors" $ do
    it "when there are multiple categories of errors, it will construct a message that separates each category by an empty line and sort the filenames in each category of error" $ \_ -> do
      let duplicatePaths = ["main.c", "jobs.txt"]
      let invalidPaths = ["saturd\0y"]
      let upaths =
            UPaths
              (S.fromList duplicatePaths)
              (S.fromList invalidPaths)
      let cwd = "/home/thomas"
      DList.toList (getErrors cwd upaths) `shouldBe`
        [duplicatePathsErrorTitle] <>
        formatPathsForErrorMessage duplicatePaths <>
        ["", invalidPathsErrorTitle] <>
        formatPathsForErrorMessage invalidPaths

    it "will not construct a message with empty lines if there is only one category of error" $ \_ -> do
      let duplicatePaths = ["homework01.txt", "records.sql", "../cute-cats.png"]
      let upaths = UPaths (S.fromList duplicatePaths) empty
      let cwd = "/home/jake/docs"
      DList.toList (getErrors cwd upaths) `shouldBe`
        [duplicatePathsErrorTitle] <>
        formatPathsForErrorMessage duplicatePaths

    it "will return an empty DList if there are no errors" $ \_ ->
      getErrors "/opt/local/x" (UPaths empty empty) `shouldBe`
        DList.empty
