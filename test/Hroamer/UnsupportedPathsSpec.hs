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

import Hroamer.UnsupportedPaths (getErrors, getUnsupportedPaths)
import Hroamer.UnsupportedPaths.Internal
       (UPaths(UPaths), absolutePathsErrorTitle, duplicatePathsErrorTitle,
        formatPathsForErrorMessage, invalidPathsErrorTitle,
        relativePathsErrorTitle)
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
    it "will construct a UPaths data structure with duplicate, absolute, relative and invalid paths" $ \_ -> do
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

    it "will not canonicalize the file path of symlinks" $ \mapOfTempDirs -> do
      let cwd = fromJust $ lookup dontCanonicalizeSymlinksKey mapOfTempDirs
          symlinkOneFilename = "s3"
          symlinkOnePath = cwd </> symlinkOneFilename
          symlinkTwoFilename = "hoisting"
          symlinkTwoPath = cwd </> symlinkTwoFilename
          paths = [ "normal.hs"
                  , symlinkOneFilename
                  , symlinkTwoFilename
                  ]
      createSymlink "/wtf/is/this" (toList symlinkOnePath)
      createSymlink "../../woo" (toList symlinkTwoPath)
      doesPathExist symlinkOnePath
      doesPathExist symlinkTwoPath
      True `shouldBe` True

  describe "getErrors" $ do
    it "when there are multiple categories of errors, it will construct a message that separates each category by an empty line and sort the filenames in each category of error" $ \_ -> do
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
        formatPathsForErrorMessage duplicatePaths <>
        ["", absolutePathsErrorTitle] <>
        formatPathsForErrorMessage absolutePaths <>
        ["", relativePathsErrorTitle $ pack cwd] <>
        formatPathsForErrorMessage relativePaths <>
        ["", invalidPathsErrorTitle] <>
        formatPathsForErrorMessage invalidPaths

    it "will not construct a message with empty lines if there is only one category of error" $ \_ -> do
      let relativePaths =
            ["../homework01.txt", "../records.sql", "../cute-cats.png"]
      let upaths = UPaths empty empty (S.fromList relativePaths) empty
      let cwd = "/home/jake/docs"
      DList.toList (getErrors cwd upaths) `shouldBe`
        [relativePathsErrorTitle $ pack cwd] <>
        formatPathsForErrorMessage relativePaths

    it "will return an empty DList if there are no errors" $ \_ ->
      getErrors "/opt/local/x" (UPaths empty empty empty empty) `shouldBe`
        DList.empty
