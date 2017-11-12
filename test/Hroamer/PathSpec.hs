module Hroamer.PathSpec
  ( spec
  ) where

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Char (chr)
import Foundation
import System.FilePath ((</>), FilePath, pathSeparator, takeDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
       (Gen, Property, arbitrary, choose, forAll, listOf1, property, suchThat)

import Hroamer.Path (isWeakAncestorDir)


filePathComponent :: Gen [Char]
filePathComponent = listOf1 $ suchThat noNullCharGen noPathSeparator
  where
    noNullCharGen :: Gen Char
    noNullCharGen = choose (chr 1, maxBound :: Char)
    noPathSeparator :: Char -> Bool
    noPathSeparator c = c /= pathSeparator


relativeFilePath :: Gen FilePath
relativeFilePath = do
  l <- listOf1 filePathComponent
  let nrPathSeparatorBlocks = length l - 1
  case l of
    [] -> return []
    (x:xs) -> fmap ((x <>) . mconcat) $ prependWithPathSep xs
  where
    prependWithPathSep :: [FilePath] -> Gen [FilePath]
    prependWithPathSep (x:xs) = do
      xs' <- prependWithPathSep xs
      pathSep <- genPathSeparators
      return $ pathSep : x : xs'
    prependWithPathSep [] = return []


genPathSeparators :: Gen FilePath
genPathSeparators = do
  n <- choose (1, 5) :: Gen Int
  return $ replicate (CountOf n) pathSeparator


absoluteFilePath :: Gen FilePath
absoluteFilePath = (<>) <$> genPathSeparators <*> relativeFilePath


checkAllAncestorPaths :: FilePath -> Reader FilePath Bool
checkAllAncestorPaths possibleAncestorFilePath = do
  orgFilePath <- ask
  let ok = isWeakAncestorDir possibleAncestorFilePath orgFilePath
  let nextAncestorFilePath = takeDirectory possibleAncestorFilePath
  if ok
    then if nextAncestorFilePath == possibleAncestorFilePath
           then return True
           else checkAllAncestorPaths nextAncestorFilePath
    else return False


relativeFilePathProp :: Property
relativeFilePathProp =
  forAll relativeFilePath (\fp -> runReader (checkAllAncestorPaths fp) fp)


absoluteFilePathProp :: Property
absoluteFilePathProp =
  forAll absoluteFilePath (\fp -> runReader (checkAllAncestorPaths fp) fp)


spec :: Spec
spec = do
  describe "isWeakAncestorDir" $ do
    it "should return True for a file under its ancestor dir (for absolute paths)" $ do
      let homeDir = "/home/jack"
      let fileInHomeDir = homeDir </> "security" </> "buffer-overflows.txt"
      isWeakAncestorDir homeDir fileInHomeDir `shouldBe` True

    it "should return True for a file under its ancestor dir (for relative paths)" $ do
      let myDir = "cookbook/sauces"
      let myFile = myDir </> "red" </> "tomato.txt"
      isWeakAncestorDir myDir myFile `shouldBe` True

    it "should return True when comparing the same path (both ending with slash)" $ do
      let myFile = "/home/paul/diary/"
      isWeakAncestorDir myFile myFile `shouldBe` True

    it "should return True when comparing the same path (both not ending with slash)" $ do
      let myFile = "/home/paul/diary/entry01.txt"
      isWeakAncestorDir myFile myFile `shouldBe` True

    it "should strip off ending slash in the initial arguments before doing comparison" $ do
      isWeakAncestorDir "/var/lib/apt/cache/"  "/var/lib/apt/cache" `shouldBe` True

    it "should return False when a path is not a descendent of a suspected ancestor path" $ do
      isWeakAncestorDir "/home/mike/blurb"  "/dev/sda1" `shouldBe` False

    it "should terminate and return False when suspected ancestor is an absolute path and path of interest is a relative path" $ do
      isWeakAncestorDir "/dev/sda1" "images/png/summer-holidays.png" `shouldBe` False

    it "should terminate and return False when suspected ancestor is a relative path and path of interest is a relative path not under it" $ do
      isWeakAncestorDir "who/let/the/dogs/out"  "jim/did" `shouldBe` False

    it "should drop off leading slashes in absolute paths" $ do
      isWeakAncestorDir "///usr/bin"  "//usr/bin/gcc" `shouldBe` True

    it "QuickCheck relative filepath tests" $ relativeFilePathProp

    it "QuickCheck absolute filepath tests" $ absoluteFilePathProp
