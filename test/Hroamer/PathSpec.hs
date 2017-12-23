module Hroamer.PathSpec
  ( spec
  ) where

import Control.Monad (foldM)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.Writer.Strict (runWriterT)
import Data.Char (chr, isSpace)
import qualified Data.DList
import Foundation
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>), FilePath, pathSeparator, takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
       (Spec, describe, it, parallel, shouldBe, shouldNotBe, shouldReturn)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
       (Gen, Property, arbitrary, choose, forAll, listOf1, property, suchThat)

import Hroamer.Path
       (appendSlashToDir, createDirNoForce, hasSpace, isWeakAncestorDir)

genCharNoNulls :: Gen Char
genCharNoNulls = choose (chr 1, maxBound :: Char)

isNotPathSeparator :: Char -> Bool
isNotPathSeparator c = c /= pathSeparator

genFilePathComponent :: Gen [Char]
genFilePathComponent = listOf1 $ suchThat genCharNoNulls isNotPathSeparator


genRelativeFilePath :: Gen FilePath
genRelativeFilePath = do
  l <- listOf1 genFilePathComponent
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
absoluteFilePath = (<>) <$> genPathSeparators <*> genRelativeFilePath


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
  forAll genRelativeFilePath (\fp -> runReader (checkAllAncestorPaths fp) fp)


absoluteFilePathProp :: Property
absoluteFilePathProp =
  forAll absoluteFilePath (\fp -> runReader (checkAllAncestorPaths fp) fp)


absAndRelFilePath :: Gen (FilePath, FilePath)
absAndRelFilePath = fmap (,) absoluteFilePath <*> genRelativeFilePath


absoluteAndRelativeMix :: Property
absoluteAndRelativeMix =
  forAll
    absAndRelFilePath
    (\(absPath, relPath) -> not $ isWeakAncestorDir absPath relPath)


relativeAndAbsoluteMix :: Property
relativeAndAbsoluteMix =
  forAll
    absAndRelFilePath
    (\(absPath, relPath) -> not $ isWeakAncestorDir relPath absPath)


genAbsoluteFilePathWithSpace :: Gen [Char]
genAbsoluteFilePathWithSpace = do
  -- shuffle this?
  l1 <- listOf1 genFilePathComponent
  l2 <- listOf1 filePathComponentWithSpace
  foldM (\pathSoFar pathComponent -> do
    pathSep <- genPathSeparators
    return $ pathSoFar <> pathSep <> pathComponent
    ) "" (l1 <> l2)
  where
    filePathComponentWithSpace :: Gen [Char]
    filePathComponentWithSpace = do
      p1 <- listOf1 $ suchThat genCharNoNulls isNotPathSeparator
      p2 <- listOf1 $ suchThat genCharNoNulls isSpace
      p3 <- listOf1 $ suchThat genCharNoNulls isNotPathSeparator
      return $ p1 <> p2 <> p3


hasSpaceDetectsFilePathWithSpace :: Property
hasSpaceDetectsFilePathWithSpace = forAll genAbsoluteFilePathWithSpace hasSpace

spec :: Spec
spec = parallel $ do
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

    it "should strip off ending slash in the initial arguments before doing comparison" $
      isWeakAncestorDir "/var/lib/apt/cache/"  "/var/lib/apt/cache" `shouldBe` True

    it "should return False when a path is not a descendent of a suspected ancestor path" $
      isWeakAncestorDir "/home/mike/blurb"  "/dev/sda1" `shouldBe` False

    it "should terminate and return False when suspected ancestor is an absolute path and path of interest is a relative path" $
      isWeakAncestorDir "/dev/sda1" "images/png/summer-holidays.png" `shouldBe` False

    it "should terminate and return False when suspected ancestor is a relative path and path of interest is a relative path not under it" $
      isWeakAncestorDir "who/let/the/dogs/out"  "jim/did" `shouldBe` False

    it "should drop off leading slashes in absolute paths" $
      isWeakAncestorDir "///usr/bin"  "//usr/bin/gcc" `shouldBe` True

    modifyMaxSuccess (const 30) $ it "QuickCheck relative filepath tests" $
      relativeFilePathProp

    modifyMaxSuccess (const 30) $ it "QuickCheck absolute filepath tests" $
      absoluteFilePathProp

    it "should return False when ancestor dir is absolute path and path of interest is relative path [QuickCheck]" $
      absoluteAndRelativeMix

    it "should return False when ancestor dir is relative path and path of interest is absolute path [QuickCheck]" $
      relativeAndAbsoluteMix

  describe "appendSlashToDir" $ do
    it "will append a slash to an existing dir if the slash is missing" $
      appendSlashToDir "/"  "etc" `shouldReturn` "etc/"

    it "will not append a slash to an existing dir if the slash is present" $
      appendSlashToDir "/"  "etc/" `shouldReturn` "etc/"

    it "will not append a slash to a file if the slash is missing" $
      appendSlashToDir "/etc"  "group" `shouldReturn` "group"

    it "will not append a slash to a non-existent file / dir" $
      appendSlashToDir "/what/a/stupid/idea/man"  "yea" `shouldReturn` "yea"

  describe "createDirNoForce" $ do
    it "will return (WriterT DList.empty IO True) for an existing dir" $ do
      (dirExists, emptyDList) <- runWriterT $ createDirNoForce "/usr/bin"
      dirExists `shouldBe` True
      emptyDList `shouldBe` Data.DList.empty

    it "will return (WriterT (non-empty DList) IO False) for an existing file" $ do
      (dirExists, errorDList) <- runWriterT $ createDirNoForce "/bin/ls"
      dirExists `shouldBe` False
      errorDList `shouldNotBe` Data.DList.empty

    it "will return (WriterT DList.empty IO True) and create a directory for a non existent path" $
      withSystemTempDirectory "pathSpecCreateDirNoForce" (\tempDir -> do
        let nonExistentDir = tempDir </> "sandwich"
        (dirExists, emptyDList) <- runWriterT $ createDirNoForce nonExistentDir
        dirExists `shouldBe` True
        emptyDList `shouldBe` Data.DList.empty
        doesDirectoryExist nonExistentDir `shouldReturn` True
      )

  describe "hasSpace" $ do
    modifyMaxSuccess (const 30) $
      it "will return True for FilePath that has at least a space" $
        hasSpaceDetectsFilePathWithSpace
