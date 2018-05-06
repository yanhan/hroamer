module Hroamer.PathSpec
  ( spec
  ) where

import Control.Monad (foldM)
import Control.Monad.Reader (Reader, ask, runReader, runReaderT)
import Control.Monad.Writer.Strict (runWriterT)
import Data.Char (isSpace)
import qualified Data.DList
import Data.Map.Strict (Map, lookup)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import Foundation
import System.Directory
       (createDirectory, doesDirectoryExist, getCurrentDirectory,
        pathIsSymbolicLink)
import System.FilePath ((</>), FilePath, pathSeparator, takeDirectory)
import System.IO (writeFile)
import System.IO.Temp (createTempDirectory, withSystemTempDirectory)
import System.Process (createProcess, proc, waitForProcess)
import Test.Hspec
       (Spec, afterAll, beforeAll, describe, it, parallel, shouldBe,
        shouldNotBe, shouldReturn)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
       (Gen, Property, arbitrary, choose, forAll, listOf1, property,
        shuffle, suchThat)

import Hroamer.DataStructures (AbsFilePath(AbsFilePath))
import Hroamer.Path
       (appendSlashToDir, createDirNoForce, hasSpace, isWeakAncestorDir,
        resolvePath)
import TestHelpers
       (genCharNotNull, genFilePathComponent, genSpace,
        genValidFilePathChar, isNotPathSeparator, rmrf)

resolvePathSpecResolveAbsPathSymlinkKey :: Text
resolvePathSpecResolveAbsPathSymlinkKey = "resolvePathSpecResolveAbsPathSymlink"

resolvePathSpecResolveSymlinkInCwdKey :: Text
resolvePathSpecResolveSymlinkInCwdKey = "resolvePathSpecResolveSymlinkInCwd"

resolvePathSpecFileInCwdKey :: Text
resolvePathSpecFileInCwdKey = "resolvePathSpecFileInCwd"

createTempDirs :: IO (Map Text FilePath)
createTempDirs = do
  resolvePathSpecResolveAbsPathSymlinkDir <-
    createTempDirectory "/tmp"  "resolvePathSpecResolveAbsPathSymlink"
  resolvePathSpecResolveSymlinkInCwdDir <-
    createTempDirectory "/tmp" "resolvePathSpecResolveSymlinkInCwd"
  resolvePathSpecFileInCwdDir <-
    createTempDirectory "/tmp" "resolvePathSpecFileInCwd"
  return $ M.fromList [ ( resolvePathSpecResolveAbsPathSymlinkKey
                        , resolvePathSpecResolveAbsPathSymlinkDir
                        )
                      , ( resolvePathSpecResolveSymlinkInCwdKey
                        , resolvePathSpecResolveSymlinkInCwdDir
                        )
                      , ( resolvePathSpecFileInCwdKey
                        , resolvePathSpecFileInCwdDir
                        )
                      ]

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


genAbsoluteFilePath :: Gen FilePath
genAbsoluteFilePath = (<>) <$> genPathSeparators <*> genRelativeFilePath


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


absAndRelFilePath :: Gen (FilePath, FilePath)
absAndRelFilePath = fmap (,) genAbsoluteFilePath <*> genRelativeFilePath


genAbsoluteFilePathWithSpace :: Gen FilePath
genAbsoluteFilePathWithSpace = do
  l1 <- listOf1 genFilePathComponent
  l2 <- listOf1 genFilePathComponentWithSpace
  filePathComponents <- shuffle $ l1 <> l2
  foldM (\pathSoFar pathComponent -> do
    pathSep <- genPathSeparators
    return $ pathSoFar <> pathSep <> pathComponent
    ) "" filePathComponents
  where
    genFilePathComponentWithSpace :: Gen FilePath
    genFilePathComponentWithSpace = do
      p1 <- listOf1 genValidFilePathChar
      p2 <- listOf1 genSpace
      p3 <- listOf1 genValidFilePathChar
      return $ p1 <> p2 <> p3

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
      forAll genRelativeFilePath (\fp -> runReader (checkAllAncestorPaths fp) fp)

    modifyMaxSuccess (const 30) $ it "QuickCheck absolute filepath tests" $
      forAll genAbsoluteFilePath (\fp -> runReader (checkAllAncestorPaths fp) fp)

    it "should return False when ancestor dir is absolute path and path of interest is relative path [QuickCheck]" $
      forAll
        absAndRelFilePath
        (\(absPath, relPath) -> not $ isWeakAncestorDir absPath relPath)

    it "should return False when ancestor dir is relative path and path of interest is absolute path [QuickCheck]" $
      forAll
        absAndRelFilePath
        (\(absPath, relPath) -> not $ isWeakAncestorDir relPath absPath)

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
        forAll genAbsoluteFilePathWithSpace hasSpace

  describe "resolvePath" $ beforeAll createTempDirs $ afterAll rmrf $ do
    it "will not modify an absolute path which is not a symlink" $ \_ ->
      let filePath = "/usr/bin/yes"
      in resolvePath undefined filePath `shouldReturn` AbsFilePath filePath

    it "will resolve an absolute path which is a symlink" $ \mapOfTempDirs -> do
      let tempDir = fromJust $
            lookup resolvePathSpecResolveAbsPathSymlinkKey mapOfTempDirs
          cwd = tempDir </> "quantify"
          symlink = cwd </> "stars"
          actualFile = cwd </> "mrbeee"
      createDirectory cwd
      writeFile actualFile "coming soon"
      (_, _, _, ph) <- createProcess (proc "ln" ["-s", actualFile, symlink])
      waitForProcess ph
      pathIsSymbolicLink symlink `shouldReturn` True
      resolvePath undefined symlink `shouldReturn` AbsFilePath actualFile

    it "will resolve a relative path which exists" $ \_ -> do
      cwd <- getCurrentDirectory
      let getLevelsToDirs path n =
            if path == "/"
               then n
               else getLevelsToDirs (takeDirectory path) (n + 1)
          levels = 1 + getLevelsToDirs cwd 0
          filePath = (mconcat $ replicate levels "../") </> "/bin/rm"
      resolvePath undefined filePath `shouldReturn` AbsFilePath "/bin/rm"

    it "will only prepend the current directory to a relative path which does not exist" $ \_ -> do
      cwd <- getCurrentDirectory
      let filePath = "I/hope/that/this/does/not/exist/on/your/computer3Z"
      resolvePath undefined filePath `shouldReturn`
        (AbsFilePath $ cwd </> filePath)

    it "will not resolve a symlink in the current directory" $ \mapOfTempDirs -> do
      let tempDir = fromJust $
            lookup resolvePathSpecResolveSymlinkInCwdKey mapOfTempDirs
          cwd = tempDir </> "lid"
          symlinkName = "bittree"
          symlinkPath = cwd </> symlinkName
          actualFile = cwd </> "eclipse"
      createDirectory cwd
      writeFile actualFile "The moon covers the sun"
      (_, _, _, ph) <- createProcess (proc "ln" ["-s", actualFile, symlinkPath])
      waitForProcess ph
      pathIsSymbolicLink symlinkPath `shouldReturn` True
      resolvePath cwd symlinkName `shouldReturn` AbsFilePath symlinkPath

    it "will resolve a path in the current directory by returning the same path" $
      \mapOfTempDirs -> do
        let tempDir = fromJust $
              lookup resolvePathSpecFileInCwdKey mapOfTempDirs
            cwd = tempDir </> "spinner"
            filePath = cwd </> "uno"
        createDirectory cwd
        writeFile filePath "numero"
        resolvePath cwd filePath `shouldReturn` AbsFilePath filePath
