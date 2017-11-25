module Hroamer.StateFileSpec
  ( spec
  ) where

import Data.List (unlines)
import Foundation
import System.Directory (createDirectory)
import System.FilePath ((</>), FilePath)
import System.IO (readFile)
import System.IO.Temp (createTempDirectory)
import System.Process (createProcess, proc, waitForProcess)
import Test.Hspec (Spec, afterAll, beforeAll, describe, it, parallel, shouldReturn)

import qualified Hroamer.StateFile as StateFile

createDirsForTest :: IO (FilePath, FilePath)
createDirsForTest = do
  cwd <- createTempDirectory "/tmp"  "StateFileSpec"
  appTmpDir <- createTempDirectory "/tmp"  "StateFileSpecApp"
  return (cwd, appTmpDir)

rmrf :: (FilePath, FilePath) -> IO ()
rmrf (fileOne, fileTwo) = do
  let rmrfProc = proc "/bin/rm" ["-rf", toList fileOne, toList fileTwo]
  (_, _, _, ph) <- createProcess rmrfProc
  waitForProcess ph
  return ()

spec :: Spec
spec = parallel $ do
  beforeAll createDirsForTest $ afterAll rmrf $ describe "StateFile.create" $ do
    it "should create a correct state file" $ \(cwd, appTmpDir) -> do
      let dirToCreatePair = ("dinosaur", "10918d1c-284b-414a-aa1b-400074abace4")
      let dotXhrcPair = (".xhrc", "2e67d5b0-6f33-4207-98db-ab0a3a4a5190")
      -- Need to create a directory
      createDirectory $ cwd </> (fst dirToCreatePair)
      let filesAndUuidInDir = [ dirToCreatePair
                              , dotXhrcPair
                              ]
      stateFilePath <- StateFile.create cwd appTmpDir filesAndUuidInDir
      let contents = unlines $ [
                       "\" pwd: " <> (toList cwd)
                     , (toList $ fst dotXhrcPair) <> " | " <> (toList $ snd dotXhrcPair)
                     , (toList $ fst dirToCreatePair) <> "/" <> " | " <> (toList $ snd dirToCreatePair)
                     ]
      fmap (<> "\n") (readFile stateFilePath) `shouldReturn` contents
