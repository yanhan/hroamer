module Hroamer.StateFileSpec
  ( spec
  ) where

import Data.List (unlines)
import Data.Map.Strict (Map, empty, fromList, lookup)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import Foundation hiding (fromList)
import System.Directory (createDirectory)
import System.FilePath ((</>), FilePath)
import System.IO (readFile, writeFile)
import System.IO.Temp (createTempDirectory)
import System.Process (createProcess, proc, waitForProcess)
import Test.Hspec (Spec, afterAll, beforeAll, describe, it, parallel, shouldReturn)

import qualified Hroamer.StateFile as StateFile

createDirsForTest :: IO (FilePath, Map Text FilePath)
createDirsForTest = do
  cwd <- createTempDirectory "/tmp"  "StateFileSpec"
  appTmpDirOne <- createTempDirectory "/tmp"  "StateFile.create"
  appTmpDirTwo <- createTempDirectory "/tmp"  "StateFile.read"
  let m = fromList [ ("StateFile.create", appTmpDirOne)
                   , ("StateFile.read", appTmpDirTwo)
                   ]
  return (cwd, m)

rmrf :: (FilePath, Map Text FilePath) -> IO ()
rmrf (dir, m) = do
  let otherTempDirs = fmap (toList . snd) $ M.toList m
  let rmrfProc = proc "/bin/rm" (["-rf", toList dir] <> otherTempDirs)
  (_, _, _, ph) <- createProcess rmrfProc
  waitForProcess ph
  return ()

spec :: Spec
spec = parallel $ beforeAll createDirsForTest $ afterAll rmrf $ do
  describe "StateFile.create" $ do
    it "should create a correct state file" $ \(cwd, appTmpDirMap) -> do
      let dirToCreatePair = ("dinosaur", "10918d1c-284b-414a-aa1b-400074abace4")
      let fileToCreatePair = ("minions", "9bc243bf-a1b4-4029-af75-f4391fe34471")
      let dotXhrcPair = (".xhrc", "2e67d5b0-6f33-4207-98db-ab0a3a4a5190")
      -- Create the dir and file
      createDirectory $ cwd </> (fst dirToCreatePair)
      writeFile (cwd </> (fst fileToCreatePair)) ""
      -- helper functions
      let getFilename = toList . fst
      let getUuid = toList . snd
      let filesAndUuidInDir = [ dirToCreatePair
                              , fileToCreatePair
                              , dotXhrcPair
                              ]
      let appTmpDir = fromJust $ lookup "StateFile.create" appTmpDirMap
      stateFilePath <- StateFile.create cwd appTmpDir filesAndUuidInDir
      let contents = unlines $ [
                       "\" pwd: " <> (toList cwd)
                     , getFilename dotXhrcPair <> " | " <> getUuid dotXhrcPair
                     , getFilename dirToCreatePair <> "/" <> " | " <> getUuid dirToCreatePair
                     , getFilename fileToCreatePair <> " | " <> getUuid fileToCreatePair
                     ]
      fmap (<> "\n") (readFile stateFilePath) `shouldReturn` contents

  describe "StateFile.read" $ do
    it "should extract all that was written by the create function" $ \(cwd, appTmpDirMap) -> do
      let pairA = ("good-Night", "2436798f-2b60-4d97-87f5-f1e91e69e455")
      let pairB = (".main-thing", "445685f1-5faa-4bbe-8a04-c26dd4098738")
      let pairC = ("FoolsErrand", "2a04c542-3e06-474f-a7d6-4cee6ceaa583")
      let pairD = ("itchymonitor", "eccef357-a905-4dbb-bbaf-bf559b092965")
      -- Create pairB as a dir
      createDirectory $ cwd </> (fst pairB)
      let filesAndUuidInDir = [pairA, pairB, pairC, pairD]
      let appTmpDir = fromJust $ lookup "StateFile.read" appTmpDirMap
      stateFilePath <- StateFile.create cwd appTmpDir filesAndUuidInDir
      StateFile.read stateFilePath `shouldReturn` [pairB, pairC, pairA, pairD]
