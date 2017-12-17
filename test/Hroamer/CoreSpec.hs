module Hroamer.CoreSpec
  ( spec
  ) where

import Control.Monad (mapM_)
--import Data.List (sortBy)
import Data.Map.Strict (Map, fromList, lookup, size)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import Foundation hiding (fromList)
import System.Directory (createDirectory)
import System.FilePath.Posix ((</>), FilePath)
import System.IO (writeFile)
import System.IO.Temp (createTempDirectory)
import Test.Hspec
       (Spec, afterAll, beforeAll, describe, it, parallel, shouldBe,
        shouldReturn, shouldSatisfy)

import Hroamer.Core (processCwd)
import Hroamer.Database.Internal (addFileDetailsToDb)
import TestHelpers (rmrf)

import qualified Hroamer.Database as HroamerDb

processCwdSpecKey :: Text
processCwdSpecKey = "processCwdSpecDir"

createDirs :: IO (Map Text FilePath)
createDirs = do
  processCwdSpecDir <- createTempDirectory "/tmp"  "processCwdSpecDir"
  return $ fromList [(processCwdSpecKey, processCwdSpecDir)]

spec :: Spec
spec = parallel $ beforeAll createDirs $ afterAll rmrf $ do
  describe "processCwd" $
    it "should update the database with the current state of cwd and return the [FilePathUUIDPair]" $
      \mapOfTempDirs -> do
        let tempDir = fromJust $ lookup processCwdSpecKey mapOfTempDirs
            pathToDb = tempDir </> "db"
            appTmpDir = tempDir </> "hroamer"
            cwd = tempDir </> "windy-day"
            fileOne = "floating_leaf.txt"
            fileOnePath = cwd </> fileOne
            fileOneUuid = "13995cc1-2bce-44a5-a135-8219a2e01e97"
            fileTwo = "flying-bird"
            fileTwoPath = cwd </> fileTwo
            fileTwoUuid = "aa2b0648-ee49-406c-8fc7-f0c07e67e667"
            fileGoneOne = "bus"
            fileGoneOnePath = cwd </> fileGoneOne
            fileGoneOneUuid = "d0cb530a-c7e0-4d27-b3d5-5b00bf729467"
            newFileOne = "perspiration.txt"
            newFileOnePath = cwd </> newFileOne
        HroamerDb.createDbAndTables pathToDb
        mapM_ createDirectory [cwd, appTmpDir]
        writeFile fileOnePath "I see a leaf floating around"
        writeFile fileTwoPath "It's a bird!"
        writeFile newFileOnePath "I am sweating despite the wind."
        HroamerDb.wrapDbConn
          pathToDb
          (\addFileDetailsToDb -> do
            addFileDetailsToDb cwd (fileOne, fileOneUuid)
            addFileDetailsToDb cwd (fileTwo, fileTwoUuid)
            addFileDetailsToDb cwd (fileGoneOne, fileGoneOneUuid))
          addFileDetailsToDb
        (filesAndUuids, _) <- processCwd cwd appTmpDir pathToDb
        length filesAndUuids `shouldBe` 3
        -- compare the db stuff first, then use it to compare against
        -- filesAndUuids
        filesToUuidInDb <- fmap fromList $
          HroamerDb.getAllFilesInDir pathToDb cwd
        size filesToUuidInDb `shouldBe` 3
        lookup (toList fileOne) filesToUuidInDb `shouldBe` Just fileOneUuid
        lookup (toList fileTwo) filesToUuidInDb `shouldBe` Just fileTwoUuid
        lookup (toList fileGoneOne) filesToUuidInDb `shouldBe` Nothing
        let maybeNewFileOneUuid = lookup (toList newFileOne) filesToUuidInDb
        maybeNewFileOneUuid `shouldSatisfy` isJust
        let newFileOneUuid = fromJust maybeNewFileOneUuid
        sortBy (compare `on` fst) filesAndUuids `shouldBe`
          [ (fileOne, fileOneUuid)
          , (fileTwo, fileTwoUuid)
          , (newFileOne, newFileOneUuid)
          ]
