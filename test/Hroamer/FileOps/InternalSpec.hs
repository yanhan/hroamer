module Hroamer.FileOps.InternalSpec
  ( spec
  ) where

import Control.Monad.Reader (runReader)
import qualified Data.Map.Strict as M
import Data.Set (fromList)
import Foundation hiding (fromList)
import System.FilePath.Posix ((</>))
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

import Hroamer.DataStructures
       (FileOpsReadState(FileOpsReadState), FileRepr(FileRepr))
import Hroamer.FileOps.Internal
       (FileOp(CopyOp, LookupDbCopyOp, TrashCopyOp), dirToTrashCopyTo,
        genCopyOps, genTrashCopyOps)

spec :: Spec
spec = parallel $ do
  describe "genTrashCopyOps" $
    it "should generate the list of TrashCopyOp (in sorted order) for files that the user wants to remove" $
      let cwd = "/the/blessed/era"
          pathToTrashCopyDir = "/things/die/here"
          r = FileOpsReadState cwd  ""  pathToTrashCopyDir
          toStay = (cwd </> "roost.png", "83ac96e0-117b-470e-a534-c98a880d30e8")
          toRemoveOneFilename = "sre-guidelines.txt"
          toRemoveOne = ( cwd </> toRemoveOneFilename
                        , "d9f6ba2e-634a-4b19-89d5-b1bfca2c67cf"
                        )
          toRemoveTwoFilename = "config.h"
          toRemoveTwo = ( cwd </> toRemoveTwoFilename
                        , "9ddb81e2-c25f-43e9-9d77-c64d20c577fa"
                        )
          newStuff = (cwd </> "combustion", "28532de4-db73-47a7-b8a1-ee02bf42a73d")
          initial = fromList [toStay, toRemoveOne, toRemoveTwo]
          current = fromList [toStay, newStuff]
          expected = [ TrashCopyOp
                         (FileRepr cwd toRemoveTwoFilename)
                         (FileRepr
                           (dirToTrashCopyTo pathToTrashCopyDir $ snd toRemoveTwo)
                           toRemoveTwoFilename)
                         (snd toRemoveTwo)
                     , TrashCopyOp
                         (FileRepr cwd toRemoveOneFilename)
                         (FileRepr
                           (dirToTrashCopyTo pathToTrashCopyDir $ snd toRemoveOne)
                           toRemoveOneFilename)
                         (snd toRemoveOne)
                     ]
          actual = runReader (genTrashCopyOps initial current) r
       in expected `shouldBe` actual

  describe "genCopyOps" $ do
    it "should generate the list of FileOp for files that need to be copied" $
      let cwd = "/chilli/tomatoes/garlic"
          trashCopyFilename = "onions"
          trashCopyOpDestFileRepr = FileRepr "/fake/trash/dir"  trashCopyFilename
          trashCopyOpUuid = "076f6f46-76f2-429a-a586-a132e4b20b3f"
          -- File renames in same dir
          -- file one
          fileOneName = "olive-oil"
          fileOneUuid = "6019cfce-57c7-480e-a22e-8406dad2bca3"
          fileOneNewName = "coconut-oil"
          -- file two
          fileTwoName = "basil_leaves"
          fileTwoUuid = "35b1f941-dde3-479d-948b-d649648f8a6a"
          fileTwoNewName = "parsley"
          -- Files that we need to lookup from the db
          lkName = "linguine"
          lkUuid = "7c6b8a68-348a-4ce5-85dd-284ab1eea3ec"

          uuidToTrashCopyOp = M.fromList [(trashCopyOpUuid, trashCopyOpDestFileRepr)]
          initialUuidToPath = M.fromList [ (fileOneUuid, cwd </> fileOneName)
                                         , (fileTwoUuid, cwd </> fileTwoName)
                                         ]
          toCopy = [ (cwd </> trashCopyFilename, trashCopyOpUuid)
                   , (cwd </> fileOneNewName, fileOneUuid)
                   , (cwd </> fileTwoNewName, fileTwoUuid)
                   , (cwd </> lkName, lkUuid)
                   ]
          expected = [ CopyOp trashCopyOpDestFileRepr  (FileRepr cwd trashCopyFilename)
                     , CopyOp (FileRepr cwd fileOneName)  (FileRepr cwd fileOneNewName)
                     , CopyOp (FileRepr cwd fileTwoName)  (FileRepr cwd fileTwoNewName)
                     , LookupDbCopyOp (FileRepr cwd lkName) lkUuid
                     ]
          actual = runReader
                     (genCopyOps uuidToTrashCopyOp initialUuidToPath toCopy)
                     (FileOpsReadState cwd  ""  "")
      in expected `shouldBe` actual
