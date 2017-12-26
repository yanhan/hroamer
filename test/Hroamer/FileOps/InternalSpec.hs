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
       (AbsFilePath(AbsFilePath), FileOpsReadState(FileOpsReadState),
        FileRepr(FileRepr))
import Hroamer.FileOps.Internal
       (FileOp(CopyOp, LookupDbCopyOp, TrashCopyOp), dirToTrashCopyTo,
        genCopyOps, genTrashCopyOps)

spec :: Spec
spec = parallel $ do
  describe "genTrashCopyOps" $
    it "should generate the list of TrashCopyOp (in sorted order) for files that the user wants to remove" $
      let cwd = "/the/blessed/era"
          pathToTrashCopyDir = "/things/die/here"
          r = FileOpsReadState ""  pathToTrashCopyDir
          toStay = ( AbsFilePath $ cwd </> "roost.png"
                   , "83ac96e0-117b-470e-a534-c98a880d30e8"
                   )
          toRemoveOneFilename = "sre-guidelines.txt"
          toRemoveOne = ( AbsFilePath $ cwd </> toRemoveOneFilename
                        , "d9f6ba2e-634a-4b19-89d5-b1bfca2c67cf"
                        )
          toRemoveTwoFilename = "config.h"
          toRemoveTwo = ( AbsFilePath $ cwd </> toRemoveTwoFilename
                        , "9ddb81e2-c25f-43e9-9d77-c64d20c577fa"
                        )
          newStuff = ( AbsFilePath $ cwd </> "combustion"
                     , "28532de4-db73-47a7-b8a1-ee02bf42a73d"
                     )
          newStuffTwo = ( AbsFilePath $ "/another/dir/on/my/system" </> "fish"
                        , "3b604de4-016d-440e-8695-4b79d3a8956c"
                        )
          initial = fromList [toStay, toRemoveOne, toRemoveTwo]
          current = fromList [toStay, newStuff, newStuffTwo]
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
          cwdParent = "/chilli/tomatoes"
          anotherDir = "/pay/me"
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
          ---- From anotherDir
          lkNameTwo = "bolognese"
          lkTwoUuid = "82d4a5cf-6394-44c1-8439-6e034619b6b8"
          ---- From cwdParent (parent dir of the cwd)
          lkNameThree = "pounds"
          lkThreeUuid = "5ce2201b-ac56-459a-ae85-9da3bf9a313a"

          uuidToTrashCopyOp = M.fromList [(trashCopyOpUuid, trashCopyOpDestFileRepr)]
          initialUuidToPath = M.fromList [ (fileOneUuid, AbsFilePath $ cwd </> fileOneName)
                                         , (fileTwoUuid, AbsFilePath $ cwd </> fileTwoName)
                                         ]
          toCopy = [ (AbsFilePath $ cwdParent </> lkNameThree, lkThreeUuid)
                   , (AbsFilePath $ cwd </> trashCopyFilename, trashCopyOpUuid)
                   , (AbsFilePath $ cwd </> fileOneNewName, fileOneUuid)
                   , (AbsFilePath $ cwd </> fileTwoNewName, fileTwoUuid)
                   , (AbsFilePath $ cwd </> lkName, lkUuid)
                   , (AbsFilePath $ anotherDir </> lkNameTwo, lkTwoUuid)
                   ]
          expected = [ LookupDbCopyOp (FileRepr cwdParent lkNameThree) lkThreeUuid
                     , CopyOp trashCopyOpDestFileRepr  (FileRepr cwd trashCopyFilename)
                     , CopyOp (FileRepr cwd fileOneName)  (FileRepr cwd fileOneNewName)
                     , CopyOp (FileRepr cwd fileTwoName)  (FileRepr cwd fileTwoNewName)
                     , LookupDbCopyOp (FileRepr cwd lkName) lkUuid
                     , LookupDbCopyOp (FileRepr anotherDir lkNameTwo) lkTwoUuid
                     ]
          actual = genCopyOps uuidToTrashCopyOp initialUuidToPath toCopy
      in expected `shouldBe` actual
