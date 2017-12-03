module Hroamer.FileOpsSpec
  ( spec
  ) where

import Control.Monad.Reader (runReader)
import Foundation
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

import Hroamer.DataStructures
       (FileOpsReadState(FileOpsReadState), FileRepr(FileRepr))
import Hroamer.FileOps
       (FileOp(CopyOp, LookupDbCopyOp, TrashCopyOp), generateFileOps)
import Hroamer.FileOps.Internal (dirToTrashCopyTo)


spec :: Spec
spec = parallel $
  describe "generateFileOps" $
    it "should generate a list of FileOp; with TrashCopyOp first, followed by everything else (sorted by filename)" $
      let cwd = "/boiling/water"
          trashCopyDir = "/rising/turtle"
          -- files that will not be touched
          safeOneFilename = "avocado"
          safeOneUuid = "012fe19a-b303-4b1d-bf4d-4422e938f7d4"
          safeOne = (safeOneFilename, safeOneUuid)
          safeTwoFilename = "barley"
          safeTwoUuid = "43012c07-6279-4a52-9369-25a422bd2df0"
          safeTwo = (safeTwoFilename, safeTwoUuid)
          -- files that will be removed
          --
          -- first file
          toRemoveOneFilename = "crabs"
          toRemoveOneUuid = "c8055958-fffb-40d2-89f6-73b6625667b7"
          toRemoveOne = (toRemoveOneFilename, toRemoveOneUuid)
          -- second file
          toRemoveTwoFilename = "allspark"
          toRemoveTwoUuid = "9f7f3317-0f0f-4d4d-887d-18446353c909"
          toRemoveTwo = (toRemoveTwoFilename, toRemoveTwoUuid)
          -- files that will be copied
          --
          -- first new file
          toCopyOneFilename = "courgette"
          toCopyOneUuid = safeTwoUuid
          toCopyOne = (toCopyOneFilename, toCopyOneUuid)
          -- second new file
          toCopyTwoFilename = "dill"
          toCopyTwoUuid = safeTwoUuid
          toCopyTwo = (toCopyTwoFilename, toCopyTwoUuid)
          -- files that will be renamed
          toRenameFilename = "aubergine"
          toRenameUuid = toRemoveTwoUuid
          toRename = (toRenameFilename, toRenameUuid)
          -- file that has to be looked up
          toLookupFilename = "egg"
          toLookupUuid = "c95581e7-0b48-4b68-b780-13a0e20c979c"
          toLookup = (toLookupFilename, toLookupUuid)
          --
          initial = [toRemoveOne, toRemoveTwo, safeOne, safeTwo]
          current = [toLookup, toCopyOne, toRename, safeOne, safeTwo, toCopyTwo]
          r = FileOpsReadState cwd "" trashCopyDir
          expected = [ TrashCopyOp
                         (FileRepr cwd toRemoveTwoFilename)
                         (FileRepr (dirToTrashCopyTo trashCopyDir toRemoveTwoUuid)
                                   toRemoveTwoFilename)
                         toRemoveTwoUuid
                     , TrashCopyOp
                         (FileRepr cwd toRemoveOneFilename)
                         (FileRepr (dirToTrashCopyTo trashCopyDir toRemoveOneUuid)
                                   toRemoveOneFilename)
                         toRemoveOneUuid
                     , CopyOp (FileRepr (dirToTrashCopyTo trashCopyDir toRemoveTwoUuid)
                                        toRemoveTwoFilename)
                              (FileRepr cwd toRenameFilename)
                     , CopyOp
                         (FileRepr cwd safeTwoFilename)
                         (FileRepr cwd toCopyOneFilename)
                     , CopyOp
                         (FileRepr cwd safeTwoFilename)
                         (FileRepr cwd toCopyTwoFilename)
                     , LookupDbCopyOp (FileRepr cwd toLookupFilename) toLookupUuid
                     ]
          actual = runReader (generateFileOps current initial) r
      in actual `shouldBe` expected
