module Hroamer.FileOps.InternalSpec
  ( spec
  ) where

import Control.Monad.Reader (runReader)
import Data.Set (fromList)
import Foundation hiding (fromList)
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

import Hroamer.DataStructures
       (FileOpsReadState(FileOpsReadState), FileRepr(FileRepr))
import Hroamer.FileOps.Internal
       (FileOp(TrashCopyOp), dirToTrashCopyTo, genTrashCopyOps)

spec :: Spec
spec = parallel $
  describe "genTrashCopyOps" $
    it "should generate the list of TrashCopyOp (in sorted order) for files that the user wants to remove" $
      let cwd = "/the/blessed/era"
          pathToTrashCopyDir = "/things/die/here"
          r = FileOpsReadState cwd  ""  pathToTrashCopyDir
          toStay = ("roost.png", "83ac96e0-117b-470e-a534-c98a880d30e8")
          toRemoveOne = ("sre-guidelines.txt", "d9f6ba2e-634a-4b19-89d5-b1bfca2c67cf")
          toRemoveTwo = ("config.h", "9ddb81e2-c25f-43e9-9d77-c64d20c577fa")
          newStuff = ("combustion", "28532de4-db73-47a7-b8a1-ee02bf42a73d")
          initial = fromList [toStay, toRemoveOne, toRemoveTwo]
          current = fromList [toStay, newStuff]
          expected = [ TrashCopyOp
                         (FileRepr cwd $ fst toRemoveTwo)
                         (FileRepr (dirToTrashCopyTo pathToTrashCopyDir $ snd toRemoveTwo) (fst toRemoveTwo))
                         (snd toRemoveTwo)
                     , TrashCopyOp
                         (FileRepr cwd $ fst toRemoveOne)
                         (FileRepr (dirToTrashCopyTo pathToTrashCopyDir $ snd toRemoveOne) (fst toRemoveOne))
                         (snd toRemoveOne)
                     ]
          actual = runReader (genTrashCopyOps initial current) r
       in expected `shouldBe` actual
