module Hroamer.PathSpec
  ( spec
  ) where

import Foundation
import System.FilePath ((</>), FilePath)
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe)

import Hroamer.Path (isWeakAncestorDir)

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
      pendingWith "have to fix this bug first"
      isWeakAncestorDir "/var/lib/apt/cache/"  "/var/lib/apt/cache" `shouldBe` True

    it "should return False when a path is not a descendent of a suspected ancestor path" $ do
      isWeakAncestorDir "/home/mike/blurb"  "/dev/sda1" `shouldBe` False
