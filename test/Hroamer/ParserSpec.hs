module Hroamer.ParserSpec
  ( spec
  ) where

import Data.Either (either, isLeft)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Foundation
import Test.Hspec
       (Spec, describe, it, parallel, shouldBe, shouldSatisfy)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Text.Parsec (ParseError(..), runParser)
import Test.QuickCheck
       (Gen, Property, choose, forAll, listOf1, shuffle, suchThat,
        vectorOf)

import Hroamer.Char (isNotSpace)
import Hroamer.Parser (parseDirStateLine)
import Hroamer.StateFile (separator)
import TestHelpers (genCharNotNull, genSpace, genValidFilePathChar)

genFilenameWithNonTrailingSpace :: Gen Text
genFilenameWithNonTrailingSpace = do
  space <- genSpace
  otherChars <- listOf1 genValidFilePathChar
  let (beforeSpace, afterSpace) = splitAt (length otherChars - 1) otherChars
  fmap (pack . (<> afterSpace)) $ shuffle $ beforeSpace <> [space]

genFilenameWithTrailingSpaces :: Gen Text
genFilenameWithTrailingSpaces = do
  n <- choose (1, 5)
  spaces <- vectorOf n genSpace
  fmap (pack . (<> spaces)) $ listOf1 genValidFilePathChar

spec :: Spec
spec = parallel $ do
  describe "parseDirStateLine" $ do
    it "should return (Right Nothing) for a comment line" $
      runParser parseDirStateLine () ""  "\" just a comment"  `shouldBe` Right Nothing

    it "should return (Right filename, uuid)) for a line which contains a filename and uuid separated by a separator" $ do
      let filename = "main.c"
      let uuid = "51z9125f-351c-4610-125m-n15jfa086j4h"
      runParser parseDirStateLine () ""  (filename <> separator <> uuid) `shouldBe`
        (Right $ Just (filename, uuid))

    it "should return (Right filename, uuid) for a line which contains a 'filename | uuid | original path'" $ do
      let filename = "atta-boy.jpeg"
          uuid = "7fbb76ac-1980-4bdc-8aaf-da4852c2c6f5"
          orgPath = "/vortex/red/flag/atta-boy.jpeg"
          line = filename <> separator <> uuid <> separator <> orgPath
      runParser parseDirStateLine () "" line `shouldBe` (Right $ Just (filename, uuid))

    it "should return Left ParseError for a line which does not fulfil the specifications" $ do
      let filename = "special.a"
      let invalidUuid = "c0aa744f664a7157465c9ac52b87d044"
      either
        (const $ True `shouldBe` True)
        (const $ True `shouldBe` False)
        (runParser parseDirStateLine () ""  (filename <> separator <> invalidUuid))

    it "should return Left ParseError for a line that has a filename with non-trailing space" $
      forAll genFilenameWithNonTrailingSpace $ \filenameWithNonTrailingSpace ->
        let uuid = "af76c156-e643-45a5-953c-43676ba7e57c"
            orgPath = "/linguine/sushi/curry"
            line = filenameWithNonTrailingSpace <>
                     separator <>
                     uuid <>
                     separator <>
                     orgPath
        in runParser parseDirStateLine () "" line `shouldSatisfy` isLeft

    modifyMaxSuccess (const 30) $ it "should ignore trailing spaces in a filename" $
      forAll genFilenameWithTrailingSpaces $ \filenameWithTrailingSpaces ->
        let uuid = "589875aa-1996-4488-a4e6-a26fd6313fb7"
            orgPath = "/dark/sausage/brine"
            line = filenameWithTrailingSpaces <>
                     separator <>
                     uuid <>
                     separator <>
                     orgPath
            expectedResult = Right . Just $
              (T.takeWhile isNotSpace filenameWithTrailingSpaces, uuid)
        in runParser parseDirStateLine () "" line `shouldBe` expectedResult
