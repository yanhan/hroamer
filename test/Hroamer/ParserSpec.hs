module Hroamer.ParserSpec
  ( spec
  ) where

import Data.Either (either)
import Foundation
import Test.Hspec (Spec, describe, it, parallel, shouldBe)
import Text.Parsec (ParseError(..), runParser)

import Hroamer.Parser (parseDirStateLine)

spec :: Spec
spec = parallel $ do
  describe "parseDirStateLine" $ do
    it "should return (Right Nothing) for a comment line" $
      runParser parseDirStateLine () ""  "\" just a comment"  `shouldBe` Right Nothing

    it "should return (Right filename, uuid)) for a line which contains a filename and uuid separated by \" | \"" $ do
      let filename = "main.c"
      let uuid = "51z9125f-351c-4610-125m-n15jfa086j4h"
      runParser parseDirStateLine () ""  (filename <> " | " <> uuid) `shouldBe`
        (Right $ Just (filename, uuid))

    it "should return Left ParseError for a line which does not fulfil the specifications" $ do
      let filename = "special.a"
      let invalidUuid = "c0aa744f664a7157465c9ac52b87d044"
      either
        (const $ True `shouldBe` True)
        (const $ True `shouldBe` False)
        (runParser parseDirStateLine () ""  (filename <> " | " <> invalidUuid))
