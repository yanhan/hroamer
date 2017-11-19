module Hroamer.Database.InternalSpec
  ( spec
  ) where

import Database.SQLite.Simple (withConnection, query_)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Foundation
import System.FilePath.Posix ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldReturn)

import Hroamer.Database (createDbAndTables)
import Hroamer.Database.Internal (addFileDetailsToDb)

instance FromRow Int where
  fromRow = field

spec :: Spec

spec = do
  describe "addFileDetailsToDb" $ do
    it "should add a new row to the database (assuming the (dir, filename) combo does not exist)" $ do
      withSystemTempDirectory "createDbAndTables" $ \dirPath -> do
        let pathToDb = dirPath </> "hroamer.db"
        createDbAndTables pathToDb
        let dir = "/home/edmund"
        let filename = "secret.txt"
        let uuid = "951ebecc-0048-4102-9c32-ecebb21c5bdc"
        withConnection
          pathToDb
          (\dbconn -> do
            query_ dbconn "SELECT COUNT(1) FROM files;" `shouldReturn` [0 :: Int]
            addFileDetailsToDb dir dbconn (filename, uuid)
            query_ dbconn "SELECT COUNT(1) FROM files;" `shouldReturn` [1 :: Int])
