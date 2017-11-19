module TestHelpers
  ( RowCount
  , getTotalRows
  ) where

import Database.SQLite.Simple (Connection, query_)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Foundation

type RowCount = Int

instance FromRow Int where
  fromRow = field

getTotalRows :: Connection -> IO [RowCount]
getTotalRows dbconn = query_ dbconn "SELECT COUNT(1) FROM files;"
