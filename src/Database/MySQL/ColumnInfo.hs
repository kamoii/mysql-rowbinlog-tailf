{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Database.MySQL.ColumnInfo (
    columnInfo,
    ColumnInfo (..),
) where

import Database.MySQL.Base (MySQLConn, MySQLValue (MySQLText), query)
import Database.MySQL.DataType (DataType, parseDataType)
import Database.MySQL.Name
import Database.MySQL.Record (fromQueryResult)
import RIO
import RIO.Extended
import Text.Megaparsec (ParseErrorBundle)
import Prelude ()

{- | テーブルのカラム情報を取得する

 * データは inforamtion_schema の columns テーブルから取得
 * カラムの順序は ordinal_position 順に取得される

NOTE: 明示的にカラムを SELECT句で指定しないと大文字で返してくる。
-}
columnInfo :: MySQLConn -> SchemaName -> TableName -> IO [ColumnInfo]
columnInfo con (SchemaName schema) (TableName table) = do
    rows <- throwE =<< fromQueryResult =<< query con q [MySQLText schema, MySQLText table]
    throwE . sequence $ map rowToColumnInfo rows
  where
    q =
        fromString $
            unlines
                [ " SELECT column_name,"
                , "        column_type,"
                , "        column_key,"
                , "        ordinal_position,"
                , "        character_set_name,"
                , "        extra"
                , "  FROM information_schema.columns"
                , " WHERE table_schema = ? AND table_name = ?"
                , " ORDER BY ordinal_position"
                ]

data ColumnInfo = ColumnInfo
    { name :: ColumnName
    , dataType :: DataType
    , key :: Text
    , ordinalPosition :: Integer
    , characterSetName :: Maybe Text
    , extra :: Text
    }
    deriving (Generic, Show)

data Row = Row
    { column_name :: Text
    , column_type :: Text
    , column_key :: Text
    , ordinal_position :: Integer
    , character_set_name :: Maybe Text
    , extra :: Text
    }
    deriving (Generic, Show)

rowToColumnInfo :: Row -> Either (ParseErrorBundle Text Void) ColumnInfo
rowToColumnInfo Row{column_type = column_type_raw, ..} = do
    dataType <- parseDataType column_type_raw
    pure $
        ColumnInfo
            { name = ColumnName column_name
            , dataType = dataType
            , key = column_key
            , ordinalPosition = ordinal_position
            , characterSetName = character_set_name
            , extra = extra
            }
