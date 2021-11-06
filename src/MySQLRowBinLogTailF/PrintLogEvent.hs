{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module MySQLRowBinLogTailF.PrintLogEvent where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.ANSI (brightGreen, brightRed, brightYellow)
import Database.MySQL.Base (MySQLValue (..))
import Database.MySQL.ColumnInfo (ColumnInfo (ColumnInfo, name))
import Database.MySQL.Name
import MySQLRowBinLogTailF.ParseRowBinLogEvent
import RIO
import RIO.Extended
import RIO.List (intersperse)
import RIO.Text (justifyRight)
import Prelude ()

printLogEvent :: LogEvent -> IO ()
printLogEvent LogEvent{time, event} = do
    putUtf8BuilderLn $ "\n" <> displayShow time
    case event of
        Left queryEvent -> do
            putUtf8BuilderLn $ displayShow queryEvent
        Right ModifyEvent{schemaName, tableName, targetRows} -> do
            let (op, builders) = case targetRows of
                    Insert rows -> (brightGreen "INSERT", map buildColumns rows)
                    Update rows -> (brightYellow "UPDATE", map buildColumns2 rows)
                    Delete rows -> (brightRed "DELETE", map buildColumns rows)
            putUtf8BuilderLn $ display op <> " " <> buildSchemaAndTableName schemaName tableName
            for_ (zip [1 ..] builders) $ \(index, builder) -> do
                putUtf8BuilderLn $
                    mconcat
                        [ "["
                        , display (index :: Int)
                        , "]"
                        , "\n"
                        , builder
                        ]

buildSchemaAndTableName :: SchemaName -> TableName -> Utf8Builder
buildSchemaAndTableName (SchemaName schema) (TableName table) =
    display schema <> "." <> display table

buildColumns2 :: [(ColumnInfo, (MySQLValue, MySQLValue))] -> Utf8Builder
buildColumns2 cols = mconcat . intersperse "\n" $ map buildColumnUpdate cols
  where
    buildColumnUpdate :: (ColumnInfo, (MySQLValue, MySQLValue)) -> Utf8Builder
    buildColumnUpdate (col, (before, after))
        | before == after =
            display (adjustColumName cols col) <> ": " <> buildMySQLValue before col
        | otherwise =
            display (brightYellow (adjustColumName cols col <> ": "))
                <> buildMySQLValue before col
                <> display (brightYellow " -> ")
                <> buildMySQLValue after col

buildColumns :: [(ColumnInfo, MySQLValue)] -> Utf8Builder
buildColumns cols = mconcat . intersperse "\n" $ map buildColumn cols
  where
    buildColumn (col, val) =
        display (adjustColumName cols col) <> ": " <> buildMySQLValue val col

adjustColumName :: forall a. [(ColumnInfo, a)] -> ColumnInfo -> Text
adjustColumName cols ColumnInfo{name = ColumnName name} =
    let maxNameWidth = foldl' max 0 $ map (\(ColumnInfo{name = ColumnName name'}, _) -> T.length name') cols
     in justifyRight maxNameWidth ' ' name

{-
 * Column情報が必要なのは bit だけ。MySQLValue では bitの長さ情報がないため
 * 数値型の display_width は利用りない
-}
buildMySQLValue :: MySQLValue -> ColumnInfo -> Utf8Builder
buildMySQLValue (MySQLDecimal sci) _ = displayShow sci
buildMySQLValue (MySQLInt8U w8) _ = display w8
buildMySQLValue (MySQLInt8 i8) _ = display i8
buildMySQLValue (MySQLInt16U w16) _ = display w16
buildMySQLValue (MySQLInt16 i16) _ = display i16
buildMySQLValue (MySQLInt32U w32) _ = display w32
buildMySQLValue (MySQLInt32 i32) _ = display i32
buildMySQLValue (MySQLInt64U w64) _ = display w64
buildMySQLValue (MySQLInt64 i64) _ = display i64
buildMySQLValue (MySQLFloat f) _ = display f
buildMySQLValue (MySQLDouble d) _ = display d
buildMySQLValue (MySQLYear y) _ = display y
buildMySQLValue (MySQLDateTime ltime) _ = displayShow ltime
buildMySQLValue (MySQLTimeStamp ltime) _ = displayShow ltime
buildMySQLValue (MySQLDate day) _ = displayShow day
buildMySQLValue (MySQLTime _ time) _ = displayShow time
buildMySQLValue (MySQLGeometry bs) _ = displayShow bs
buildMySQLValue (MySQLBytes bs) _ = "bytes: length=" <> display (BS.length bs)
buildMySQLValue (MySQLBit w64) _ = display w64
buildMySQLValue (MySQLText txt) _ = "\"" <> display txt <> "\""
buildMySQLValue (MySQLNull) _ = "NULL"
