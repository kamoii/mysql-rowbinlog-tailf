{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
module MySQLRowBinLogTailF.ParseRowBinLogEvent where

import Data.Generics.Product.Typed
import Data.Text.Encoding (decodeUtf8)
import Data.Time (LocalTime, TimeZone, utcToLocalTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Database.MySQL.Base (BitMap, MySQLValue, Query, isColumnSet)
import Database.MySQL.BinLog (BinLogValue, DeleteRowsEvent (deletePresentMap, deleteRowData), QueryEvent' (QueryEvent'), RowBinLogEvent (RowDeleteEvent, RowQueryEvent, RowUpdateEvent, RowWriteEvent), TableMapEvent (tmColumnCnt, tmSchemaName, tmTableName), UpdateRowsEvent (updatePresentMap, updateRowData), WriteRowsEvent (writePresentMap, writeRowData))
import Database.MySQL.BinLogValue (binLogValueToMySQLValue)
import Database.MySQL.ColumnInfo (ColumnInfo (ColumnInfo, dataType))
import Database.MySQL.Name
import RIO
import Prelude ()

data LogEvent = LogEvent
    { time :: LocalTime
    , event :: Either QueryEvent ModifyEvent
    }

data ModifyEvent = ModifyEvent
    { schemaName :: SchemaName
    , tableName :: TableName
    , targetRows :: TargetRows
    }

data TargetRows
    = Insert [[(ColumnInfo, MySQLValue)]]
    | Update [[(ColumnInfo, (MySQLValue, MySQLValue))]]
    | Delete [[(ColumnInfo, MySQLValue)]]

newtype QueryEvent = QueryEvent Query
    deriving (Show)

-- 失なわれたカラム情報を ColumnInfo から補いながら,本プログラムに必要なデータを
-- 処理しやすい形で抽出する。
parseRowBinLogEvent ::
    TimeZone ->
    (SchemaName -> TableName -> IO [ColumnInfo]) ->
    RowBinLogEvent ->
    IO LogEvent
parseRowBinLogEvent timeZone getColumnInfo event = do
    let timestamp = getTyped @Word32 event
    let localTime = mkLocalTimeFromTimeStamp timeZone (fromIntegral timestamp)
    LogEvent localTime <$> case event of
        RowQueryEvent _ _ (QueryEvent' query) ->
            pure $ Left $ QueryEvent query
        RowWriteEvent _ _ tmEv wrEv ->
            fmap Right $
                modifyTable tmEv (writePresentMap wrEv) $ \columns ->
                    Insert . map (map toMySQLValue') <$> traverse (indexByColumn columns) (writeRowData wrEv)
        RowUpdateEvent _ _ tmEv upEv -> do
            fmap Right $
                modifyTable tmEv (fst $ updatePresentMap upEv) $ \columns -> do
                    let (b0, b1) = updatePresentMap upEv
                    assertIO (b0 == b1)
                    fmap Update $
                        for (updateRowData upEv) $ \(beforeRow, afterRow) -> do
                            beforeRow' <- map toMySQLValue' <$> indexByColumn columns beforeRow
                            afterRow' <- map toMySQLValue' <$> indexByColumn columns afterRow
                            pure $ zipWith (\(c, b) (_, a) -> (c, (b, a))) beforeRow' afterRow'
        RowDeleteEvent _ _ tmEv delEv -> do
            fmap Right $
                modifyTable tmEv (deletePresentMap delEv) $ \columns ->
                    Delete . map (map toMySQLValue') <$> traverse (indexByColumn columns) (deleteRowData delEv)
  where
    assertIO :: Bool -> IO ()
    assertIO True = pure ()
    assertIO False = error ""

    modifyTable :: TableMapEvent -> BitMap -> ([ColumnInfo] -> IO TargetRows) -> IO ModifyEvent
    modifyTable tableMapEvent columnsBitMap cb = do
        let schemaName = SchemaName $ decodeUtf8 $ tmSchemaName tableMapEvent
        let tableName = TableName $ decodeUtf8 $ tmTableName tableMapEvent
        columnsAll <- getColumnInfo schemaName tableName
        assertIO $ length columnsAll == tmColumnCnt tableMapEvent
        let columnsTarget = extractColumns columnsAll columnsBitMap
        targetRows <- cb columnsTarget
        pure $ ModifyEvent{schemaName, tableName, targetRows}

    -- BitMap から操作対象のカラム列のみ取り出す
    extractColumns :: [b] -> BitMap -> [b]
    extractColumns columns bitMap =
        zip [0 ..] columns
            & map (first (isColumnSet bitMap))
            & filter fst
            & map snd

    -- cols と vals が同じ長さでなければ例外を投げる。
    indexByColumn :: [ColumnInfo] -> [BinLogValue] -> IO [(ColumnInfo, BinLogValue)]
    indexByColumn cols vals
        | length cols == length vals = pure $ zip cols vals
        | otherwise = error ""

    toMySQLValue :: ColumnInfo -> BinLogValue -> MySQLValue
    toMySQLValue ColumnInfo{dataType} val =
        binLogValueToMySQLValue timeZone (val, dataType)

    toMySQLValue' =
        (,) <$> fst <*> uncurry toMySQLValue

{- | timestamp から LocalTime を作成する。

受け取る型は POSIXTime だが、これは NominalDiffTime 型の型エイリアスである。
NominalDiffTime は Num/Fractional のインスタンスを持っている。渡す数値は秒
として解釈される。
-}
mkLocalTimeFromTimeStamp :: TimeZone -> POSIXTime -> LocalTime
mkLocalTimeFromTimeStamp timeZone posixTime =
    utcToLocalTime timeZone $ posixSecondsToUTCTime posixTime
