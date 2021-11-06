module MySQLRowBinLogTailF.Main (run) where

import Data.Time (getCurrentTimeZone)
import Database.MySQL.Base
import Database.MySQL.BinLog
import Database.MySQL.ColumnInfo (columnInfo)
import MySQLRowBinLogTailF.ParseRowBinLogEvent (LogEvent, parseRowBinLogEvent)
import MySQLRowBinLogTailF.PrintLogEvent (printLogEvent)
import RIO
import RIO.Extended
import System.IO.Streams qualified as St
import Prelude ()

{-

binlog の形式が row でなくても decodeRowBinLogEvent は適用可能らしい。
ただし ROW 系のイベントでない限り単に無視されるので適切に検出する必要がある。

-}
run :: ConnectInfo -> IO ()
run ci = run' ci printLogEvent

-- ライブラリとして使いたい場合
run' :: ConnectInfo -> (LogEvent -> IO ()) -> IO ()
run' ci logEventHandler = do
    timeZone <- getCurrentTimeZone
    bracket (connect ci) close $ \conForBinLog -> do
        bracket (connect ci) close $ \conForColumnInfo -> do
            _ <- registerPesudoSlave conForBinLog slaveId
            Just binLogTracker <- getLastBinLogTracker conForBinLog
            putUtf8BuilderLn $ displayShow binLogTracker
            eventStream <- decodeRowBinLogEvent =<< dumpBinLog conForBinLog slaveId binLogTracker False
            whileJust_ (St.read eventStream) $ \rowBinLogEvent -> do
                logEvent <- parseRowBinLogEvent timeZone (columnInfo conForColumnInfo) rowBinLogEvent
                logEventHandler logEvent
  where
    -- Slave IDは適当に決みているがこれで正しいのか？？
    slaveId = 1234
