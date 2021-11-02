module MySQLRowBinLogTailF.Main (run) where

import Data.Time (getCurrentTimeZone)
import Database.MySQL.Base
import Database.MySQL.BinLog
import Database.MySQL.ColumnInfo (columnInfo)
import MySQLRowBinLogTailF.ParseRowBinLogEvent (parseRowBinLogEvent)
import MySQLRowBinLogTailF.PrintLogEvent (printLogEvent)
import RIO
import RIO.Extended
import qualified System.IO.Streams as St
import Prelude ()

{-

binlog の形式が row でなくても decodeRowBinLogEvent は適用可能らしい。
ただし ROW 系のイベントでない限り単に無視されるので適切に検出する必要がある。

-}
run :: IO ()
run = do
    bracket (connect defaultConnectInfo) close $ \conForBinLog -> do
        bracket (connect defaultConnectInfo) close $ \conForColumnInfo -> do
            let slaveId = 1234
            timeZone <- getCurrentTimeZone
            _ <- registerPesudoSlave conForBinLog slaveId
            Just binLogTracker <- getLastBinLogTracker conForBinLog
            putUtf8BuilderLn $ displayShow binLogTracker
            eventStream <- decodeRowBinLogEvent =<< dumpBinLog conForBinLog slaveId binLogTracker False
            whileJust_ (St.read eventStream) $ \rowBinLogEvent -> do
                logEvent <- parseRowBinLogEvent timeZone (columnInfo conForColumnInfo) rowBinLogEvent
                printLogEvent logEvent
