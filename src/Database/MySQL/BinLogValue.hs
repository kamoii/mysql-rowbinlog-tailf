{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.MySQL.BinLogValue (binLogValueToMySQLValue) where

import Data.Bits (Bits (xor))
import Data.Time
import Data.Time.Clock.POSIX
import Database.MySQL.Base
import Database.MySQL.BinLog
import Database.MySQL.DataType
import RIO
import Prelude ()

{- | BinLogValue + カラム情報 -> MySQLValue

`mysql-haskell` では binlog のデータは MySQLValue には変換されていない
(効率のため)。

それとは別に MySQLの仕様問題がある。MySQL の仕様でbinlogの整数データは
符号情報を持たない。また blob/stringはまとめて Bytesとされている。その
ため BinLogValue のみからでは MySQLValue を得ることはできない。
TableMapEvent には COLUMN_TYPE と COLUMN_META を持つが、ここでも整数の
符号情報は含まれていない(ただ STRING なのか BLOB なのかは判別できる)。

そのため別途各カラム情報を元に MySQLValue に変換する必要がある。
-}

{- |

WARNING
It is users responssiblity to pass proper BinLogValue and DataType.

 * TimeZone is used to convert `timestamp`'s to LocalTime. It could
   be your prefered value, or MySQLに合わせるなら `time_zone` 変数か
   ら取得するべき

MySQLDecimal !Scientific
MySQLInt8U !Word8
MySQLInt8 !Int8
MySQLInt16U !Word16
MySQLInt16 !Int16
MySQLInt32U !Word32
MySQLInt32 !Int32
MySQLInt64U !Word64
MySQLInt64 !Int64
MySQLFloat !Float
MySQLDouble !Double
MySQLYear !Word16
MySQLDateTime !LocalTime
MySQLTimeStamp !LocalTime
MySQLDate !Day
MySQLTime !Word8 !TimeOfDay
MySQLGeometry !ByteString
MySQLBytes !ByteString
MySQLBit !Word64
MySQLText !Text
MySQLNull
-}
binLogValueToMySQLValue :: TimeZone -> (BinLogValue, DataType) -> MySQLValue
binLogValueToMySQLValue tz = \case
    (BinLogTiny i8, TinyInt _ s _) ->
        signCase s (MySQLInt8 i8) (MySQLInt8U (convI8toW8 i8))
    (BinLogShort i16, SmallInt _ s _) ->
        signCase s (MySQLInt16 i16) (MySQLInt16U (convI16toW16 i16))
    -- MediumInt は MySQLValue は MySQLInt32/MySQLInt32U を使っている
    -- ??? why
    (BinLogInt24 _, _) ->
        notImplemented
    (BinLogLong i32, Int _ s _) ->
        signCase s (MySQLInt32 i32) (MySQLInt32U (convI32toW32 i32))
    (BinLogLongLong i64, BigInt _ s _) ->
        signCase s (MySQLInt64 i64) (MySQLInt64U (convI64toW64 i64))
    -- FLOAT/DOUBLE型には符号(sign)情報を持つが、UNSIGNEDの場合でもデータ形式は同じ
    (BinLogFloat f, Float _ _ _) ->
        MySQLFloat f
    (BinLogDouble d, Double _ _ _) ->
        MySQLDouble d
    (BinLogBit b, Bit _) ->
        MySQLBit b
    -- a utc timestamp, note 0 doesn't mean 1970-01-01 00:00:00, because
    -- mysql choose 0 to present '0000-00-00 00:00:00'
    -- TODO: LocalTime では '0000-00-00 00:00:00' というものは表現できないのでは...
    -- msyql-haskell内の実装を見たら何か分かるかも。
    (BinLogTimeStamp sec, TimeStamp _) ->
        if sec == 0
            then undefined
            else MySQLTimeStamp $ mkLocaTimeFromTimeStamp tz $ fromIntegral sec
    -- like BinLogTimeStamp with an addtional microseconds field.
    (BinLogTimeStamp2 sec microSec, TimeStamp _) ->
        if sec == 0
            then undefined
            else MySQLTimeStamp $ mkLocaTimeFromTimeStamp tz $ secAddMicroSec sec microSec
    -- YYYY MM DD hh mm ss
    (BinLogDateTime y mo d h m s, DateTime _) ->
        MySQLDateTime $ mkLocalTime y mo d h m s (0 :: Int)
    -- YYYY MM DD hh mm ss microsecond
    (BinLogDateTime2 y mo d h m s ms, DateTime _) ->
        MySQLDateTime $ mkLocalTime y mo d h m s ms
    -- YYYY MM DD
    (BinLogDate y mo d, Date) ->
        MySQLDate $ mkDay y mo d
    -- sign(1= non-negative, 0= negative) hh mm ss
    -- MySQLTime !Word8 !TimeOfDay -- sign(0 = non-negative, 1 = negative) hh mm ss microsecond The sign is OPPOSITE to binlog one !!!
    (BinLogTime w8 h m s, Time _) ->
        MySQLTime (1 `xor` w8) $ mkTimeOfDay h m s (0 :: Int)
    -- sign(1= non-negative, 0= negative) hh mm ss microsecond
    (BinLogTime2 w8 h m s ms, Time _) ->
        MySQLTime (1 `xor` w8) $ mkTimeOfDay h m s ms
    (BinLogYear w16, Year) ->
        MySQLYear w16
    (BinLogNewDecimal sci, Decimal _ _ _) ->
        MySQLDecimal sci
    (BinLogEnum _, _) ->
        notImplemented
    (BinLogSet _, _) ->
        notImplemented
    (BinLogBytes bs, t)
        | isTextType t -> MySQLText (decodeUtf8Lenient bs)
        | isBlobType t -> MySQLBytes bs
    (BinLogGeometry bs, Geometry) ->
        MySQLGeometry bs
    (BinLogNull, _) ->
        MySQLNull
    (binLogValue, dataType) -> error $ show binLogValue <> ", " <> show dataType
  where
    signCase s a b = case s of
        Signed -> a
        UnSigned -> b

    mkLocaTimeFromTimeStamp timeZone posixTime =
        utcToLocalTime timeZone $ posixSecondsToUTCTime posixTime

    mkLocalTime y mo d h mi s ms =
        LocalTime (mkDay y mo d) (mkTimeOfDay h mi s ms)

    mkTimeOfDay h m s ms =
        TimeOfDay (fromIntegral h) (fromIntegral m) (secAddMicroSec s ms)

    mkDay y mo d =
        fromGregorian (fromIntegral y) (fromIntegral mo) (fromIntegral d)

    secAddMicroSec sec microSec =
        fromRational (toRational sec + toRational microSec / 1000000)

{- | バイナリレベルのレイアウトは保ちつつsigned整数をunsigned整数として解釈しなおす。

現状シリアラズライブラリ(store)を使って、一旦bytestringに変換して再度
読み直すということをしている。もっと効率的な方法が恐らく存在するはずだ
が、取りあえずの実装としては十分かな。
-}
convI8toW8 :: Int8 -> Word8
convI8toW8 = unsafeConv'

convI16toW16 :: Int16 -> Word16
convI16toW16 = unsafeConv'

convI32toW32 :: Int32 -> Word32
convI32toW32 = unsafeConv'

convI64toW64 :: Int64 -> Word64
convI64toW64 = unsafeConv'

-- 正の場合はそのまま変換しても問題ないはず
-- 負の場合は解釈しなおす必要あり
unsafeConv' :: (Integral a, Integral b) => a -> b
unsafeConv' a =
    let i = toInteger a
     in if i < 0 then undefined else fromIntegral i

notImplemented :: a
notImplemented = undefined
