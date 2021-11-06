{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Database.MySQL.Record (
    fromMySQLValueMap,
    FromMySQLValue (..),
    fromQueryResult,
    -- ToMySQLValue (..),
    -- executeUpdateDiff,
) where

import Data.Map qualified as M
import Data.Scientific (Scientific)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime)
import Database.MySQL.Base
import GHC.Generics (Generic (Rep))
import RIO
import Record.FromMap (FromMapError, GFromMap, fromMap)
import System.IO.Streams (InputStream)
import System.IO.Streams.List qualified as IOS
import Prelude ()

-- * from

fromQueryResult ::
    ( Generic a
    , GFromMap FromMySQLValue MySQLValue (Rep a)
    ) =>
    ([ColumnDef], InputStream [MySQLValue]) ->
    IO (Either (FromMapError MySQLValue) [a])
fromQueryResult (defs, inputs) = do
    rs <- IOS.toList inputs
    let names = map (decodeUtf8Lenient . columnName) defs
    let toMap = M.fromList . zip names
    pure $ sequence $ map (fromMySQLValueMap . toMap) rs

{- | Map Text MySQLValue' の値をGenericなレコードに変換する。

 レコードの各フィールドの型は FromMySQLValue'のインスタンスである必要がある。
 キーはフィールド名と一致する必要がある。
 レコードのあるフィールド名がマップに含まれていない場合は FromMapError が返る。
 逆に余計なものがマップに入っていたとしても単に無視される。
-}
fromMySQLValueMap ::
    (Generic a, GFromMap FromMySQLValue MySQLValue (Rep a)) =>
    Map Text MySQLValue ->
    Either (FromMapError MySQLValue) a
fromMySQLValueMap = fromMap @FromMySQLValue fromMySQLValue

{- | MySQLValue' からある型へ自然に変換。

 Record.FromMap に渡す制約が主目的の型クラスだが、他のコンテキストで使っても問題ない。
-}
class FromMySQLValue a where
    fromMySQLValue :: MySQLValue -> Either MySQLValue a

instance FromMySQLValue Int where
    fromMySQLValue (MySQLInt64 i) = Right $ fromIntegral i
    fromMySQLValue (MySQLInt32 i) = Right $ fromIntegral i
    fromMySQLValue (MySQLInt8 i) = Right $ fromIntegral i
    fromMySQLValue v = Left v

instance FromMySQLValue Integer where
    fromMySQLValue (MySQLInt64U i) = Right $ fromIntegral i
    fromMySQLValue (MySQLInt64 i) = Right $ fromIntegral i
    fromMySQLValue (MySQLInt32 i) = Right $ fromIntegral i
    fromMySQLValue (MySQLInt8 i) = Right $ fromIntegral i
    fromMySQLValue v = Left v

instance FromMySQLValue Bool where
    fromMySQLValue (MySQLInt8 i) = Right $ i > 0
    fromMySQLValue v = Left v

instance FromMySQLValue Float where
    fromMySQLValue (MySQLFloat f) = Right f
    fromMySQLValue v = Left v

instance FromMySQLValue Text where
    fromMySQLValue (MySQLText t) = Right t
    fromMySQLValue v = Left v

-- NULL の可能性があるものは Maybe 型にしておく必要がある
instance FromMySQLValue a => FromMySQLValue (Maybe a) where
    fromMySQLValue MySQLNull = Right $ Nothing
    fromMySQLValue v = Just <$> fromMySQLValue v

instance FromMySQLValue Day where
    fromMySQLValue (MySQLDate t) = Right t
    fromMySQLValue v = Left v

instance FromMySQLValue LocalTime where
    fromMySQLValue (MySQLDateTime t) = Right t
    fromMySQLValue v = Left v

instance FromMySQLValue Scientific where
    fromMySQLValue (MySQLDecimal d) = Right $ d
    fromMySQLValue v = Left v

-- -- * to

-- genUpdateQueryAndParams
--   :: Text
--   -> Text
--   -> MySQLValue
--   -> Map Text MySQLValue
--   -> (Query, [MySQLValue])
-- genUpdateQueryAndParams table primaryKey id updates = (query', vals <> [id])
--   where
--     keys = map fst $ M.toList updates
--     vals = map snd $ M.toList updates
--     query' = fromString . toString $ T.intercalate " " [tableKu, setKu, whereKu]
--     tableKu = "UPDATE " <> quote table
--     setKu   = "SET " <> T.intercalate ", " (map (\k -> quote k <> " = ?") keys)
--     whereKu = "WHERE " <> quote primaryKey <> " = ?"
--     quote t = "`" <> t <> "`"

-- -- 現状雑に取りあえず作った
-- executeUpdateDiff
--   :: (Generic a, GToMap ToMySQLValue MySQLValue (Rep a), MonadIO m)
--   => MySQLConn
--   -> Text
--   -> Text
--   -> a
--   -> a
--   -> (Map Text MySQLValue -> Map Text MySQLValue)
--   -> m ()
-- executeUpdateDiff conn table primaryKey old new modifier = do
--   let om = toMySQLValueMap old
--   let nm = toMySQLValueMap new
--   id' <- M.lookup primaryKey om & throwMaybe "No id in old"
--   id  <- M.lookup primaryKey nm & throwMaybe "No id in new"
--   when (id' /= id) $ throwString "id's not equal"
--   let updates = modifier
--         $ M.differenceWith (\a b -> if a == b then Nothing else Just a) nm om
--   if M.null updates
--      then pure ()
--      else do
--        let (q, params) = genUpdateQueryAndParams table primaryKey id updates
--        void $ liftIO $ execute conn q params
--   where
--     throwMaybe str = fromEither . maybeToRight (stringException str)

-- toMySQLValueMap :: (_) => a -> Map Text MySQLValue
-- toMySQLValueMap = toMap @ToMySQLValue toMySQLValue

-- {-

-- 実際のmysql側の SQL型と合わない可能性がある。例えば 数字の 42 は int
-- でも bigint でも smallintでも格納できる。Haskell の型から MySQLValue
-- への変換時では情報は失われないが、サーバに送られた後での変換について
-- は保証できない。

-- -}
-- class ToMySQLValue a where
--   toMySQLValue :: a -> MySQLValue

-- instance ToMySQLValue Int where
--   toMySQLValue = MySQLInt64 . fromIntegral

-- instance ToMySQLValue Bool where
--   toMySQLValue = MySQLInt8 . bool 0 1

-- instance ToMySQLValue Text where
--   toMySQLValue = MySQLText

-- instance ToMySQLValue LocalTime where
--   toMySQLValue = MySQLDateTime

-- instance ToMySQLValue a => ToMySQLValue (Maybe a) where
--   toMySQLValue = maybe MySQLNull toMySQLValue
