{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Database.MySQL.Base (ConnectInfo (..))
import Database.MySQL.Connection (defaultConnectInfo)
import MySQLRowBinLogTailF.Main (run)
import Options.Generic (ParseRecord, getRecord)
import RIO
import Prelude ()

main :: IO ()
main = do
    opt <- getRecord "mysql-rowbinlog-tailf"
    let con = applyOpt opt defaultConnectInfo
    run con

data Option = Option
    { host :: Maybe String
    , port :: Maybe Int
    , user :: Maybe ByteString
    , password :: Maybe ByteString
    }
    deriving (Generic)

instance ParseRecord Option

applyOpt :: Option -> ConnectInfo -> ConnectInfo
applyOpt Option{..} con =
    con
        & applyIfJust host setHost
        & applyIfJust port setPort
        & applyIfJust user setUser
        & applyIfJust password setPassword
  where
    setHost host' ci = ci{ciHost = host'}
    setPort port' ci = ci{ciPort = fromIntegral port'}
    setUser user' ci = ci{ciUser = user'}
    setPassword password' ci = ci{ciPassword = password'}

    applyIfJust (Just v) f = f v
    applyIfJust Nothing _ = id
