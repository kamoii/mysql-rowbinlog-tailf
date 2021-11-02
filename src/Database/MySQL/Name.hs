-- |

module Database.MySQL.Name where

import RIO

newtype SchemaName = SchemaName Text
    deriving (Eq, Ord, Show)

newtype TableName = TableName Text
    deriving (Eq, Ord, Show)

newtype ColumnName = ColumnName Text
    deriving (Eq, Ord, Show)
