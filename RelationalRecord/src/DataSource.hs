{-# LANGUAGE LambdaCase, PatternGuards #-}

module  DataSource where

import  Database.HDBC.PostgreSQL    (connectPostgreSQL, Connection)
import  Database.HDBC               (IConnection, SqlError(..), quickQuery', safeFromSql, throwSqlError)
import  Data.Int                    (Int32)


type PKey = Int32

pgSchemaName :: String
pgSchemaName = "public"

pgLastVal :: IConnection conn => conn -> IO PKey
pgLastVal conn =
    quickQuery' conn "SELECT LASTVAL();" [] >>= \case
        [[i]] | Right i' <- safeFromSql i   -> return i'
        _                                   -> throwSqlError (SqlError "" (-1) "")

getDataSource :: IO Connection
getDataSource =
    connectPostgreSQL "dbname=vacationlabs"
