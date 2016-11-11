{-# LANGUAGE LambdaCase, PatternGuards, TypeSynonymInstances #-}

{-|
Module      :  DataSource
Copyright   :  (c) VacationLabs
Maintainer  :  michaelkarg77@gmail.com

This module defines everything specific to a certain DB backend; in
this case PostgreSQL. Switching to another data source / DB like e.g.
MySQL should be as simple as adjusting the definitions found here.
-}

module  DataSource where

import  Database.HDBC.PostgreSQL    (connectPostgreSQL, Connection)
import  Database.HDBC               (IConnection, SqlError(..), quickQuery', safeFromSql, throwSqlError)
import  Data.Time.LocalTime         (ZonedTime)
import  Data.Int                    (Int32)


-- what Haskell type does the DB use for timestamps?
type DBTime = ZonedTime

-- what Haskell type does the DB use for its primary keys?
type PKey = Int32

-- common parameters of a SqlError that's not thrown by the DB, but
-- by the Haskell application layer
mkDBErr :: String -> SqlError
mkDBErr = SqlError "" (-1)

-- the database schema to access
schemaName :: String
schemaName = "public"

-- get the last inserted primary key
lastInsertedPK :: IConnection conn => conn -> IO PKey
lastInsertedPK conn =
    quickQuery' conn "SELECT LASTVAL();" [] >>= \case
        [[i]] | Right i' <- safeFromSql i -> return i'
        _ -> throwSqlError $ mkDBErr "could not acquire last inserted primary key"

-- action to acquire a connection to the data source
getDataSource :: IO Connection
getDataSource =
    connectPostgreSQL "dbname=vacationlabs"
