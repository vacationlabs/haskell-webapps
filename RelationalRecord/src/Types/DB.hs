{-# LANGUAGE DeriveFoldable, LambdaCase, GADTs #-}

{-|
Module      :  Types.DB
Copyright   :  (c) VacationLabs
Maintainer  :  michaelkarg77@gmail.com

Various DB-related helper types.
-}

module  Types.DB
        ( DBResult(..)
        , TimestampedUpdate
        , DBUniqueResult
        , DBConnector(..)
        , dbUniqueResult
        , module DataSource
        , module Data.Text
        ) where


import  DataSource                  (PKey, DBTime, mkDBErr)

import  Database.HDBC               (SqlError, IConnection)
import  Database.Relational.Query   (Update)
import  Data.Text                   (Text)



-- a database connector that can carry tenant and/or user identity with it
data DBConnector where
    DBConnector :: IConnection conn =>
        { dbTenantId    :: Maybe PKey
        , dbUserId      :: Maybe PKey
        , dbConn        :: conn
        } -> DBConnector


data DBResult a
    = ResEmpty
    | ResJust   a
    | ResMany   [a]
    | ResPKId   PKey
    | ResDBErr  SqlError
    deriving (Show, Eq, Foldable)


type DBUniqueResult = Either SqlError

dbUniqueResult :: DBResult a -> DBUniqueResult a
dbUniqueResult = \case
    ResJust v       -> Right v
    ResDBErr err    -> Left err
    _               -> Left $ mkDBErr "expected exactly one query result"


-- type TimestampedUpdate a b = Update ((a, DBTime), b)

type TimestampedUpdate = Update (DBTime, PKey)
