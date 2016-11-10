{-# LANGUAGE DeriveFoldable, LambdaCase #-}

module  Types.DB
        ( HasTableName(..)
        , DBResult(..)
        , TimestampedUpdate
        , DBWriteResult
        , DBUniqueResult
        , dbWriteResult
        , dbUniqueResult
        , module DataSource
        ) where


import  DataSource                  (PKey)

import  Data.Time.LocalTime         (ZonedTime)
import  Database.HDBC               (SqlError(..))
import  Database.Relational.Query   (Update)


-- TODO get table name from somewhere else out of HRR?
-- check: https://hackage.haskell.org/package/relational-query-0.8.3.2/docs/Database-Relational-Query-Table.html
class HasTableName a where
    getTableName :: a -> String


data DBResult a
    = ResEmpty
    | ResJust   a
    | ResMany   [a]
    | ResPKId   PKey
    | ResDBErr  SqlError
    deriving (Show, Eq, Foldable)

type DBWriteResult  = Either SqlError PKey

type DBUniqueResult = Either SqlError

dbWriteResult :: DBResult a -> DBWriteResult
dbWriteResult = \case
    ResPKId k       -> Right k
    ResDBErr err    -> Left err
    _               -> Left $ SqlError "" (-1) ""

dbUniqueResult :: DBResult a -> Either SqlError a
dbUniqueResult = \case
    ResJust v       -> Right v
    ResDBErr err    -> Left err
    _               -> Left $ SqlError "" (-1) ""

type TimestampedUpdate a b = Update ((a, ZonedTime), b)
