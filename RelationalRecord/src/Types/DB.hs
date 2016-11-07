{-# LANGUAGE DeriveFoldable #-}

module  Types.DB
        ( HasAuditMetadata(..)
        , DBResult(..)
        , TimestampedUpdate
        , module Data.Int
        ) where


import  Data.Int

import  Data.Time.LocalTime         (ZonedTime)
import  Database.HDBC               (SqlError)
import  Database.Relational.Query   (Update)


-- TODO get table name from somewhere else out of HRR?
-- check: https://hackage.haskell.org/package/relational-query-0.8.3.2/docs/Database-Relational-Query-Table.html
class HasAuditMetadata a where
    getAuditMetadata :: a -> (Int32, String)


data DBResult a
    = ResEmpty
    | ResJust   a
    | ResMany   [a]
    | ResDBErr  SqlError
    deriving (Show, Eq, Foldable)


type TimestampedUpdate a = Update (ZonedTime, a)
