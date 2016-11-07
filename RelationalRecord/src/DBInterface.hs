{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module  DBInterface
        ( dbQuery
        , dbInsert
        , dbUpdate
        , dbDelete
        ) where

import  Types.AuditLogs     as AuditLog
import  Types.DB

import Database.HDBC        (IConnection, SqlValue, handleSql, commit, rollback)

import Database.Relational.Query

import Database.Record

import Database.HDBC.Record.Query   (runQuery)
import Database.HDBC.Record.Delete  (runDelete)
import Database.HDBC.Record.Update  (runUpdate)
import Database.HDBC.Record.Insert  (runInsert)
import Data.Time.LocalTime          (getZonedTime)



handle :: IO () -> IO (DBResult a) -> IO (DBResult a)
handle doRollback = handleSql $ \err ->
    doRollback >> return (ResDBErr err)


dbQuery :: (IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
       => conn -> Relation p a -> p -> IO (DBResult a)
dbQuery conn rel param = handle (return ()) $
    runQuery conn (relationalQuery rel) param >>= return . \case
        []  -> ResEmpty
        [r] -> ResJust r
        rs  -> ResMany rs

dbDelete :: (IConnection conn, ToSql SqlValue p)
     => conn -> Delete p -> p -> IO (DBResult Integer)
dbDelete conn dlt param = handle (rollback conn) $ do
    num     <- runDelete conn dlt param
    -- TODO create auditlog insert
    commit  conn
    return  $ if num > 0 then ResJust num else ResEmpty

dbInsert :: (IConnection conn, ToSql SqlValue p)
     => conn -> Insert p -> p -> IO (DBResult Integer)
dbInsert conn ins param = handle (rollback conn) $ do
    num     <- runInsert conn ins param
    -- TODO create auditlog insert
    commit  conn
    return  $ if num > 0 then ResJust num else ResEmpty

dbUpdate :: (IConnection conn, ToSql SqlValue p)
     => conn -> TimestampedUpdate p -> p -> IO (DBResult Integer)
dbUpdate conn upd param = handle (rollback conn) $ do
    tNow    <- getZonedTime
    res     <- runUpdate conn upd (tNow, param)
    -- TODO create auditlog insert
    commit  conn
    return  $ ResJust res


-- AuditLogs

-- TODO
dbLog :: InsertQuery ()
dbLog = undefined
