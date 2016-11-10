{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module  DBInterface
        ( dbQuery
        , dbInsert
        , dbUpdate
        , dbDelete
        , module Types.DB
        ) where

import  Types.AuditLog                  as AuditLog
import  Types.DB
import  DataSource

import  Database.HDBC                   (IConnection, SqlValue, handleSql, commit, rollback)

import  Database.Relational.Query

import  Database.Record

import  Database.HDBC.Record.Query      (runQuery)
import  Database.HDBC.Record.Delete     (runDelete)
import  Database.HDBC.Record.Update     (runUpdate)
import  Database.HDBC.Record.Insert     (runInsert)
import  Database.HDBC.Query.TH          (makeRecordPersistableDefault)

import  Data.Time.LocalTime             (getZonedTime)



handle :: IO () -> IO (DBResult a) -> IO (DBResult a)
handle doRollback = handleSql $ \err ->
    doRollback >> return (ResDBErr err)


data AuditLogInsert = AuditLogInsert
    { iTenantId             :: PKey
    , iUserid               :: Maybe PKey
    , iAuditableId          :: PKey
    , iAutditableTableName  :: String
    -- TODO
    }
$(makeRecordPersistableDefault ''AuditLogInsert)

piAuditLog :: Pi AuditLogs AuditLogInsert
piAuditLog = AuditLogInsert
    |$| AuditLog.tenantId'
    |*| AuditLog.userId'
    |*| AuditLog.auditableId'
    |*| AuditLog.auditableTableName'

insertLogEntry :: Insert AuditLogInsert
insertLogEntry = derivedInsert piAuditLog


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
    commit  conn
    return  $ if num > 0 then ResJust num else ResEmpty

dbInsert :: (IConnection conn, ToSql SqlValue p, HasTableName (Insert p))
     => conn -> Insert p -> p -> IO (DBResult ())
dbInsert conn ins param = handle (rollback conn) $ do
    num     <- runInsert conn ins param
    newId   <- pgLastVal conn
    let logEntry = AuditLogInsert 1 Nothing newId (getTableName ins)
    -- _       <- runInsert conn insertLogEntry logEntry
    commit  conn
    return  $ if num > 0 then ResPKId newId else ResEmpty

dbUpdate :: (IConnection conn, ToSql SqlValue p)
     => conn -> TimestampedUpdate p PKey -> PKey -> p -> IO (DBResult Integer)
dbUpdate conn upd k param = handle (rollback conn) $ do
    tNow    <- getZonedTime
    res     <- runUpdate conn upd ((param, tNow), k)
    -- TODO create auditlog insert
    commit  conn
    return  $ ResJust res
