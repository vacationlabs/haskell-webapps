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

import  Database.Relational.Query       hiding (isNothing)

import  Database.Record

import  Database.HDBC.Record.Query      (runQuery)
import  Database.HDBC.Record.Delete     (runDelete)
import  Database.HDBC.Record.Update     (runUpdate)
import  Database.HDBC.Record.Insert     (runInsert)
import  Database.HDBC.Query.TH          (makeRecordPersistableDefault)

import  Data.Time.LocalTime             (getZonedTime)
import  Data.Maybe



handle :: IO () -> IO (DBResult a) -> IO (DBResult a)
handle doRollback = handleSql $ \err ->
    doRollback >> return (ResDBErr err)


data AuditLogInsert = AuditLogInsert
    { iTenantId             :: PKey
    , iUserid               :: Maybe PKey
    , iChangedBySystem      :: Bool
    , iAuditableId          :: PKey
    , iAutditableTableName  :: String
    -- TODO
    }
$(makeRecordPersistableDefault ''AuditLogInsert)

piAuditLog :: Pi AuditLogs AuditLogInsert
piAuditLog = AuditLogInsert
    |$| AuditLog.tenantId'
    |*| AuditLog.userId'
    |*| AuditLog.changedBySystem'
    |*| AuditLog.auditableId'
    |*| AuditLog.auditableTableName'

insertLogEntry :: Insert AuditLogInsert
insertLogEntry = derivedInsert piAuditLog


dbQuery :: (FromSql SqlValue a, ToSql SqlValue p)
       => DBConnector -> Relation p a -> p -> IO (DBResult a)
dbQuery (DBConnector _ _ conn) rel param = handle (return ()) $
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

dbInsert :: (ToSql SqlValue p, HasTableName (Insert p))
     => DBConnector -> Insert p -> p -> IO (DBResult ())
dbInsert DBConnector {dbTenantId = Nothing} _ _ = return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to insert into the DB"
dbInsert (DBConnector (Just tenantPK) mUserId conn) ins param = handle (rollback conn) $ do
    num     <- runInsert conn ins param
    newId   <- pgLastVal conn
    let
        logEntry = AuditLogInsert tenantPK mUserId (isNothing mUserId) newId (getTableName ins)
    -- _       <- runInsert conn insertLogEntry logEntry
    commit  conn
    return  $ ResPKId newId

dbUpdate :: (ToSql SqlValue p)
     => DBConnector -> TimestampedUpdate p PKey -> PKey -> p -> IO (DBResult ())
dbUpdate DBConnector {dbTenantId = Nothing} _ _ _ = return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to update the DB"
dbUpdate (DBConnector (Just tenantPK) mUserId conn) upd k param = handle (rollback conn) $ do
    tNow    <- getZonedTime
    res     <- runUpdate conn upd ((param, tNow), k)
    let
        logEntry = AuditLogInsert tenantPK mUserId (isNothing mUserId) k ""     -- TODO (getTableName upd)
    -- _       <- runInsert conn insertLogEntry logEntry
    commit  conn
    return  $ ResPKId k
