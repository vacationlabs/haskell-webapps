{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      :  DBInterface
Copyright   :  (c) VacationLabs
Maintainer  :  michaelkarg77@gmail.com

This module contains the DB CRUD interface; more specifically different
actions with run different types of relations. Also, it provides the
interface to the audit log.
-}

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

import  Database.HDBC                   (SqlValue, handleSql, commit, rollback)

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
    PKey            -- tenantId
    (Maybe PKey)    -- userId
    Bool            -- changedBySystem
    PKey            -- auditableId
    String          -- auditableTableName
    String          -- Summary
    -- ByteString   -- TODO changes
$(makeRecordPersistableDefault ''AuditLogInsert)

piAuditLog :: Pi AuditLogs AuditLogInsert
piAuditLog = AuditLogInsert
    |$| AuditLog.tenantId'
    |*| AuditLog.userId'
    |*| AuditLog.changedBySystem'
    |*| AuditLog.auditableId'
    |*| AuditLog.auditableTableName'
    |*| AuditLog.summary'

insertLogEntry :: Insert AuditLogInsert
insertLogEntry = derivedInsert piAuditLog



dbQuery :: (FromSql SqlValue a, ToSql SqlValue p)
       => DBConnector -> Relation p a -> p -> IO (DBResult a)
dbQuery (DBConnector _ _ conn) rel param =
    handle (return ()) $
        runQuery conn (relationalQuery rel) param >>= return . \case
            []  -> ResEmpty
            [r] -> ResJust r
            rs  -> ResMany rs


dbDelete :: (ToSql SqlValue p)
     => DBConnector -> Delete p -> p -> IO (DBResult ())
dbDelete (DBConnector _ _ conn) dlt param =
    handle (rollback conn) $ do
        num     <- runDelete conn dlt param
        commit  conn
                                                                                -- TODO auditLog entry for delete action?
        return  $ if num > 0 then ResPKId 0 else ResPKId (-1)                   -- TODO which return codes does DomainAPI want/need?


dbInsert :: (ToSql SqlValue p)
    => DBConnector -> String -> Insert p -> p -> IO (DBResult ())
dbInsert DBConnector {dbTenantId = Nothing} _ _ _ =
    return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to insert into the DB"
dbInsert (DBConnector (Just tenantPK) mUserId conn) tName ins param =
    handle (rollback conn) $ do
        _       <- runInsert conn ins param
        newId   <- lastInsertedPK conn
        let
            logEntry = AuditLogInsert tenantPK mUserId (isNothing mUserId) newId tName (show ins)
        _       <- runInsert conn insertLogEntry logEntry
        commit  conn
        return  $ ResPKId newId


dbUpdate :: DBConnector -> String -> TimestampedUpdate -> PKey -> IO (DBResult ())
dbUpdate DBConnector {dbTenantId = Nothing} _ _ _ =
    return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to update the DB"
dbUpdate (DBConnector (Just tenantPK) mUserId conn) tName upd k =
    handle (rollback conn) $ do
        tNow    <- getZonedTime
        _       <- runUpdate conn upd (tNow, k)
        let
            logEntry = AuditLogInsert tenantPK mUserId (isNothing mUserId) k tName (show upd)
        _       <- runInsert conn insertLogEntry logEntry
        commit  conn
        return  $ ResPKId k
