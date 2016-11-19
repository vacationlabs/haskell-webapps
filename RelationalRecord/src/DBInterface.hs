{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, GADTs #-}

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
        , AuditInfo (..)
        , module Types.DB
        ) where

import  Types.AuditLog                  as AuditLog
import  Types.DB
import  DataSource
import  Helpers.JSONDiff

import  Database.HDBC                   (SqlValue, handleSql, commit, rollback)

import  Database.Relational.Query       hiding (isNothing)

import  Database.Record

import  Database.HDBC.Record.Query      (runQuery)
import  Database.HDBC.Record.Delete     (runDelete)
import  Database.HDBC.Record.Update     (runUpdate)
import  Database.HDBC.Record.Insert     (runInsert)
import  Database.HDBC.Query.TH          (makeRecordPersistableDefault)

import  Data.Aeson                      (ToJSON())
import  Data.Time.LocalTime             (getZonedTime)
import  Data.Maybe



data AuditInfo a where
    AuditInfo :: ToJSON a => Text -> Text -> Maybe (a, a) -> AuditInfo a


data AuditLogInsert = AuditLogInsert
    PKey            -- tenantId
    (Maybe PKey)    -- userId
    Bool            -- changedBySystem
    PKey            -- auditableId
    Text            -- auditableTableName
    Text            -- summary
    Text            -- changes
$(makeRecordPersistableDefault ''AuditLogInsert)

piAuditLog :: Pi AuditLogs AuditLogInsert
piAuditLog = AuditLogInsert
    |$| AuditLog.tenantId'
    |*| AuditLog.userId'
    |*| AuditLog.changedBySystem'
    |*| AuditLog.auditableId'
    |*| AuditLog.auditableTableName'
    |*| AuditLog.summary'
    |*| AuditLog.changes'

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


dbDelete :: ToSql SqlValue p
     => DBConnector -> AuditInfo () -> Delete p -> p -> IO (DBResult ())
dbDelete DBConnector {dbTenantId = Nothing} _ _ _ =
    return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to delete from the DB"
dbDelete (DBConnector (Just tenantPK) mUserId conn) (AuditInfo tName summ _) dlt param =
    handle (rollback conn) $ do
        num <- runDelete conn dlt param
        res <- if num > 0
            then do
                _ <- runInsert conn insertLogEntry $ AuditLogInsert
                        tenantPK mUserId (isNothing mUserId) (-1) tName summ "{}"
                return $ ResPKId 0
            else return ResEmpty
        commit conn
        return res


dbInsert :: ToSql SqlValue p
    => DBConnector -> AuditInfo () -> Insert p -> p -> IO (DBResult ())
dbInsert DBConnector {dbTenantId = Nothing} _ _ _ =
    return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to insert into the DB"
dbInsert (DBConnector (Just tenantPK) mUserId conn) (AuditInfo tName summ _) ins param =
    handle (rollback conn) $ do
        _       <- runInsert conn ins param
        newId   <- lastInsertedPK conn
        _       <- runInsert conn insertLogEntry $ AuditLogInsert
                    tenantPK mUserId (isNothing mUserId) newId tName summ "{}"
        commit  conn
        return  $ ResPKId newId


dbUpdate :: DBConnector -> AuditInfo a -> TimestampedUpdate -> PKey -> IO (DBResult ())
dbUpdate DBConnector {dbTenantId = Nothing} _ _ _ =
    return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to update the DB"
dbUpdate (DBConnector (Just tenantPK) mUserId conn) (AuditInfo tName summ mDiff) upd k =
    handle (rollback conn) $ do
        tNow    <- getZonedTime
        _       <- runUpdate conn upd (tNow, k)
        let dif = maybe "{}" (asText . uncurry jsonDiff) mDiff
        _       <- runInsert conn insertLogEntry $ AuditLogInsert
                    tenantPK mUserId (isNothing mUserId) k tName summ dif
        commit  conn
        return  $ ResEmpty


handle :: IO () -> IO (DBResult a) -> IO (DBResult a)
handle doRollback = handleSql $ \err ->
    doRollback >> return (ResDBErr err)
