{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

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

import  Database.HDBC                   (SqlValue, handleSql, throwSqlError, commit, rollback)

import  Database.Relational.Query       hiding (isNothing)

import  Database.Record

import  Database.HDBC.Record.Query      (runQuery)
import  Database.HDBC.Record.Delete     (runDelete)
import  Database.HDBC.Record.Update     (runUpdate)
import  Database.HDBC.Record.Insert     (runInsert)
import  Database.HDBC.Query.TH          (makeRecordPersistableDefault)

import  Data.Aeson                      (encode, ToJSON(..))
import  Data.Time.LocalTime             (getZonedTime)
import  Data.Maybe
import  Data.Text                       (pack)


-- the information a DB action needs to create an audit log entry
-- NB the retrieval action parameter is a workaround for HRR's missing feature
--      INSERT INTO ... RETURNING *;
--      (cf. https://github.com/khibino/haskell-relational-record/issues/44)
--      sadly meaning, we need to make an extra trip to the DB.
data AuditInfo a = AuditInfo
    Text            -- table name
    Text            -- summary
    (Maybe (DBConnector -> PKey -> IO (DBUniqueResult a)))                      -- db action to retrieve a record


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


dbDelete :: (ToSql SqlValue p)
     => DBConnector -> AuditInfo () -> Delete p -> p -> IO (DBResult ())
dbDelete DBConnector {dbTenantId = Nothing} _ _ _ =
    return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to delete from the DB"
dbDelete (DBConnector (Just tenantPK) mUserId conn) (AuditInfo tName summ _) dlt param =
    handle (rollback conn) $ do
        num <- runDelete conn dlt param
        res <- if num > 0
            then
                let logEntry = AuditLogInsert tenantPK mUserId (isNothing mUserId) (-1) tName summ "{}"
                in runInsert conn insertLogEntry logEntry >> return (ResPKId 0)
            else return ResEmpty
        commit conn
        return res                                                              -- TODO which return codes does DomainAPI want/need for deletes?


dbInsert :: (ToSql SqlValue p, ToJSON a)
    => DBConnector -> AuditInfo a -> Insert p -> p -> IO (DBResult a)
dbInsert DBConnector {dbTenantId = Nothing} _ _ _ =
    return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to insert into the DB"
dbInsert dbc@(DBConnector (Just tenantPK) mUserId conn) (AuditInfo tName summ mGet) ins param =
    handle (rollback conn) $ runInsert conn ins param >> case mGet of
        Nothing ->
            let
                logEntry = AuditLogInsert
                    tenantPK mUserId (isNothing mUserId) (-1) tName summ "{}"
            in runInsert conn insertLogEntry logEntry
                >> commit conn
                >> return ResEmpty
        Just get -> do
            newId <- lastInsertedPK conn
            get dbc newId >>= \case
                Left _ -> throwSqlError $ mkDBErr "dbInsert: couldn't read inserted row"
                Right newRec ->
                    let
                        diff = asText $ encode $ removeTimestamps $ toJSON newRec
                        logEntry = AuditLogInsert
                            tenantPK mUserId (isNothing mUserId) newId tName summ diff
                    in runInsert conn insertLogEntry logEntry
                        >> commit conn
                        >> return (ResJust newRec)


dbUpdate :: ToJSON a => DBConnector -> AuditInfo a -> TimestampedUpdate -> PKey -> IO (DBResult a)
dbUpdate DBConnector {dbTenantId = Nothing} _ _ _ =
    return $ ResDBErr $ mkDBErr "connection needs a tenant id to be allowed to update the DB"
dbUpdate dbc@(DBConnector (Just tenantPK) mUserId conn) (AuditInfo tName summ ~(Just get)) upd k =
    handle (rollback conn) $ do
        ~(Right oldRec) <- get dbc k
        tNow    <- getZonedTime
        _       <- runUpdate conn upd (tNow, k)
        ~(Right newRec)  <- get dbc k
        let
            diff = asText $ jsonDiff oldRec newRec
            logEntry = AuditLogInsert
                tenantPK mUserId (isNothing mUserId) k tName summ diff
        _       <- runInsert conn insertLogEntry logEntry
        commit  conn
        return  $ ResJust newRec


handle :: IO () -> IO (DBResult a) -> IO (DBResult a)
handle doRollback = handleSql $ \err ->
    doRollback >> return (ResDBErr err)
