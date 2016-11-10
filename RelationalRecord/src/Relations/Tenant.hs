{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module  Relations.Tenant where

import  Types.Tenant as Tenant
import  Types.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)

import  Database.Record.Persistable (PersistableWidth)
import  Database.Relational.Query.Monad.Trans.Assigning (Assignings)

import  Data.Int

allTenants :: Relation () Tenants
allTenants = Tenant.tenants

getTenant :: Relation PKey Tenants
getTenant = relation' . placeholder $ \tenId -> do
    a       <- query tenants
    wheres  $ a ! Tenant.id' .=. tenId
    return  a



data TenantInsert = TenantInsert
    { iName         :: String
    , iFirstName    :: String
    , iLastName     :: String
    , iPhone        :: String
    , iEmail        :: String
    , iOwnerId      :: Maybe PKey
    , iBOD          :: String
    }
$(makeRecordPersistableDefault ''TenantInsert)

piTenant :: Pi Tenants TenantInsert
piTenant = TenantInsert
    |$| Tenant.name'
    |*| Tenant.firstName'
    |*| Tenant.lastName'
    |*| Tenant.phone'
    |*| Tenant.email'
    |*| Tenant.ownerId'
    |*| Tenant.backofficeDomain'

insertTenant :: Insert TenantInsert
insertTenant = derivedInsert piTenant

instance HasTableName (Insert TenantInsert) where
    getTableName = const Tenant.tableName
{-
data TenantUpdate = TenantUpdate
    { uName         :: String
    , uFirstName    :: Maybe String
    , uLastName     :: Maybe String
    , uPhone        :: Maybe String
    , uEmail        :: Maybe String
    , uBOD          :: Maybe String
    , uStatus       :: Maybe Int32
    , uOwnerId      :: Maybe (Maybe Int32)
    }
-}

updName :: Monad m => Projection Flat String -> Assignings Tenants m ()
updName = (Tenant.name' <-#)

updStatus :: Monad m => Projection Flat Int32 -> Assignings Tenants m ()
updStatus = (Tenant.status' <-#)

updateTenant :: (PersistableWidth b, SqlProjectable p)
    => (p b -> Assignings Tenants Restrict a) -> TimestampedUpdate b PKey
updateTenant assigning = derivedUpdate $ \projection -> do
    (phColumns, _)  <- placeholder assigning
    (phTStamp, _)   <- placeholder (\tStamp -> Tenant.updatedAt' <-# tStamp)
    (phTenId, _)    <- placeholder (\tenId -> wheres $ projection ! Tenant.id' .=. tenId)
    return          $ phColumns >< phTStamp >< phTenId
