{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module  Relations.Tenant where

import  Types.Tenant as Tenant
import  Types.DB
import  Relations.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)

import  Data.Int (Int32)


-- SELECTS

allTenants :: Relation () Tenants
allTenants = Tenant.tenants

getTenant :: Relation PKey Tenants
getTenant = relation' . placeholder $ \tenId -> do
    a       <- query tenants
    wheres  $ a ! Tenant.id' .=. tenId
    return  a


-- INSERTS

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


-- UPDATES

data TenantUpdate = TenantUpdate
    { uName         :: Maybe String
    , uFirstName    :: Maybe String
    , uLastName     :: Maybe String
    , uPhone        :: Maybe String
    , uEmail        :: Maybe String
    , uBOD          :: Maybe String
    , uStatus       :: Maybe Int32
    , uOwnerId      :: Maybe (Maybe Int32)
    }

tenantUpdate :: TenantUpdate
tenantUpdate = TenantUpdate
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

updateTenantVariadic :: TenantUpdate -> TimestampedUpdate
updateTenantVariadic TenantUpdate {..} = derivedUpdate $ \projection -> do
    Tenant.name'        <-#? uName
    Tenant.firstName'   <-#? uFirstName
    Tenant.lastName'    <-#? uLastName
    Tenant.phone'       <-#? uPhone
    Tenant.email'       <-#? uEmail
    Tenant.backofficeDomain' <-#? uBOD
    Tenant.status'      <-#? uStatus
    Tenant.ownerId'     <-#? uOwnerId

    (phTStamp, _)   <- placeholder (\tStamp -> Tenant.updatedAt' <-# tStamp)
    (phTenId, _)    <- placeholder (\tenId -> wheres $ projection ! Tenant.id' .=. tenId)
    return          $ phTStamp >< phTenId


-- DELETES
