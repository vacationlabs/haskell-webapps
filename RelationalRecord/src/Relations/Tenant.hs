{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Relations.Tenant where

import  Types.Tenant as Tenant
import  Types.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)


allTenants :: Relation () Tenants
allTenants = Tenant.tenants

getTenant :: Relation Int32 Tenants
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
    , iBOD          :: String
    }

$(makeRecordPersistableDefault ''TenantInsert)

insertTenant :: Insert TenantInsert
insertTenant = derivedInsert piTenant

piTenant :: Pi Tenants TenantInsert
piTenant = TenantInsert
    |$| Tenant.name'
    |*| Tenant.firstName'
    |*| Tenant.lastName'
    |*| Tenant.phone'
    |*| Tenant.email'
    |*| Tenant.backofficeDomain'


updateTenant :: TimestampedUpdate Int32
updateTenant = derivedUpdate $ \projection -> do
    (phTStamp, _)   <- placeholder (\tStamp -> Tenant.updatedAt' <-# tStamp)
    (phTenId, _)    <- placeholder (\tenId -> wheres $ projection ! Tenant.id' .=. tenId)
    return          $ phTStamp >< phTenId
