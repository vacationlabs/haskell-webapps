{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module  Relations.Tenant where

import  Types.Tenant as Tenant
import  Types.DB
import  Relations.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)

import  Data.Int                    (Int32)
import  GHC.Generics                (Generic)
import  Data.Default



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
    { uName         :: VariadicArg String
    , uFirstName    :: VariadicArg String
    , uLastName     :: VariadicArg String
    , uPhone        :: VariadicArg String
    , uEmail        :: VariadicArg String
    , uBOD          :: VariadicArg String
    , uStatus       :: VariadicArg Int32
    , uOwnerId      :: VariadicArg (Maybe Int32)
    }
    deriving (Generic, Default)

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
