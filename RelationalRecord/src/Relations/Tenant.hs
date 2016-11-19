{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module  Relations.Tenant where

import  Types.Tenant as Tenant
import  Types.DB
import  Relations.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH              (makeRecordPersistableDefault)

import  Data.Int                            (Int32)
import  GHC.Generics                        (Generic)
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

-- an insert constrained to the obligatory fields, thus enforcing
-- default values encoded in the DB schema for all other fields
data TenantInsert = TenantInsert
    { iName         :: Text
    , iFirstName    :: Text
    , iLastName     :: Text
    , iPhone        :: Text
    , iEmail        :: Text
    , iOwnerId      :: Maybe PKey
    , iBOD          :: Text
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
    { uName         :: VariadicArg Text
    , uFirstName    :: VariadicArg Text
    , uLastName     :: VariadicArg Text
    , uPhone        :: VariadicArg Text
    , uEmail        :: VariadicArg Text
    , uStatus       :: VariadicArg Int32
    , uOwnerId      :: VariadicArg (Maybe Int32)
    }
    deriving (Generic, Default)

tenantVariadic :: Tenants -> Tenants -> TenantUpdate
tenantVariadic old new = TenantUpdate
    (varArg name old new)
    (varArg firstName old new)
    (varArg lastName old new)
    (varArg phone old new)
    (varArg email old new)
    (varArg status old new)
    (varArg ownerId old new)

updateTenantVariadic :: TenantUpdate -> TimestampedUpdate
updateTenantVariadic TenantUpdate {..} = derivedUpdate $ \projection -> do
    Tenant.name'        <-#? uName
    Tenant.firstName'   <-#? uFirstName
    Tenant.lastName'    <-#? uLastName
    Tenant.phone'       <-#? uPhone
    Tenant.email'       <-#? uEmail
    Tenant.status'      <-#? uStatus
    Tenant.ownerId'     <-#? uOwnerId

    (phTStamp, _)   <- placeholder (\tStamp -> Tenant.updatedAt' <-# tStamp)
    (phTenId, _)    <- placeholder (\tenId -> wheres $ projection ! Tenant.id' .=. tenId)
    return          $ phTStamp >< phTenId


-- DELETES
