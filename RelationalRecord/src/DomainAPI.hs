{-# LANGUAGE OverloadedStrings, LambdaCase #-}

{-|
Module      :  DomainAPI
Copyright   :  (c) VacationLabs
Maintainer  :  michaelkarg77@gmail.com

This is the collection of domain API functions.
These functions are meant to be called from the outside world,
e.g. from a REST handler, a script or any kind of application.

The domain API module is where the DBInterface and the relations
for each data type defined in the Relations/ namespace come together.
-}

module  DomainAPI where

import  DataSource
import  DBInterface
import  Types.Tenant                as Tenant
import  Types.User                  as User
import  Types.Role                  as Role
import  Types.UsersRoles            as UsersRoles
import  Types.UserStatus            as UserStatus
import  Types.TenantStatus          as TenantStatus
import  Relations.Tenant            as Tenant
import  Relations.Role              as Role
import  Relations.User              as User

import  Control.Monad               ((>=>))



-- Role

createRole :: DBConnector -> RoleInsert -> IO (DBUniqueResult Roles)
createRole conn =
    dbInsert conn audit insertRole >=> \case
        ResPKId pkey    -> DomainAPI.getRole conn pkey
        ResDBErr err    -> return $ Left err
        _               -> return $ Left $ mkDBErr "unexpected DB result"
  where
    audit = AuditInfo Role.tableName "create a new role" Nothing

getRole :: DBConnector -> PKey -> IO (DBUniqueResult Roles)
getRole conn =
    dbQuery conn Role.getRole >=> return . dbUniqueResult

updateRole :: DBConnector -> Roles -> Roles -> IO (DBUniqueResult Roles)
updateRole conn old new =
    let
        upd     = roleVariadic old new
        pkey    = Role.id old
    in dbUpdate conn audit (updateRoleVariadic upd) pkey >>= return . \case
        ResDBErr err    -> Left err
        _               -> Right new
  where
    audit = AuditInfo Role.tableName "update a role" $ Just (old, new)

deleteRole :: DBConnector -> Either PKey Text -> IO (DBResult ())
deleteRole conn = \case
    Left pkey   -> dbDelete conn audit deleteRoleById pkey
    Right rName -> dbDelete conn audit deleteRoleByName rName
  where
    audit = AuditInfo Role.tableName "delete a role" Nothing

assignRole :: DBConnector -> Users -> Roles -> IO (DBResult ())
assignRole conn Users {User.id = uid} Roles {Role.id = rid} =
    dbInsert conn audit Role.assignRole (UsersRoles uid rid)
  where
    audit = AuditInfo UsersRoles.tableName "add a role assignment" Nothing

removeRole :: DBConnector -> Users -> Roles -> IO (DBResult ())
removeRole conn Users {User.id = uid} Roles {Role.id = rid} =
    dbDelete conn audit Role.removeRole (UsersRoles uid rid)
  where
    audit = AuditInfo UsersRoles.tableName "remove a role assignment" Nothing


-- User

createUser :: DBConnector -> UserInsert -> IO (DBUniqueResult Users)
createUser conn =
    dbInsert conn audit insertUser >=> \case
        ResPKId pkey    -> DomainAPI.getUser conn pkey
        ResDBErr err    -> return $ Left err
        _               -> return $ Left $ mkDBErr "unexpected DB result"
  where
    audit = AuditInfo User.tableName "create a new user" Nothing

getUser :: DBConnector -> PKey -> IO (DBUniqueResult Users)
getUser conn =
    dbQuery conn User.getUser >=> return . dbUniqueResult

activateUser :: DBConnector -> Users -> IO (DBUniqueResult Users)
activateUser conn u =
    updateUserInt "activate a user" conn u u {User.status = UserStatus.Active}

deactivateUser :: DBConnector -> Users -> IO (DBUniqueResult Users)
deactivateUser conn u =
    updateUserInt "deactivate a user" conn u u {User.status = UserStatus.Inactive}

updateUser :: DBConnector -> Users -> Users -> IO (DBUniqueResult Users)
updateUser = updateUserInt "update a user"

updateUserInt :: Text -> DBConnector -> Users -> Users -> IO (DBUniqueResult Users)
updateUserInt summary conn old new =
    let
        upd     = userVariadic old new
        pkey    = User.id old
    in dbUpdate conn audit (updateUserVariadic upd) pkey >>= return . \case
        ResDBErr err    -> Left err
        _               -> Right new
  where
    audit = AuditInfo User.tableName summary $ Just (old, new)


-- Tenant

createTenant :: DBConnector -> TenantInsert -> IO (DBUniqueResult Tenants)
createTenant conn =
    dbInsert conn audit insertTenant >=> \case
        ResPKId pkey    -> DomainAPI.getTenant conn pkey
        ResDBErr err    -> return $ Left err
        _               -> return $ Left $ mkDBErr "unexpected DB result"
  where
    audit = AuditInfo Role.tableName "create a new tenant" Nothing

getTenant :: DBConnector -> PKey -> IO (DBUniqueResult Tenants)
getTenant conn =
    dbQuery conn Tenant.getTenant >=> return . dbUniqueResult

activateTenant :: DBConnector -> Tenants -> IO (DBUniqueResult Tenants)
activateTenant conn t =
    updateTenantInt "activate a tenant" conn t t {Tenant.status = TenantStatus.Active}

deactivateTenant :: DBConnector -> Tenants -> IO (DBUniqueResult Tenants)
deactivateTenant conn t =
    updateTenantInt "deactivate a tenant" conn t t {Tenant.status = TenantStatus.Inactive}

updateTenant :: DBConnector -> Tenants -> Tenants -> IO (DBUniqueResult Tenants)
updateTenant = updateTenantInt "update a tenant"

updateTenantInt :: Text -> DBConnector -> Tenants -> Tenants -> IO (DBUniqueResult Tenants)
updateTenantInt summary conn old new =
    let
        upd     = tenantVariadic old new
        pkey    = Tenant.id old
    in dbUpdate conn audit (updateTenantVariadic upd) pkey >>= return . \case
        ResDBErr err    -> Left err
        _               -> Right new
  where
    audit = AuditInfo Tenant.tableName summary $ Just (old, new)
