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
import  Types.Tenant                as Tenant (Tenants, tableName)
import  Types.User                  as User (Users, tableName)
import  Types.Role                  as Role (Roles, tableName)
import  Types.UsersRoles            as UsersRoles (tableName)
import  Relations.Tenant            as Tenant
import  Relations.Role              as Role
import  Relations.User              as User
import  Relations.DB

import  Control.Monad               ((>=>))
import  Data.Default                (def)
import  Data.Maybe                  (fromMaybe)



-- Role

createRole :: DBConnector -> RoleInsert -> IO (DBWriteResult Roles)
createRole conn =
    dbInsert conn audit insertRole >=> return . dbWriteResult
  where
    audit = AuditInfo Role.tableName "create a new role" (Just DomainAPI.getRole)

getRole :: DBConnector -> PKey -> IO (DBUniqueResult Roles)
getRole conn =
    dbQuery conn Role.getRole >=> return . dbUniqueResult

updateRole :: DBConnector -> PKey -> Maybe Text -> RoleUpdate -> IO (DBWriteResult Roles)
updateRole conn pkey mSummary upd =
    dbWriteResult <$> dbUpdate conn audit (updateRoleVariadic upd) pkey
  where
    audit = AuditInfo Role.tableName (fromMaybe "update a role" mSummary) (Just DomainAPI.getRole)

deleteRole :: DBConnector -> Either PKey Text -> IO (DBResult ())
deleteRole conn = \case
    Left pkey   -> dbDelete conn audit deleteRoleById pkey
    Right name  -> dbDelete conn audit deleteRoleByName name
  where
    audit = AuditInfo Role.tableName "delet a role" Nothing

assignRole :: DBConnector -> RoleAssignment -> IO (DBResult ())
assignRole conn =
    dbInsert conn audit Role.assignRole
  where
    audit :: AuditInfo ()
    audit = AuditInfo UsersRoles.tableName "add a role assignment" Nothing

removeRole :: DBConnector -> RoleAssignment -> IO (DBResult ())
removeRole conn =
    dbDelete conn audit Role.removeRole
  where
    audit = AuditInfo UsersRoles.tableName "remove a role assignment" Nothing


-- User

createUser :: DBConnector -> UserInsert -> IO (DBWriteResult Users)
createUser conn =
    dbInsert conn audit insertUser >=> return . dbWriteResult
  where
    audit = AuditInfo User.tableName "create a new user" (Just DomainAPI.getUser)

getUser :: DBConnector -> PKey -> IO (DBUniqueResult Users)
getUser conn =
    dbQuery conn User.getUser >=> return . dbUniqueResult

activateUser :: DBConnector -> PKey -> IO (DBWriteResult Users)
activateUser conn pkey =
    updateUser conn pkey (Just "activate a user") def {User.uStatus = NewVal 2}

deactivateUser :: DBConnector -> PKey -> IO (DBWriteResult Users)
deactivateUser conn pkey =
    updateUser conn pkey (Just "deactivate a user") def {User.uStatus = NewVal 1}

updateUser :: DBConnector -> PKey -> Maybe Text -> UserUpdate -> IO (DBWriteResult Users)
updateUser conn pkey mSummary upd =
    dbWriteResult <$> dbUpdate conn audit (updateUserVariadic upd) pkey
  where
    audit = AuditInfo User.tableName (fromMaybe "update a user" mSummary) (Just DomainAPI.getUser)


-- Tenant

createTenant :: DBConnector -> TenantInsert -> IO (DBWriteResult Tenants)
createTenant conn =
    dbInsert conn audit insertTenant >=> return . dbWriteResult
    where
      audit = AuditInfo Tenant.tableName "create a new tenant" (Just DomainAPI.getTenant)

getTenant :: DBConnector -> PKey -> IO (DBUniqueResult Tenants)
getTenant conn =
    dbQuery conn Tenant.getTenant >=> return . dbUniqueResult

activateTenant :: DBConnector -> PKey -> IO (DBWriteResult Tenants)
activateTenant conn pkey =
    updateTenant conn pkey (Just "activate a tenant") def {Tenant.uStatus = NewVal 2}

deactivateTenant :: DBConnector -> PKey -> IO (DBWriteResult Tenants)
deactivateTenant conn pkey =
    updateTenant conn pkey (Just "deactivate a tenant") def {Tenant.uStatus = NewVal 1}

updateTenant :: DBConnector -> PKey -> Maybe Text -> TenantUpdate -> IO (DBWriteResult Tenants)
updateTenant conn pkey mSummary upd =
    dbWriteResult <$> dbUpdate conn audit (updateTenantVariadic upd) pkey
  where
    audit = AuditInfo Tenant.tableName (fromMaybe "update a tenant" mSummary) (Just DomainAPI.getTenant)
