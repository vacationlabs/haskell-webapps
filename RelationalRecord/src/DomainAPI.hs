
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



-- Role

createRole :: DBConnector -> RoleInsert -> IO DBWriteResult
createRole conn =
    dbInsert conn Role.tableName insertRole >=> return . dbWriteResult

getRole :: DBConnector -> PKey -> IO (DBUniqueResult Roles)
getRole conn =
    dbQuery conn Role.getRole >=> return . dbUniqueResult

updateRole :: DBConnector -> PKey -> RoleUpdate -> IO DBWriteResult
updateRole conn pkey upd =
    dbWriteResult <$> dbUpdate conn Role.tableName
        (updateRoleVariadic upd)
        pkey

deleteRole :: DBConnector -> Either PKey Text -> IO DBWriteResult
deleteRole conn eFilter =
    dbWriteResult <$> case eFilter of
        Left pkey   -> dbDelete conn deleteRoleById pkey
        Right name  -> dbDelete conn deleteRoleByName name

assignRole :: DBConnector -> RoleAssignment -> IO DBWriteResult
assignRole conn =
    dbInsert conn UsersRoles.tableName Role.assignRole >=> return . dbWriteResult

removeRole :: DBConnector -> RoleAssignment -> IO DBWriteResult
removeRole conn =
    dbDelete conn Role.removeRole >=> return . dbWriteResult


-- User

createUser :: DBConnector -> UserInsert -> IO DBWriteResult
createUser conn =
    dbInsert conn User.tableName insertUser >=> return . dbWriteResult

getUser :: DBConnector -> PKey -> IO (DBUniqueResult Users)
getUser conn =
    dbQuery conn User.getUser >=> return . dbUniqueResult

activateUser :: DBConnector -> PKey -> IO DBWriteResult
activateUser conn pkey =
    updateUser conn pkey def {User.uStatus = NewVal 2}

deactivateUser :: DBConnector -> PKey -> IO DBWriteResult
deactivateUser conn pkey =
    updateUser conn pkey def {User.uStatus = NewVal 1}

updateUser :: DBConnector -> PKey -> UserUpdate -> IO DBWriteResult
updateUser conn pkey upd =
    dbWriteResult <$> dbUpdate conn User.tableName
        (updateUserVariadic upd)
        pkey


-- Tenant

createTenant :: DBConnector -> TenantInsert -> IO DBWriteResult
createTenant conn =
    dbInsert conn Tenant.tableName insertTenant >=> return . dbWriteResult

getTenant :: DBConnector -> PKey -> IO (DBUniqueResult Tenants)
getTenant conn =
    dbQuery conn Tenant.getTenant >=> return . dbUniqueResult

activateTenant :: DBConnector -> PKey -> IO DBWriteResult
activateTenant conn pkey =
    updateTenant conn pkey def {Tenant.uStatus = NewVal 2}

deactivateTenant :: DBConnector -> PKey -> IO DBWriteResult
deactivateTenant conn pkey =
    updateTenant conn pkey def {Tenant.uStatus = NewVal 1}

updateTenant :: DBConnector -> PKey -> TenantUpdate -> IO DBWriteResult
updateTenant conn pkey upd =
    dbWriteResult <$> dbUpdate conn Tenant.tableName
        (updateTenantVariadic upd)
        pkey
