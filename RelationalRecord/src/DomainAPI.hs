
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

import  Control.Monad               ((>=>))



-- Role

getRole :: DBConnector -> PKey -> IO (DBUniqueResult Roles)
getRole conn = dbQuery conn Role.getRole >=> return . dbUniqueResult

assignRole :: DBConnector -> AssignRole -> IO DBWriteResult
assignRole conn = dbInsert conn UsersRoles.tableName Role.assignRole >=> return . dbWriteResult

-- TODO    CreateRole, DeleteRole,  RemoveRole



-- User

createUser :: DBConnector -> UserInsert -> IO DBWriteResult
createUser conn =
    dbInsert conn User.tableName insertUser >=> return . dbWriteResult

getUser :: DBConnector -> PKey -> IO (DBUniqueResult Users)
getUser conn = dbQuery conn User.getUser >=> return . dbUniqueResult

-- TODO  UpdateUser, ActivateUser, DeactivateUser



-- Tenant

createTenant :: DBConnector -> TenantInsert -> IO DBWriteResult
createTenant conn =
    dbInsert conn Tenant.tableName insertTenant >=> return . dbWriteResult

getTenant :: DBConnector -> PKey -> IO (DBUniqueResult Tenants)
getTenant conn =
    dbQuery conn Tenant.getTenant >=> return . dbUniqueResult

activateTenant :: DBConnector -> PKey -> IO DBWriteResult
activateTenant conn pkey =
    updateTenant conn pkey tenantUpdate {uStatus = Just 2}

deactivateTenant :: DBConnector -> PKey -> IO DBWriteResult
deactivateTenant conn pkey =
    updateTenant conn pkey tenantUpdate {uStatus = Just 1}

updateTenant :: DBConnector -> PKey -> TenantUpdate -> IO DBWriteResult
updateTenant conn pkey upd =
    dbWriteResult <$> dbUpdate conn Tenant.tableName
        (updateTenantVariadic upd)
        pkey
