
module  DomainAPI where

import  DataSource
import  DBInterface
import  Types.Tenant                (Tenants)
import  Types.User                  (Users)
import  Types.Role                  (Roles)
import  Relations.Tenant            as Tenant
import  Relations.Role              as Role
import  Relations.User              as User

import  Control.Monad               ((>=>))



-- Role

getRole :: DBConnector -> PKey -> IO (DBUniqueResult Roles)
getRole conn = dbQuery conn Role.getRole >=> return . dbUniqueResult

assignRole :: DBConnector -> AssignRole -> IO DBWriteResult
assignRole conn = dbInsert conn Role.assignRole >=> return . dbWriteResult

-- TODO    CreateRole, DeleteRole,  RemoveRole



-- User

createUser :: DBConnector -> UserInsert -> IO DBWriteResult
createUser conn = dbInsert conn insertUser >=> return . dbWriteResult

getUser :: DBConnector -> PKey -> IO (DBUniqueResult Users)
getUser conn = dbQuery conn User.getUser >=> return . dbUniqueResult

-- TODO  UpdateUser, ActivateUser, DeactivateUser



-- Tenant

createTenant :: DBConnector -> TenantInsert -> IO DBWriteResult
createTenant conn = dbInsert conn insertTenant >=> return . dbWriteResult

getTenant :: DBConnector -> PKey -> IO (DBUniqueResult Tenants)
getTenant conn = dbQuery conn Tenant.getTenant >=> return . dbUniqueResult

activateTenant :: HasPKey k => DBConnector -> k -> PKey -> IO DBWriteResult
activateTenant conn pkey ownerId =
    dbWriteResult <$> dbUpdate conn (updateTenant Tenant.updStatus2) (getPKey pkey) (2, Just ownerId)

deactivateTenant :: HasPKey k => DBConnector -> k -> IO DBWriteResult
deactivateTenant conn pkey =
    dbWriteResult <$> dbUpdate conn (updateTenant Tenant.updStatus) (getPKey pkey) 1

-- TODO UpdateTenant
