
module  DomainAPI where


import  DBInterface
import  Types.Tenant                (Tenants)
import  Types.User                  (Users)
import  Relations.Tenant            as Tenant
import  Relations.Role
import  Relations.User              as User

import  Database.HDBC               (IConnection)
import  Control.Monad               ((>=>))


--    CreateRole, DeleteRole, AssignRole, RemoveRole, GetRole

-- User

createUser :: IConnection conn => conn -> UserInsert -> IO DBWriteResult
createUser conn = dbInsert conn insertUser >=> return . dbWriteResult

getUser :: IConnection conn => conn -> PKey -> IO (DBUniqueResult Users)
getUser conn = dbQuery conn User.getUser >=> return . dbUniqueResult
-- TODO  UpdateUser, ActivateUser, DeactivateUser



-- Tenant

createTenant :: IConnection conn => conn -> TenantInsert -> IO DBWriteResult
createTenant conn = dbInsert conn insertTenant >=> return . dbWriteResult

getTenant :: IConnection conn => conn -> PKey -> IO (DBUniqueResult Tenants)
getTenant conn = dbQuery conn Tenant.getTenant >=> return . dbUniqueResult

activateTenant :: IConnection conn => conn -> PKey -> IO DBWriteResult
activateTenant conn pkey =
    dbWriteResult <$> dbUpdate conn (updateTenant Tenant.updStatus) pkey 2

deactivateTenant :: IConnection conn => conn -> PKey -> IO DBWriteResult
deactivateTenant conn pkey =
    dbWriteResult <$> dbUpdate conn (updateTenant Tenant.updStatus) pkey 1

-- TODO UpdateTenant
