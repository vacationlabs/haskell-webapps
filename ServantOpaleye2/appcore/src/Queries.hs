module Queries (
    roleQuery,
    userQuery,
    tenantQuery,
    TenantQuery,
    UserQuery,
    RoleQuery,
    TenantTableR,
    UserTableR,
    RoleTableR
    )
where

import TenantDefs
import UserDefs
import RoleDefs
import Opaleye (Query, queryTable)

type TenantQuery = Query TenantTableR
type UserQuery = Query UserTableR
type RoleQuery = Query RoleTableR

roleQuery :: RoleQuery
roleQuery = queryTable roleTable

userQuery :: UserQuery
userQuery = queryTable userTable

tenantQuery :: TenantQuery
tenantQuery = queryTable tenantTable
