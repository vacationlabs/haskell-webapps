{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module  Relations.Role where

import  Types.Role                  as Role
import  qualified Types.User        as User
import  Types.UsersRoles            as UsersRoles
import  Types.DB
import  Relations.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH              (makeRecordPersistableDefault)
import  Database.Relational.Query.Pi.Unsafe (defineDirectPi)
import  Database.Relational.Query.Relation  (tableOf)


import  GHC.Generics                        (Generic)
import  Data.Default



-- SELECTS

allRoles :: Relation () Roles
allRoles = Role.roles

allRoleAssignments :: Relation () UsersRoles
allRoleAssignments = UsersRoles.usersRoles

getRole :: Relation PKey Roles
getRole = relation' . placeholder $ \rolId -> do
    a       <- query roles
    wheres  $ a ! Role.id' .=. rolId
    return  a

-- given a user, get all his/her roles (inner join)
getRoles :: Relation User.Users Roles
getRoles = relation' . placeholder $ \user -> do
    a       <- query roles
    ur      <- query usersRoles
    on      $ a ! Role.id' .=. ur ! UsersRoles.roleId'
    wheres  $ ur ! UsersRoles.userId' .=. user ! User.id'
    return  a



-- INSERTS

-- table has no PK: we can use the identity projection provided by HRR
assignRole :: Insert UsersRoles
assignRole = typedInsert (tableOf usersRoles) (defineDirectPi [0, 1])


-- an insert constrained to the obligatory fields, thus enforcing
-- default values encoded in the DB schema for all other fields
data RoleInsert = RoleInsert
    { iTenantId     :: PKey
    , iName         :: Text
    , iPermissions  :: Text
    }
$(makeRecordPersistableDefault ''RoleInsert)

piRoles :: Pi Roles RoleInsert
piRoles = RoleInsert
    |$| Role.tenantId'
    |*| Role.name'
    |*| Role.permissions'

insertRole :: Insert RoleInsert
insertRole = derivedInsert piRoles


-- UPDATES

data RoleUpdate = RoleUpdate
    { uTenantId     :: VariadicArg PKey
    , uName         :: VariadicArg Text
    , uPermissions  :: VariadicArg Text
    }
    deriving (Generic, Default)

roleVariadic :: Roles -> Roles -> RoleUpdate
roleVariadic old new = RoleUpdate
    (varArg tenantId old new)
    (varArg name old new)
    (varArg permissions old new)

updateRoleVariadic :: RoleUpdate -> TimestampedUpdate
updateRoleVariadic RoleUpdate {..} = derivedUpdate $ \projection -> do
    Role.tenantId'      <-#? uTenantId
    Role.name'          <-#? uName
    Role.permissions'   <-#? uPermissions

    (phTStamp, _)   <- placeholder (\tStamp -> Role.updatedAt' <-# tStamp)
    (phRolId, _)    <- placeholder (\rolId -> wheres $ projection ! Role.id' .=. rolId)
    return          $ phTStamp >< phRolId


-- DELETES

deleteRoleById :: Delete PKey
deleteRoleById = derivedDelete $ \projection ->
    fst <$> placeholder (\rolId -> wheres $ projection ! Role.id' .=. rolId)

deleteRoleByName :: Delete Text
deleteRoleByName = derivedDelete $ \projection ->
    fst <$> placeholder (\rolName -> wheres $ projection ! Role.name' .=. rolName)

-- TODO check if there's some default definition in HRR for this Delete
removeRole :: Delete UsersRoles
removeRole = derivedDelete $ \projection ->
    fst <$> placeholder (\rAssign -> do
        wheres $ projection ! UsersRoles.userId' .=. rAssign ! userId'
        wheres $ projection ! UsersRoles.roleId' .=. rAssign ! roleId'
        )
