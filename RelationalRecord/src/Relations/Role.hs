{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module  Relations.Role where

import  Types.Role          as Role
import  Types.User          as User
import  Types.UsersRoles    as UsersRoles
import  Types.DB
import  Relations.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)

import  GHC.Generics                (Generic)
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
getRoles :: Relation Users Roles
getRoles = relation' . placeholder $ \user -> do
    a       <- query roles
    ur      <- query usersRoles
    on      $ a ! Role.id' .=. ur ! UsersRoles.roleId'
    wheres  $ ur ! UsersRoles.userId' .=. user ! User.id'
    return  a



-- INSERTS

data RoleAssignment = RoleAssignment
    { iUserId :: PKey
    , iRoleId :: PKey
    }
$(makeRecordPersistableDefault ''RoleAssignment)

piAssignRole :: Pi UsersRoles RoleAssignment
piAssignRole = RoleAssignment |$| UsersRoles.userId' |*| UsersRoles.roleId'

assignRole :: Insert RoleAssignment
assignRole = derivedInsert piAssignRole


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

removeRole :: Delete RoleAssignment
removeRole = derivedDelete $ \projection ->
    fst <$> placeholder (\rAssign -> do
        wheres $ projection ! UsersRoles.userId' .=. rAssign ! iUserId'
        wheres $ projection ! UsersRoles.roleId' .=. rAssign ! iRoleId'
        )
