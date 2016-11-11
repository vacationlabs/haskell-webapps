{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Relations.Role where

import  Types.Role          as Role
import  Types.User          as User
import  Types.UsersRoles    as UsersRoles
import  Types.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)


data AssignRole = AssignRole {iUserId :: PKey, iRoleId :: PKey}

$(makeRecordPersistableDefault ''AssignRole)

piAssignRole :: Pi UsersRoles AssignRole
piAssignRole = AssignRole |$| UsersRoles.userId' |*| UsersRoles.roleId'


allRoles :: Relation () Roles
allRoles = Role.roles

getRole :: Relation PKey Roles
getRole = relation' . placeholder $ \rolId -> do
    a       <- query roles
    wheres  $ a ! Role.id' .=. rolId
    return  a

getRoles :: Relation Users Roles
getRoles = relation' . placeholder $ \user -> do
    a       <- query roles
    ur      <- query usersRoles
    on      $ a ! Role.id' .=. ur ! UsersRoles.roleId'
    wheres  $ ur ! UsersRoles.userId' .=. user ! User.id'
    return  a

deleteRoleById :: Delete PKey
deleteRoleById = derivedDelete $ \projection ->
    fst <$> placeholder (\rolId -> wheres $ projection ! Role.id' .=. rolId)

deleteRoleByName :: Delete String
deleteRoleByName = derivedDelete $ \projection ->
    fst <$> placeholder (\rolName -> wheres $ projection ! Role.name' .=. rolName)

assignRole :: Insert AssignRole
assignRole = derivedInsert piAssignRole

instance HasTableName (Insert AssignRole) where
    getTableName = const UsersRoles.tableName
