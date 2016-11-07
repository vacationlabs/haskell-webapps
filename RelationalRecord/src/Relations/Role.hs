{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Relations.Role where

import  Types.Role as Role
import  Types.DB

import  Database.Relational.Query
-- import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)


allRoles :: Relation () Roles
allRoles = Role.roles

getRole :: Relation Int32 Roles
getRole = relation' . placeholder $ \rolId -> do
    a       <- query roles
    wheres  $ a ! Role.id' .=. rolId
    return  a

deleteRoleById :: Delete Int32
deleteRoleById = derivedDelete $ \projection ->
    fst <$> placeholder (\rolId -> wheres $ projection ! Role.id' .=. rolId)

deleteRoleByName :: Delete String
deleteRoleByName = derivedDelete $ \projection ->
    fst <$> placeholder (\rolName -> wheres $ projection ! Role.name' .=. rolName)
