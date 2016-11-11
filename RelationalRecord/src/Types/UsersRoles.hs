{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Types.UsersRoles where

import  DefineTable
import  Database.Relational.Query.Table     (name)
import  Database.Relational.Query.Relation  (tableOf)

$(defineTable "users_roles")

tableName :: String
tableName = Database.Relational.Query.Table.name $ tableOf usersRoles
