{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Types.UsersRoles where

import  DefineTable
import  Database.Relational.Query.Table     as T (name)
import  Database.Relational.Query.Relation  (tableOf)
import  Data.Text                           (Text, pack)


$(defineTable "users_roles")

tableName :: Text
tableName = pack $ T.name $ tableOf usersRoles
