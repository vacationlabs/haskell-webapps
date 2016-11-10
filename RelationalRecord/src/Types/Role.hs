{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.Role where

import  Types.DefineTable
import  Database.Relational.Query.Table     (name)
import  Database.Relational.Query.Relation  (tableOf)

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "roles")

tableName :: String
tableName = Database.Relational.Query.Table.name $ tableOf roles

deriving instance Generic Roles
instance ToJSON Roles
