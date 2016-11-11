{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.Tenant where

import  Prelude                             hiding (id)
import  Types.DefineTable
import  Database.Relational.Query.Table     (name)
import  Database.Relational.Query.Relation  (tableOf)

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "tenants")

tableName :: String
tableName = Database.Relational.Query.Table.name $ tableOf tenants

instance HasPKey Tenants where
    getPKey = id

deriving instance Generic Tenants
instance ToJSON Tenants
