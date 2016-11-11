{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.Tenant where

import  DefineTable
import  Database.Relational.Query.Table     as T (name)
import  Database.Relational.Query.Relation  (tableOf)

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "tenants")

tableName :: String
tableName = T.name $ tableOf tenants

deriving instance Generic Tenants
instance ToJSON Tenants
