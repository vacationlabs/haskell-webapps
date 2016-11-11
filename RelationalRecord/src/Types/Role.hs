{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.Role where

import  DefineTable
import  Database.Relational.Query.Table     as T (name)
import  Database.Relational.Query.Relation  (tableOf)

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "roles")

tableName :: String
tableName = T.name $ tableOf roles

deriving instance Generic Roles
instance ToJSON Roles
