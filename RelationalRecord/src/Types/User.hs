{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.User where

import  DefineTable
import  Database.Relational.Query.Table     (name)
import  Database.Relational.Query.Relation  (tableOf)

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "users")

tableName :: String
tableName = Database.Relational.Query.Table.name $ tableOf users

deriving instance Generic Users
instance ToJSON Users
