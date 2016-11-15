{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.Role where

import  DefineTable
import  Database.Relational.Query.Table     as T (name)
import  Database.Relational.Query.Relation  (tableOf)
import  Data.Text                           (Text, pack)

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "roles")

tableName :: Text
tableName = pack $ T.name $ tableOf roles

deriving instance Generic Roles
instance ToJSON Roles
