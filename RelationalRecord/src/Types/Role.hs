{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveAnyClass #-}

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

-- NOTE deriving a default instance doesn't make sense here: there's
-- no sane value for the primary key field, and for the timestamp
-- fields (createdAt, updatedAt), IO is necessary.
