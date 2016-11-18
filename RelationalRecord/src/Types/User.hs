{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.User where

import  DefineTable
import  Database.Relational.Query.Table     as T (name)
import  Database.Relational.Query.Relation  (tableOf)
import  Data.Text                           (Text, pack)

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "users")

tableName :: Text
tableName = pack $ T.name $ tableOf users

deriving instance Generic Users
instance ToJSON Users

-- NOTE deriving a default instance doesn't make sense here: there's
-- no sane value for the primary key field, and for the timestamp
-- fields (createdAt, updatedAt), IO is necessary.
