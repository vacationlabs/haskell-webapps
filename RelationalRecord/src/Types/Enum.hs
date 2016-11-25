{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Types.Enum where

import  Helpers.DefineEnum

-- import  Database.HDBC.Query.TH              (makeRecordPersistableDefault)
import  Database.Record
import  Database.Record.TH              (deriveNotNullType)

import  Database.HDBC.SqlValue
import  Data.ByteString.Char8           (unpack)
import  Data.Char                       (toUpper, toLower)

$(defineEnum "test_enum")

-- TODO
-- This Haskell wrapper type is *not* type-safe, conversion might result
-- in inconsistencies or even a runtime error when the underlying DB type
-- has been changed :(

-- possible solution: build a custom TH action, select * from the
-- above defined table, creating the datatype below

-- But at least, mapping of a Postgres enum is working!


-- this definition must be verbatim to the enum type def in schema.sql !
data TestEnum = Inactive | Active | New deriving (Show, Read, Eq)


instance FromSql SqlValue TestEnum where
    recordFromSql = valueRecordFromSql sqlRead

instance ToSql SqlValue TestEnum where
    recordToSql = valueRecordToSql sqlShow


sqlRead :: Read a => SqlValue -> a
sqlRead (SqlString (s:ss))  = read $ toUpper s : ss
sqlRead (SqlByteString s)   = sqlRead (SqlString $ unpack s)
sqlRead _                   = undefined

sqlShow :: Show a => a -> SqlValue
sqlShow val = let (s:ss) = show val in SqlString $ toLower s : ss

$(deriveNotNullType [t| TestEnum |])
