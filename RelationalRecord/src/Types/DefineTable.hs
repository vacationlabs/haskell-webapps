{-# LANGUAGE TemplateHaskell #-}

module Types.DefineTable
    ( defineTable
    , module Types.DB
    ) where


import  DataSource
import  Types.DB

import  Language.Haskell.TH             (Q, Dec)

import  Database.HDBC.Query.TH          (defineTableFromDB)
import  Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)


defineTable :: String -> Q [Dec]
defineTable tableName =
    defineTableFromDB getDataSource driverPostgreSQL pgSchemaName tableName [''Show]
