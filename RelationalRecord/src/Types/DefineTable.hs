{-# LANGUAGE TemplateHaskell #-}

module  Types.DefineTable
        ( defineTable
        , module DataSource
        ) where


import  DataSource

import  Language.Haskell.TH             (Q, Dec)

import  Database.HDBC.Query.TH          (defineTableFromDB')
import  Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import  Database.HDBC.Schema.Driver     (typeMap)
import  Database.Relational.Query.Component (Config(..), defaultConfig)
import  Data.ByteString                 (ByteString)


defineTable :: String -> Q [Dec]
defineTable tableName =
    defineTableFromDB' getDataSource
        (defaultConfig {verboseAsCompilerWarning = True})
        (withAdditionalTypes driverPostgreSQL)
        pgSchemaName tableName
        [''Show]
  where
    withAdditionalTypes driver =
        driver {typeMap = ("jsonb", [t| ByteString |]) : typeMap driver}        -- FIXME should work, but doesn't!
