{-# LANGUAGE TemplateHaskell #-}

{-|
Module      :  DefineTable
Copyright   :  (c) VacationLabs
Maintainer  :  michaelkarg77@gmail.com

This module provides the TH function which reads the DB catalog at
compile time and generates Haskell data types from it.
-}

module  DefineTable
        ( defineTable
        , module DataSource
        ) where


import  DataSource
import  Types.Enum

import  Language.Haskell.TH                 (Q, Dec)

import  Database.HDBC.Query.TH              (defineTableFromDB')
import  Database.HDBC.Schema.PostgreSQL     (driverPostgreSQL)
import  Database.HDBC.Schema.Driver         (typeMap)
import  Database.Relational.Query.Component (Config(..), defaultConfig)
import  Data.ByteString                     (ByteString)

import  Data.Text                           (Text)



defineTable :: String -> Q [Dec]
defineTable tableName =
    defineTableFromDB' getDataSource
        (defaultConfig {verboseAsCompilerWarning = False})
        (withAdditionalTypes driverPostgreSQL)
        schemaName tableName
        [''Show]
  where
    withAdditionalTypes driver =
        driver { typeMap =
            [ ("jsonb", [t| ByteString |])                                      -- FIXME see Note below
            , ("text", [t| Text |])
            , ("test_enum", [t| TestEnum |])                                    -- FIXME see Note below
            ] ++ typeMap driver
            }


-- NOTE
{-
This is the relation used by the HRR driver to get postgres type info;
those will be the types considered for a generating a corresponding
attribute in the derived Haskell type.
(in pseudo-code, taken from module Database.Relational.Schema.PostgreSQL):

(select * from pg_type)
    wheres $ att ! Attr.atttypid'    .=. typ ! Type.oid'
    wheres $ typ ! Type.typtype'     .=. value 'b'  -- 'b': base type only

    wheres $ typ ! Type.typcategory' `in'` values [ 'B' -- Boolean types
                                                  , 'D' -- Date/time types
                                                  , 'I' -- Network Address types
                                                  , 'N' -- Numeric types
                                                  , 'S' -- String types
                                                  , 'T' -- typespan types
                                                  ]

We can see, JSONB is not amongst them as it is of category 'U' (user-defined).
Also, enums are not considered for mapping, they're of category 'E'.

Conlusion: The HRR library has to be patched accordingly for HRR to even consider
generating a datatype derivation for those categories in Haskell.
-}
