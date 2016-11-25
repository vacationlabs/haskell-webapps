{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module Helpers.DefineEnum where

import DataSource

import Language.Haskell.TH                  (Q, runIO, Name, TypeQ, Dec)
import Language.Haskell.TH.Name.CamelCase   (varCamelcaseName)
import Language.Haskell.TH.Lib.Extra        (reportWarning, reportError)

import Database.HDBC.Session                (withConnectionIO)
import Database.HDBC                        (IConnection, SqlValue, safeFromSql, quickQuery')

import Data.Either


-- select enumlabel from pg_type join pg_enum on (enumtypid = typelem) where typname = '_test_enum' order by enumsortorder asc;

defineEnum :: String -> Q [Dec]
defineEnum enumName =
    runIO getInfo >>= \case
        []  -> die $ "no ENUM found on DB named " ++ enumName
        vs  -> case partitionEithers [safeFromSql (head v) | v <- vs, (not . null) v] of
                ([], vs'')  -> generateEnum enumName vs''
                (errs, _)   -> die $ concatMap show errs

  where
    die err     = reportError ("defineEnum: " ++ err) >> return []
    enumName'   = '_' : enumName
    query       = "SELECT enumlabel FROM pg_type JOIN pg_enum ON (enumtypid=typelem) \
                    \WHERE typname='" ++ enumName' ++ "' ORDER BY enumsortorder ASC;"
    getInfo :: IO [[SqlValue]]
    getInfo = withConnectionIO getDataSource $ \conn' -> quickQuery' conn' query []


generateEnum :: String -> [String] -> Q [Dec]
generateEnum enumName enumVals =
    reportWarning ('\n' : enumName ++ " ->\n" ++ unlines enumVals) >> return []
