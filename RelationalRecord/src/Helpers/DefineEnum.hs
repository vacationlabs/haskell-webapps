{-# LANGUAGE TemplateHaskell, LambdaCase, ViewPatterns #-}

module Helpers.DefineEnum (defineEnum) where

import DataSource

import Language.Haskell.TH
import Language.Haskell.TH.Name.CamelCase
import Language.Haskell.TH.Compat.Data      (dataD')

import Database.HDBC.Session                (withConnectionIO)
import Database.HDBC                        (SqlValue(..), safeFromSql, quickQuery')

import Database.Record
import Database.Record.TH                   (deriveNotNullType)

import Data.ByteString.Char8                as B (pack, unpack)
import Data.Either                          (partitionEithers)
import Control.Arrow                        ((&&&))



defineEnum :: String -> Q [Dec]
defineEnum enumName =
    runIO getInfo >>= \case
        []  -> die $ "no ENUM found on DB named " ++ enumName
        vs  -> case partitionEithers [safeFromSql (head v) | v <- vs, (not . null) v] of
                ([], vs'')  -> generateEnum enumName vs''
                (errs, _)   -> die $ unlines $ map show errs

  where
    die err     = reportError ("defineEnum: " ++ err) >> return []
    enumName'   = '_' : enumName
    query       = "SELECT enumlabel FROM pg_type JOIN pg_enum ON (enumtypid=typelem) \
                    \WHERE typname='" ++ enumName' ++ "' ORDER BY enumsortorder ASC;"
    getInfo :: IO [[SqlValue]]
    getInfo =
        withConnectionIO getDataSource $ \conn' -> quickQuery' conn' query []


sqlShow :: Show a => a -> SqlValue
sqlShow = SqlByteString . B.pack . show

sqlRead :: Read a => SqlValue -> a
sqlRead = \case
    SqlByteString bs    -> read . B.unpack $ bs
    SqlString s         -> read s
    _                   -> read ""

genSqlInstances :: ConName -> DecsQ
genSqlInstances (conName -> name) =
    [d| instance ToSql SqlValue $(conT name) where
            recordToSql = valueRecordToSql sqlShow
        instance FromSql SqlValue $(conT name) where
            recordFromSql = valueRecordFromSql sqlRead|]
{-
genMapping :: String -> ConName -> DecsQ
genMapping eName (conName -> name) =
    [d| mapping :: (String, TypeQ)
        mapping = (eName , $(varT name))|]
-}

generateEnum :: String -> [String] -> Q [Dec]
generateEnum enumName enumVals = do
    typeDecl    <- genEnumType name vals
    showDecl    <- genShow name (zip vals enumVals)
    readDecl    <- genRead name
    sqlDecl     <- genSqlInstances name
    persDecl    <- deriveNotNullType (conT $ conName name)
    return      $ typeDecl : showDecl : readDecl ++ sqlDecl ++ persDecl
  where
    name = conCamelcaseName enumName
    vals = map conCamelcaseName enumVals

genShow :: ConName -> [(ConName, String)] -> DecQ
genShow (conName -> name) pairs =
  instanceD (cxt [])
    (appT (conT ''Show) (conT name))
    [funD (mkName "show") $ map genClause pairs]
  where
    genClause (k, v) = clause [(conP (conName k) [])] (normalB [|v|]) []

genRead :: ConName -> DecsQ
genRead (conName -> name) =
    [d|instance Read $(conT name) where
            readsPrec _ v = case lookup v dict of
                Just res -> [(res, [])]
                Nothing  -> []
              where
                dict = map (show &&& id) [minBound .. maxBound]|]

genEnumType :: ConName -> [ConName] -> DecQ
genEnumType (conName -> name) cons =
    dataD' (cxt []) name [] cons' [''Eq, ''Enum, ''Bounded]
  where
    cons' = map (\n -> normalC (conName n) []) cons
