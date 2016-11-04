{-# LANGUAGE FlexibleContexts #-}

module Main where

import Types.Tenant
import DataSource


import Database.HDBC            hiding (run)


import Database.Relational.Query
import Database.Record
import Database.HDBC.Record.Query

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL


run :: (IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
       => conn -> p -> Relation p a -> IO [a]
run conn param rel =
    runQuery conn (relationalQuery rel) param


allTenants :: Relation () Tenant
allTenants = relation (query tenant)

main :: IO ()
main = do
    conn    <- getDataSource
    results <- run conn () allTenants
    mapM_ (BL.putStrLn . encode) results
