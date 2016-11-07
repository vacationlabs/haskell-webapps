{-# LANGUAGE FlexibleContexts #-}

module Main where

import DataSource
import DBInterface
import Relations.Tenant
import Relations.Role



import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL


createTenant :: TenantInsert
createTenant = TenantInsert
    "TestTenant" "Sylvain" "Duchamps" "sly@champsxxx.fr" "3980" "sy.champs.xxxxx.fr"

main :: IO ()
main = do
    conn    <- getDataSource

    dbUpdate conn updateTenant 1 >>= print
    results <- dbQuery conn getTenant 1
    mapM_ (BL.putStrLn . encode) results
    --mapM_ print results

    dbDelete conn deleteRoleById 4 >>= print

    results <- dbQuery conn allRoles ()
    mapM_ print results

    dbInsert conn insertTenant createTenant >>= print
    results <- dbQuery conn allTenants ()
    print results
