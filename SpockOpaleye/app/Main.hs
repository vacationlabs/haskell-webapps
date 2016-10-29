{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import TenantApi
import UserApi
import RoleApi
import DataTypes

import Data.List.NonEmpty
import Data.Maybe

main = do
    conn <- 
        connect
            defaultConnectInfo
            { connectDatabase = "haskell-webapps"
            }
    let role = 
            Role
            {
              role_id= 5,
              role_tenantid = 1,
              role_name= "name",
              role_permission= fromJust $ nonEmpty $ fmap Permission ["read", "write", "update"]
            }
    create_role conn role
    --tenants <- read_tenants conn
    --putStrLn $ show tenants
    return ()
