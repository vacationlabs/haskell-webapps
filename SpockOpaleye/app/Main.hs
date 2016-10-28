{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import TenantApi
import UserApi
import DataTypes

main = do
    conn <- 
        connect
            defaultConnectInfo
            { connectDatabase = "haskell-webapps"
            }
    let user = 
            User
            { user_id = 2
            , user_tenantid = 1
            , user_username = "sdasdadsD"
            , user_password = ""
            , user_firstname = Just "firstname"
            , user_lastname = Nothing
            , user_status = UserStatusInActive
            }
    create_user conn user
    --tenants <- read_tenants conn
    --putStrLn $ show tenants
    return
        ()
