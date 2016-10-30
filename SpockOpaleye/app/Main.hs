{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import TenantApi()
import UserApi
import RoleApi()
import DataTypes

main :: IO ()
main = do
    conn <- 
        connect
            defaultConnectInfo
            { connectDatabase = "haskell-webapps"
            }
    let user = 
            User
            { user_id = Just 5
            , user_tenantid = 1
            , user_username = "asdsdasdadsD"
            , user_password = ""
            , user_firstname = Just "firstname"
            , user_lastname = Nothing
            , user_status = UserStatusInActive
            }
    ids <- create_user conn user
    putStrLn $ show ids
    return ()
