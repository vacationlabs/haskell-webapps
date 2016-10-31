{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import UserApi
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
        { user_id = UserId 2
        , user_tenantid = 1
        , user_username = "asasd"
        , user_password = ""
        , user_firstname = Just "firstname_updated"
        , user_lastname = Nothing
        , user_status = UserStatusInActive
        }
  update_user conn (UserId 2) user
  return ()
