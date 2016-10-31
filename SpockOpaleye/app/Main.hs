{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import UserApi
import DataTypes
import TenantApi
import RoleApi

import Data.Maybe
import Data.List.NonEmpty

main :: IO ()
main = do
  conn <-
    connect
      defaultConnectInfo
      { connectDatabase = "haskell-webapps"
      }
  clear_database conn
  Just tenant <-
    create_tenant
      conn
      Tenant
      { tenant_id = TenantId 0
      , tenant_name = "tenant_1"
      , tenant_firstname = "firstname"
      , tenant_lastname = "lastname"
      , tenant_email = "email"
      , tenant_phone = "phone"
      , tenant_status = TenantStatusInActive
      , tenant_ownerid = Nothing
      , tenant_backofficedomain = "bo domain"
      }
  putStrLn $ show tenant
  Just user <-
    create_user
      conn
      User
      { user_id = UserId 0
      , user_tenantid = tenant_id tenant
      , user_username = "username"
      , user_password = "password"
      , user_firstname = Just "firstname"
      , user_lastname = Just "lastname"
      , user_status = UserStatusInActive
      }
  putStrLn $ show user
  Just role <-
    create_role
      conn
      Role
      { role_id = RoleId 0
      , role_tenantid = tenant_id tenant
      , role_name = "Role 1"
      , role_permission =
        fromJust $ nonEmpty $ [Read, Update]
      }
  add_role_to_user conn (user_id user) (role_id role)
  remove_role_from_user conn (user_id user) (role_id role)
  --update_user conn user_id 
  return ()

clear_database :: Connection -> IO ()
clear_database conn = do
  tenants <- read_tenants conn
  mapM_ (remove_tenant conn) tenants
  return ()
