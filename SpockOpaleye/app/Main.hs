{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import UserApi
import DataTypes
import TenantApi

main :: IO ()
main = do
  conn <-
    connect
      defaultConnectInfo
      { connectDatabase = "haskell-webapps"
      }
  clear_database conn
  return ()

clear_database :: Connection -> IO ()
clear_database conn = do
  Just tenants <- read_tenants conn
  mapM_ (remove_tenant conn) tenants
  return ()
