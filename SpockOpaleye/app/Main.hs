{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import DataTypes
import JsonInstances ()
import TenantApi
import Validations

import Web.Spock
import Web.Spock.Config

import qualified Data.Text as T

data MySession =
  EmptySession

data MyAppState = DummyAppState 

connectDb :: IO Connection
connectDb = connect defaultConnectInfo { connectDatabase = "haskell-webapps" }

main :: IO ()
main = do
  spockCfg <-
    defaultSpockCfg
      EmptySession
      (PCConn $ ConnBuilder connectDb close (PoolCfg 10 10 10))
      DummyAppState
  runSpock 8080 (spock spockCfg app)

app :: SpockM Connection MySession MyAppState ()
app = do
  post ("tenants/new") $
    do maybe_tenant_incoming <- jsonBody
       maybe_newtenant <-
         case maybe_tenant_incoming of
           Just incoming_tenant -> do
             result <-
               runQuery (\conn -> validateIncomingTenant conn incoming_tenant)
             case result of
               Valid -> runQuery (\conn -> create_tenant conn incoming_tenant)
               _ -> return Nothing
           Nothing -> return Nothing
       case maybe_newtenant of
         Just tenant -> json tenant
         _ -> json $ T.pack "Tenant not created"
