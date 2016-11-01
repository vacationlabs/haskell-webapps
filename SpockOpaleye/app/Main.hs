{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import UserApi
import DataTypes
import JsonInstances
import TenantApi
import RoleApi
import CryptoDef
import Validations

import Data.Maybe
import Data.List.NonEmpty

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T
import Data.Aeson hiding (json)

data MySession =
  EmptySession

data MyAppState =
  DummyAppState (IORef Int)

connectDb :: IO Connection
connectDb =
  connect
    defaultConnectInfo
    { connectDatabase = "haskell-webapps"
    }

main :: IO ()
main = do
  ref <- newIORef 0
  spockCfg <-
    defaultSpockCfg
      EmptySession
      (PCConn $ ConnBuilder connectDb close (PoolCfg 10 10 10))
      (DummyAppState ref)
  runSpock 8080 (spock spockCfg app)

app :: SpockM Connection MySession MyAppState ()
app = do
  post ("tenants/new") $
    do maybe_tenant_incoming <- jsonBody
       maybe_newtenant <-
         case maybe_tenant_incoming of
           Just incoming_tenant -> do
             result <- runQuery (\conn -> validateIncomingTenant conn incoming_tenant)
             liftIO $ putStrLn $ show $ result
             case  result of
               Valid -> runQuery (\conn -> create_tenant conn incoming_tenant)
               _ -> return Nothing
           Nothing -> return Nothing
       case maybe_newtenant of
         Just tenant -> json tenant
         _ -> json $ T.pack "Tenant not created"
