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
  post ("createTenant") $ do 
    maybe_tenant_incoming <- jsonBody
    result <- case maybe_tenant_incoming of
      Just incoming_tenant ->  do
        case validateIncomingTenant incoming_tenant of
          Valid -> runQuery (\conn -> create_tenant conn incoming_tenant)
          _ -> return Nothing
      Nothing -> return Nothing
    case result of
      Just _ -> json $ T.pack "Tenant created sucessfully"
      _ -> json $ T.pack "Tenant not created"
