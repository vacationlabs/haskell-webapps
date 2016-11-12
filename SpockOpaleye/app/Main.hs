{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import           DataTypes
import           JsonInstances              ()
import           TenantApi
import           Validations

import           Web.Spock
import           Web.Spock.Config

import qualified Data.Text                  as T

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
    do maybeTenantIncoming <- jsonBody
       case maybeTenantIncoming of
         Just incomingTenant -> do
           result <- runQuery (\conn -> validateIncomingTenant conn incomingTenant)
           case result of
             Valid -> json $ T.pack "Validation fail" 
             _ -> json $ T.pack "Validation fail"
         Nothing -> json $ T.pack "Unrecognized input"
