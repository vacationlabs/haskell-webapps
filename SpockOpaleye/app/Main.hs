{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import           DataTypes
import           JsonInstances              ()
import           TenantApi
import           Validations

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Reader
import           Control.Monad.Writer
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

runAuditM :: Connection -> AuditM a -> IO a
runAuditM conn x = do
  (item, lg) <- runReaderT (runWriterT x) (conn, Nothing, Nothing)
  putStrLn lg
  return item


app :: SpockM Connection MySession MyAppState ()
app = do
  post ("tenants/new") $
    do maybeTenantIncoming <- jsonBody
       case maybeTenantIncoming of
         Just incomingTenant -> do
           result <- runQuery (\conn -> validateIncomingTenant conn incomingTenant)
           case result of
             Valid -> do
                  newTenant <- runQuery (\conn -> runAuditM conn $ createTenant conn incomingTenant)
                  json newTenant
             _ -> json $ T.pack "Validation fail"
         Nothing -> json $ T.pack "Unrecognized input"
