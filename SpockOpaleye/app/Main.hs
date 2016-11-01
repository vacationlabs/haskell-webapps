{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import UserApi
import DataTypes
import JsonInstances
import TenantApi
import RoleApi
import CryptoDef

import Data.Maybe
import Data.List.NonEmpty

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T
import Data.Aeson hiding (json)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

connectDb :: IO Connection
connectDb = connect defaultConnectInfo {connectDatabase="haskell-webapps"}

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession (PCConn $ ConnBuilder connectDb close (PoolCfg 10 10 10)) (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM Connection MySession MyAppState ()
app =  do
         post ("createTenant") $
             do 
              (DummyAppState ref) <- getState
              maybe_tenant <- jsonBody
              liftIO $ putStrLn $ show maybe_tenant
              result <- runQuery (\conn -> case maybe_tenant of 
                Just tenant -> create_tenant conn tenant
                _ -> return Nothing)
              case result of 
                Just _ -> json $ T.pack "Tenant created sucessfully"
                _ -> json $ T.pack "Tenant not created"
              
