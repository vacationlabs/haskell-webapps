{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Config
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Time.Clock
import DomainApi
import Network.Wai
import Network.Wai.Handler.Warp
import RestApi
import Servant
import Types

import qualified Database.HDBC as DB
import Database.HDBC.PostgreSQL


readerToHandler :: Config -> AppM :~> ExceptT ServantErr IO
readerToHandler cfg = Nat $ \x -> runReaderT x cfg

cartServer :: Config -> Server ShoppingCartAPI 
cartServer cfg = enter (readerToHandler cfg) handlers

shoppingCart ::Proxy ShoppingCartAPI
shoppingCart = Proxy

application :: Config -> Application
application cfg = serve shoppingCart (cartServer cfg)

main :: IO ()
main = do
    putStrLn "start"
    now <- getCurrentTime
    conn <- connectPostgreSQL "host=localhost dbname=cart user=cart password=cart"
    let t = Tenant 0 now now "asdf" "foo" "bar" "a@example.com" "phone" TenantInactive Nothing (show now)
    tid <- createTenant conn t
    tenant <- getTenant conn $ fromJust tid

    let u = User 0 now now (_tid $ fromJust tenant) ("user" ++ show now) "pass" "first" "last" "new"
    uid <- createUser conn u
    user <- getUser conn $ fromJust uid
    putStrLn $ show user
    deactivateUser conn $ fromJust user
    user <- getUser conn $ fromJust uid
    putStrLn $ show user
    activateUser conn $ fromJust user
    user <- getUser conn $ fromJust uid
    putStrLn $ show user

    
    activateTenant conn (fromJust tenant) (fromJust user)
    putStrLn $ show tenant
    tenant <- getTenant conn $ fromJust tid
    putStrLn $ show tenant

    let r = Role 0 (fromJust tid) "reader" [ReadFoo, ReadBar] now now
    rid <- createRole conn r
    role <- getRole conn $ fromJust rid
    putStrLn $ show role

    addRole conn (fromJust user) (fromJust role)
    permissions <- getPermissions conn (fromJust user)
    putStrLn $ show $ permissions
    removeRole conn (fromJust user) (fromJust role)
    permissions <- getPermissions conn (fromJust user)
    putStrLn $ show $ permissions
{-
    let cfg = Config conn Development
    let app = application $ Config conn Development
    run 8000 app
--  select <- DB.prepare conn "select * from tenants"
--  DB.execute select []
--  results <- DB.fetchAllRows select
--  putStrLn $ show results
--  putStrLn "connected"
--    putStrLn "starting ServantHDBC"
--    run 8000 application
--    -}
