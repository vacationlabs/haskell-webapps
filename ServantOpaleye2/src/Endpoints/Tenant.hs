{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Endpoints.Tenant (
  Type
  ,server
) where

import           Servant
import           AppM
import           AppCore
import           TenantApi

type Type = "tenants" :> BasicAuth "foo-realm" String :> Get '[JSON] [Tenant]
       :<|> "tenantsAll" :> BasicAuth "foo-realm" String :> Get '[JSON] [Tenant]

readTenants' :: String -> AppM [Tenant]
readTenants' s = readTenants

readTenantsAll' :: String -> AppM [Tenant]
readTenantsAll' s = readTenants

server::ServerT Type AppM
server = readTenants' :<|> readTenantsAll'
