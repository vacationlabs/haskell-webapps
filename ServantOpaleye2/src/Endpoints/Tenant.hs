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
import           UserApi

type Type = "tenants" :> BasicAuth "foo-realm" String :> Get '[JSON] [Tenant]

allTenants :: String -> AppM [Tenant]
allTenants s = readTenants

server::ServerT Type AppM
server = allTenants
