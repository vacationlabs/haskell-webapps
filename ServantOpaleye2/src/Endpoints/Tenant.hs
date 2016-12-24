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

type Type = "tenants" :> Get '[JSON] [Tenant]

allTenants :: AppM [Tenant]
allTenants = readTenants

server::ServerT Type AppM
server = allTenants
