{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

module Endpoints.Tenant (
  Type
  ,server
) where

import           Servant
import           AppM
import           AppCore
import           TenantApi
import           UserApi

import Servant.Server.Experimental.Auth.Cookie

type instance AuthCookieData = CookieData

type Type = "tenants" :> AuthProtect "cookie-auth" :> Get '[JSON] [Tenant]

allTenants :: CookieData -> AppM [Tenant]
allTenants _ = readTenants

server::ServerT Type AppM
server = allTenants
