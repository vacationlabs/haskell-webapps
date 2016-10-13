{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module Api.Tenant (TenantAPI, tenantHandlers) where

import Control.Monad.Trans.Except
import Servant

type Tenant = String
type TenantAPI = "tenants" :> 
                         ("new" :> Post '[JSON] Tenant
                          :<|> Capture "x" Int :> Get '[JSON] Tenant
                          :<|> Capture "x" Int :> "activate" :> Post '[JSON] Tenant)

tenantNew :: ExceptT ServantErr IO Tenant
tenantNew = return "new tenant"

tenantGet :: Int -> ExceptT ServantErr IO Tenant 
tenantGet x = return $ "get tenant " ++ show x

tenantActivate :: Int -> ExceptT ServantErr IO Tenant
tenantActivate x = return $ "activating " ++ show x

tenantHandlers = tenantNew :<|> tenantGet :<|> tenantActivate
