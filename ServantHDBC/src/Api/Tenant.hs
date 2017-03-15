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
import Data.Aeson
import GHC.Generics
import Servant

data Tenant = Tenant String
    deriving (Generic)

instance FromJSON Tenant

type TenantAPI = "tenants" :> TenantAPI'

type TenantAPI' = 
                "new" 
                    :> ReqBody '[JSON] Tenant 
                        :> Post '[JSON] String
                :<|> Capture "x" Int 
                    :> Get '[JSON] String
                :<|> Capture "x" Int :> "activate" 
                    :> ReqBody '[JSON] Tenant 
                        :> Post '[JSON] String

tenantNew :: Tenant -> ExceptT ServantErr IO String
tenantNew (Tenant t) = return "new tenant"

tenantGet :: Int -> ExceptT ServantErr IO String
tenantGet x = return $ "get tenant " ++ show x

tenantActivate :: Int -> 
                  Tenant -> 
                  ExceptT ServantErr IO String
tenantActivate x (Tenant t) = return $ "activating " ++ show x ++ " " ++ show t

tenantHandlers = tenantNew :<|> tenantGet :<|> tenantActivate
