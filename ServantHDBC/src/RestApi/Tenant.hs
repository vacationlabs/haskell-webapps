{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RestApi.Tenant (TenantAPI, tenantHandlers) where

import Config
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Time.Clock
import DomainApi
import GHC.Generics
import Servant
import Types


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

tenantNew :: Tenant -> AppM String
tenantNew t = return "new tenant"


tenantGet :: Int -> AppM String
tenantGet x = do
    conn <- asks getConnection
    r <- lift $ lift $ getTenant conn x
    return ""
    

tenantActivate :: Int -> Tenant -> AppM String
tenantActivate x t = return $ "activating " ++ show x ++ " " 

tenantHandlers = tenantNew :<|> tenantGet :<|> tenantActivate
