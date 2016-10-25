{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RestApi (ShoppingCartAPI, handlers) where 

import RestApi.Authentication
import RestApi.Photo
import RestApi.Product
import RestApi.Tenant
import Servant

type ShoppingCartAPI = 
                 TenantAPI
--            :<|> AuthAPI
--            :<|> ProductAPI 
--            :<|> PhotoAPI

--handlers :: ShoppingCartAPI
handlers =  
             tenantHandlers 
--        :<|> authHandlers
--        :<|> productHandlers 
--        :<|> photoHandlers

--shoppingCart :: Proxy ShoppingCartAPI
--shoppingCart = Proxy
