{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Authentication (AuthAPI, authHandlers) where

import Control.Monad.Trans.Except
import Servant

type Auth = String
type AuthAPI = "sessions" :>
                         ("new" :> Post '[JSON] Auth
                          :<|> "refresh" :> Post '[JSON] Auth
                          :<|> "destroy" :> Post '[JSON] Auth)

authNew :: ExceptT ServantErr IO Auth
authNew = return "should create a cookie"

authRefresh :: ExceptT ServantErr IO Auth
authRefresh = return "should refresh session timeout"

authDestroy :: ExceptT ServantErr IO Auth
authDestroy = return "should destroy the session"

authHandlers = authNew :<|> authRefresh :<|> authDestroy

