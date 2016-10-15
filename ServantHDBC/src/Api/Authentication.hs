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
import Data.Aeson
import GHC.Generics
import Servant

data Auth = Auth String
    deriving (Generic)

instance FromJSON Auth

type AuthAPI = "sessions" :> AuthAPI'

type AuthAPI' = 
            "new" 
                :> ReqBody '[JSON] Auth
                    :> Post '[JSON] String
       :<|> "refresh" 
                :> ReqBody '[JSON] Auth
                    :> Post '[JSON] String
       :<|> "destroy" 
                :> ReqBody '[JSON] Auth
                    :> Post '[JSON] String

authNew :: Auth -> ExceptT ServantErr IO String
authNew _ = return "should create a cookie"

authRefresh :: Auth -> ExceptT ServantErr IO String
authRefresh _ = return "should refresh session timeout"

authDestroy :: Auth -> ExceptT ServantErr IO String
authDestroy _ = return "should destroy the session"

authHandlers = authNew :<|> authRefresh :<|> authDestroy

