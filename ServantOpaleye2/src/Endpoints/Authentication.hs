{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Endpoints.Authentication (
  Type
  ,server
) where

import           Servant
import           AppM

type Type =
        "login" :> Get '[JSON] (Headers '[Header "SetCookie" String] String)

login :: AppM (Headers '[Header "SetCookie" String] String)
login = (addHeader "cookievalue") <$> login'
  where 
    login' :: (Monad m) => m String
    login' = return "abcd"

server::ServerT Type AppM
server = login
