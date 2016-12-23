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
import           Data.Aeson
import           Data.Text
import           UserServices
import           Control.Monad.IO.Class

data LoginInfo = LoginInfo Text Text

instance FromJSON LoginInfo where
  parseJSON (Object v) = LoginInfo <$> 
    v .: "username" <*>
    v .: "password"

type Type =
        "login" :> ReqBody '[JSON] LoginInfo :> Post '[JSON] (Headers '[Header "Set-Cookie" String] String)

login :: LoginInfo -> AppM (Headers '[Header "Set-Cookie" String] String)
login (LoginInfo uname pword) = do
  cookie <- getCookie
  (addHeader cookie) <$> login'
  where 
    getCookie :: AppM String
    getCookie = do
      isValid <- (doAuthenticate uname pword)
      return $ if isValid then "Authenticated=True" else "Authenticated=False"
    login' :: (Monad m) => m String
    login' = return "test"

server::ServerT Type AppM
server = login
