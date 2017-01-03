{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}

module Endpoints.Authentication  where

import           Servant
import           AppM
import           Data.Text
import           UserApi
import           Control.Monad.IO.Class
import           AppCore
import           Prelude hiding (id)
import           Control.Lens
import Servant.Server.Experimental.Auth.Cookie

type instance AuthCookieData = CookieData

type Type =
        "login" :> ReqBody '[JSON] LoginInfo :> Post '[JSON] (Headers '[Header "set-cookie" EncryptedSession] String)

login :: AuthCookieSettings -> RandomSource -> ServerKey -> LoginInfo -> AppM (Headers '[Header "set-cookie" EncryptedSession] String)
login ac rs sk (LoginInfo uname pword) = do
  authResult <- (authenticateUser uname pword)
  case authResult of
    Right (user, roles) -> (addSession
        ac
        rs
        sk
        (CookieData (user ^. key) (user ^. username) roles)
        login'
      ) 
    _ -> return $ addHeader (EncryptedSession "Unauthenticated") login'
  where 
    login' :: String
    login' = "test"

server:: AuthCookieSettings -> RandomSource -> ServerKey -> ServerT Type AppM
server ac rs sk = login ac rs sk
