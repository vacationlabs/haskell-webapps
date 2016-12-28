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
   :<|> "protected" :> AuthProtect "cookie-auth" :> Get '[JSON] String

login :: AuthCookieSettings -> RandomSource -> ServerKey -> LoginInfo -> AppM (Headers '[Header "set-cookie" EncryptedSession] String)
login ac rs sk (LoginInfo uname pword) = do
  authResult <- (authenticateUser uname pword)
  case authResult of
    Right user -> (addSession
        ac
        rs
        sk
        (CookieData (user ^. id) [])
        login'
      ) 
    _ -> return $ addHeader (EncryptedSession "Unauthenticated") login'
  where 
    login' :: String
    login' = "test"

protectedPage :: CookieData -> AppM String
protectedPage (CookieData (UserId i) _) = return "asdaa"

server:: AuthCookieSettings -> RandomSource -> ServerKey -> ServerT Type AppM
server ac rs sk = login ac rs sk :<|> protectedPage
