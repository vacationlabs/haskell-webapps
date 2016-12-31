{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

module Endpoints.User (
  Type
  ,server
) where

import           Servant
import           AppM
import           AppCore
import           UserApi
import           Utils

import Servant.Server.Experimental.Auth.Cookie

type instance AuthCookieData = CookieData

type Type = "users" :> AuthProtect "cookie-auth" :> Get '[JSON] [User]
       :<|> "createUser" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] UserIncoming :>  Post '[JSON] User

allUsers :: (DbConnection m) => CookieData -> m [User]
allUsers cd = requireRole cd (RoleName "manager") >> readUsers

createUser' :: (DbConnection m) => CookieData -> UserIncoming -> m User
createUser' cd ui = requireRole cd (RoleName "manager") >> createUser ui

updateUser' :: (CurrentTenant m, CurrentUser m, DbConnection m) => CookieData -> (UserId, UserIncoming) -> m User
updateUser' cd (uid, ui) = do
  requireRole cd (RoleName "manager")
  user <- readUserById uid
  updateUser $ merge ui user

server::ServerT Type AppM
server = allUsers :<|> createUser'
