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
import           Control.Monad.IO.Class

import Servant.Server.Experimental.Auth.Cookie
import JsonValidation

type instance AuthCookieData = CookieData

type Type = "user" :> "list"   :> AuthProtect "cookie-auth" :> Get '[JSON] [User]
       :<|> "user" :> "create" :> AuthProtect "cookie-auth" :> ReqBodyVal UserIncoming  :>  Post '[JSON] User
       :<|> "user" :> "update" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] (UserId, UserIncoming) :> Post '[JSON] User
       :<|> "user" :> "remove" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] UserId :> Post '[JSON] User

allUsers :: (DbConnection m) => CookieData -> m [User]
allUsers cd =  do
  requireRole cd (RoleName "manager") 
  readUsers

createUser' :: (DbConnection m) => CookieData -> UserIncoming -> m User
createUser' cd ui = do
  requireRole cd (RoleName "manager") 
  createUser ui

updateUser' :: (CurrentTenant m, CurrentUser m, DbConnection m) => CookieData -> (UserId, UserIncoming) -> m User
updateUser' cd (uid, ui) = do
  requireRole cd (RoleName "manager")
  user <- readUserById uid
  updateUser $ merge ui user

removeUser' :: (DbConnection m, MonadIO m) => CookieData -> UserId -> m User
removeUser' cd uid = do 
  requireRole cd (RoleName "administrator") 
  user <- (readUserById uid) 
  removeAuditableRow userTable user
  return user

server::ServerT Type AppM
server = allUsers
    :<|> createUser'
    :<|> updateUser'
    :<|> removeUser'
