{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

module Endpoints.Role (
  Type
  ,server
) where

import           Servant
import           AppM
import           AppCore
import           Exceptions
import           RoleApi
import           UserApi
import           Utils
import           Control.Lens
import           Data.Text
import           Control.Monad.Catch
import           Prelude hiding (id)

import Servant.Server.Experimental.Auth.Cookie

type instance AuthCookieData = CookieData

type Type = "roles" :> AuthProtect "cookie-auth" :> Get '[JSON] [Role]
       :<|> "role/create" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] RoleIncoming :>  Post '[JSON] Role
       :<|> "role/update" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] RoleUpdate :>  Post '[JSON] Role
       :<|> "role/remove" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] RoleId :>  Post '[JSON] Role
       :<|> "role/assignUser" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] (RoleId, UserId) :>  Post '[JSON] RoleId
       :<|> "role/assignTenant" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] (RoleId, TenantId) :>  Post '[JSON] RoleId

allRoles :: (DbConnection m) => CookieData -> m [Role]
allRoles cd = requireRole cd (RoleName "manager") >> undefined

createRole' :: (DbConnection m, MonadThrow m) => CookieData -> RoleIncoming -> m Role
createRole' cd ri = do
  requireRole cd (RoleName "administrator") 
  checkTenant cd 
  createRole ri
  where 
    checkTenant :: (MonadThrow m, DbConnection m) =>  CookieData -> m ()
    checkTenant CookieData { loggedUserId = userId } = do
      tenant <- getTenantForUser userId
      if tenant ^. id /= ri ^. tenantid then throwM (SomeException (NotFoundException "")) else return ()

updateRole' :: (
    DbConnection m,
    CurrentUser m,
    CurrentTenant m,
    MonadThrow m) => CookieData -> RoleUpdate -> m Role
updateRole' cd ri = do
  requireRole cd (RoleName "administrator") 
  role <- readRoleById (ri ^. id)
  updateRole $ role

removeRole' :: (
    DbConnection m,
    CurrentUser m,
    CurrentTenant m,
    MonadThrow m) => CookieData -> RoleId -> m Role
removeRole' cd roleId = do
  requireRole cd (RoleName "administrator") 
  role <- readRoleById roleId
  removeRole role
  return role

addRoleToTenant' :: (
    DbConnection m,
    CurrentUser m,
    CurrentTenant m,
    MonadThrow m) => CookieData -> (RoleId, TenantId) -> m RoleId
addRoleToTenant' cd (roleId, tenantId) = do
  requireRole cd (RoleName "administrator") 
  linkTenantRole tenantId roleId
  return roleId

addRoleToUser' :: (
    DbConnection m,
    CurrentUser m,
    CurrentTenant m,
    MonadThrow m) => CookieData -> (RoleId, UserId) -> m RoleId
addRoleToUser' cd (roleId, userId) = do
  requireRole cd (RoleName "administrator") 
  linkUserRole userId roleId
  return roleId
 
server::ServerT Type AppM
server = allRoles 
    :<|> createRole'
    :<|> updateRole'
    :<|> removeRole'
    :<|> addRoleToUser'
    :<|> addRoleToTenant'
