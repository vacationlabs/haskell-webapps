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
       :<|> "createRole" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] RoleIncoming :>  Post '[JSON] Role

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

server::ServerT Type AppM
server = allRoles :<|> createRole'
