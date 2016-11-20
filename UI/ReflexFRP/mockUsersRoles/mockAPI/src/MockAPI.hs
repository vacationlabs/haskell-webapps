{-# LANGUAGE DataKinds, OverloadedStrings, TypeFamilies, TypeOperators #-}

module MockAPI
  ( module MockAPI.Permission
  , module MockAPI.RoleAttributes
  , module MockAPI.Roles
  , module MockAPI.User
  , module MockAPI.Shaped
  , module MockAPI
  ) where

import Servant.API
import Servant.Router

import MockAPI.Permission
import MockAPI.RoleAttributes
import MockAPI.Roles
import MockAPI.User
import MockAPI.Shaped
import Servant.HTML.Blaze
import Text.Blaze.Html4.Transitional (Html)

type MockApi = --"server" :>
     ("delete" :> Capture "role" RoleName :> Capture "user" User :> Delete '[JSON] NoContent
  :<|> "add" :> Capture "role" RoleName :> ReqBody '[JSON] RoleAttributes :> Put '[JSON] NoContent
  :<|> "roles" :> Get '[JSON] Roles
  :<|> Raw)

type Navigation = "overview" :> View
             :<|> "edit" :> Capture "roleName" RoleName :> View

type NavigationServer = ViewTransform Navigation (Get '[HTML] Html)

type WholeApi = NavigationServer
           :<|> MockApi
