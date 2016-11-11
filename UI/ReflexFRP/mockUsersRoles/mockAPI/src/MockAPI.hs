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

import MockAPI.Permission
import MockAPI.RoleAttributes
import MockAPI.Roles
import MockAPI.User
import MockAPI.Shaped

type MockApi = "delete" :> Capture "role" RoleName :> Capture "user" User :> Delete '[JSON] NoContent
          :<|> "roles" :> Get '[JSON] Roles
          :<|> "assets" :> Raw
          :<|> Raw
