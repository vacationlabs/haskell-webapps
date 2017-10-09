{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MockAPI.RoleAttributes where

import MockAPI.Prelude
import MockAPI.Permission
import MockAPI.User

data RoleAttributes = RoleAttributes
  { _rolePermission      :: Set Permission
  , _roleAssociatedUsers :: Set User
  } deriving (Show, Read, Eq, Ord, Generic)

emptyRoleAttributes :: RoleAttributes
emptyRoleAttributes = RoleAttributes mempty mempty

makeLenses ''RoleAttributes

instance ToJSON RoleAttributes
instance FromJSON RoleAttributes
