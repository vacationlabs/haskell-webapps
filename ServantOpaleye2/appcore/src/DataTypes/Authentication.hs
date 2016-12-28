{-# LANGUAGE OverloadedStrings   #-}

module DataTypes.Authentication where

import           Data.Serialize hiding (Get)
import           Ids
import           Data.Aeson
import           Data.Text
import           RoleDefs

data CookieData = CookieData { loggedUserId::UserId, loggedUserName::Text, loggedUserRoles::[RoleId] } deriving (Show)

instance Serialize CookieData where
  put (CookieData uid name roles) = put (uid, name, roles)
  get = CookieData <$> (UserId <$> get) <*> get <*> get

instance Serialize RoleId where
  put (RoleId i) = put i
  get = RoleId <$> get

instance Serialize UserId where
  put (UserId i) = put i
  get = UserId <$> get

instance Serialize Text where
  put t = put (unpack t)
  get = pack <$> get

data LoginInfo = LoginInfo Text Text

instance FromJSON LoginInfo where
  parseJSON (Object v) = LoginInfo <$> 
    v .: "username" <*>
    v .: "password"
