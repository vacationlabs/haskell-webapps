{-# LANGUAGE OverloadedStrings   #-}

module DataTypes.Authentication where

import           Data.Serialize hiding (Get)
import           Ids
import           Data.Aeson
import           Data.Text

data CookieData = CookieData UserId [String] deriving (Show)

instance Serialize CookieData where
  put (CookieData (UserId i) roles) = put (i, roles)
  get = CookieData <$> (UserId <$> get) <*> get

data LoginInfo = LoginInfo Text Text

instance FromJSON LoginInfo where
  parseJSON (Object v) = LoginInfo <$> 
    v .: "username" <*>
    v .: "password"
