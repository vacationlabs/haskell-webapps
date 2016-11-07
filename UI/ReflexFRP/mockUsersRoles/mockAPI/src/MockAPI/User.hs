{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module MockAPI.User where

import MockAPI.Prelude
import Servant.API

import qualified Text.Email.Validate as Email

import Text.Digestive
import Data.String.Conversions

-- @ Core datatypes
type Email = Text

data User = User
  { userMail :: Email
  } deriving (Show, Eq, Ord, Generic)

-- @ Instances

instance FromHttpApiData User where
  parseUrlPiece mail = Right (User mail)

instance ToHttpApiData User where
  toUrlPiece (User mail) = mail

instance ToJSON User

instance FromJSON User

-- @ Forms

userForm :: Monad m => Form Text m User
userForm = User <$>
  "email" .: check "Please insert a valid email address" isValidEmail (text Nothing)

isValidEmail :: Text -> Bool
isValidEmail = Email.isValid . cs

userView :: View Text -> Text
userView _ = undefined
