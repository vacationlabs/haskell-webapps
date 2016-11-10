{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving  #-}
{-# LANGUAGE UndecidableInstances                                          #-}

module MockAPI.User where

import MockAPI.Prelude
import MockAPI.Shaped
import Servant.API

import qualified Text.Email.Validate as Email

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

-- @ Validation

data UserShaped i = UserShaped
  { userShapedMail :: Field i Email
  }

deriving instance Eq   (Field i Email) => Eq   (UserShaped i)
deriving instance Show (Field i Email) => Show (UserShaped i)

instance Shaped User (UserShaped Info) where
  toShape   (User m) = UserShaped m
  fromShape (UserShaped m) = User m

userValidation :: UserShaped Validation
userValidation = UserShaped validateMail
  where
    validateMail m = if isValidEmail m
                     then Nothing
                     else Just "Please insert a valid email address"

validateUser :: User -> UserShaped Validation -> UserShaped Error
validateUser u uv = validateUser' (toShape u) uv
  where
    validateUser' :: UserShaped Info -> UserShaped Validation -> UserShaped Error
    validateUser' (UserShaped m) (UserShaped f) = UserShaped (f m)

isValidEmail :: Text -> Bool
isValidEmail = Email.isValid . cs
