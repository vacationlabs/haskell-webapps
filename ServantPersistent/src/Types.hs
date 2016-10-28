{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE OverloadedStrings #-}
module Types
    where

import Data.Aeson
import Servant
import Servant.Server.Experimental.Auth.Cookie
import GHC.Generics
import Control.Monad.Except
import Control.Monad.Reader
import Data.Serialize
import Data.Text
import Data.Time.Clock
import Control.Lens hiding ((.=))
import Database.Persist.Sql
import Database.Persist.TH

data Environment = Test | Devel | Production deriving (Eq, Show)

data CookieError = NotPresent | AuthError AuthCookieException deriving (Eq, Show)

data Config = Config
    { authSettings :: AuthCookieSettings
    , randomSource :: RandomSource
    , serverKey :: ServerKey
    , environment :: Environment
    , dbPool :: ConnectionPool
    }

type App = (ReaderT Config (ExceptT ServantErr IO))

data LoginForm = Login { loginUsername :: String
                       , loginPassword :: String
                       } deriving (Show, Generic, Serialize, FromJSON, ToJSON)

data Session = Session { sessionUser :: String
                       } deriving (Show, Generic, Serialize, FromJSON , ToJSON)


data UserStatus = BlockedU | InactiveU | ActiveU
                        deriving (Read, Show, Generic, Serialize, FromJSON , ToJSON)
derivePersistField "UserStatus"

data TenantStatus = NewT | InactiveT | ActiveT
                        deriving (Eq, Read, Show, Generic, Serialize, FromJSON , ToJSON)
derivePersistField "TenantStatus"

class HasTimestamp s where
    createdAt :: Lens' s UTCTime
    updatedAt :: Lens' s UTCTime

class HasName a where
    name :: Lens' a Text
class HasBackofficeDomain a where
    backofficeDomain :: Lens' a Text

class HasHumanName a where
    firstName :: Lens' a Text
    lastName :: Lens' a Text

class HasContactDetails a where
    email :: Lens' a Text
    phone :: Lens' a Text

class HasUsername a where
    username :: Lens' a Text

class HasPassword a where
    password :: Lens' a Text

