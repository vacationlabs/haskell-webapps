{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TypeInType  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE GADTs  #-}
module Types
    where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import Data.ByteString
import Data.Proxy
import GHC.Generics
import Control.Monad.Except
import Control.Monad.Reader
import Data.Serialize
import Data.Text
import Database.Persist
import Database.Persist.Postgresql
import Data.Time.Clock
import Control.Lens hiding ((.=))
import Database.Persist.Sql
import Database.Persist.TH
import Data.Type.Equality

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

data LoginForm = Login { username :: String
                       , password :: String
                       } deriving (Show, Generic, Serialize, FromJSON, ToJSON)

data Session = Session { sessionUser :: String
                       } deriving (Show, Generic, Serialize, FromJSON , ToJSON)

class Reify s a | a -> s where
    reify :: Proxy a -> s a

data UserStatus = BlockedU | InactiveU | ActiveU
                        deriving (Read, Show, Generic, Serialize, FromJSON , ToJSON)
derivePersistField "UserStatus"

data TenantStatus = NewT | InactiveT | ActiveT
                        deriving (Eq, Read, Show, Generic, Serialize, FromJSON , ToJSON)
derivePersistField "TenantStatus"
data TSSingleton (s :: TenantStatus) where
    TNew :: TSSingleton NewT
    TInactive :: TSSingleton InactiveT
    TActive :: TSSingleton ActiveT
instance Reify TSSingleton 'NewT where
    reify _ = TNew
instance Reify TSSingleton 'InactiveT where
    reify _ = TInactive
instance Reify TSSingleton 'ActiveT where
    reify _ = TActive

class HasTimestamp s where
    createdAt :: Lens' s UTCTime
    updatedAt :: Lens' s UTCTime

data TenantIdent =
    TI { _name :: Text
       , _backofficeDomain :: Text
       } deriving (Generic, FromJSON, ToJSON)
makeClassy ''TenantIdent

data TenantBase a =
    TB { _tbTenantIdent :: TenantIdent
       , _tbCreatedAt :: UTCTime
       , _tbUpdatedAt :: UTCTime
       , _tbOwner :: a
       } deriving (Functor,Generic, ToJSON)
makeLenses ''TenantBase
instance HasTenantIdent (TenantBase a) where
    tenantIdent = tbTenantIdent
instance HasTimestamp (TenantBase a) where
    createdAt = tbCreatedAt
    updatedAt = tbUpdatedAt

type TenantInput = TenantIdent
