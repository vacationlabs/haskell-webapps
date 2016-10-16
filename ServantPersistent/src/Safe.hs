{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeInType  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Safe (TenantSafe, tenantBase, liftTB)
    where

import Data.Coerce
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
import Data.Coerce
import Types

liftTB :: TenantBase a -> TenantSafe s a
liftTB = TS

newtype TenantSafe (s :: TenantStatus) a = TS (TenantBase a) deriving (Functor)

instance HasTenantIdent (TenantSafe s a) where
    tenantIdent = tenantBase . tenantIdent

instance HasTimestamp (TenantSafe s a) where
    createdAt = tenantBase . createdAt
    updatedAt = tenantBase . updatedAt

tenantBase :: Lens' (TenantSafe s a) (TenantBase a)
tenantBase = lens (\(TS a) -> a) (\(TS a) b -> TS b)

