{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.BaseTypes where
import Control.Monad.Identity
import DB
import Data.Text
import Data.Time
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Reader (ReaderT, ask)
import Control.Lens

data AppConfig = AppConfig {
  appConfigDbPool :: Connection
  }

$(makeLensesWith abbreviatedFields ''AppConfig)

-- TODO: Figure out the right monad-transformed stack for the domain API. We'll have to do the following:
-- * DB operations
-- * Logging
-- * Redis operations, potentitally
type AppM = ReaderT AppConfig IO

class Monad m => HasDbConn m where
  askDbConnection :: m Connection

instance HasDbConn (AppM) where
  askDbConnection = fmap appConfigDbPool ask

-- data TenantWithStatus status = Tenant
