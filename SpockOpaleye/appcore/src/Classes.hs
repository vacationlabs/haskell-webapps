{-# LANGUAGE FlexibleInstances      #-}
module Classes where

import UserDefs
import TenantDefs
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Control.Monad.IO.Class
import           Data.ByteString
import Control.Exception
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Except

class (Monad m) => CurrentUser m where
  getCurrentUser :: m (Maybe User)

class (Monad m) => CurrentTenant m where
  getCurrentTenant :: m (Maybe Tenant)

class (Monad m, MonadIO m) => DbConnection m where
  getConnection :: m Connection
