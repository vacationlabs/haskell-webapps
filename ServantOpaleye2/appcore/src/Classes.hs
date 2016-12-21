{-# LANGUAGE FlexibleInstances #-}
module Classes where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader       as R
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Control.Monad.Catch
import           Data.ByteString
import           Database.PostgreSQL.Simple
import           TenantDefs
import           UserDefs

class (Monad m) => CurrentUser m where
  getCurrentUser :: m (Maybe User)

class (Monad m) => CurrentTenant m where
  getCurrentTenant :: m (Maybe Tenant)

class (Monad m, MonadIO m, MonadThrow m) => DbConnection m where
  getConnection :: m Connection
