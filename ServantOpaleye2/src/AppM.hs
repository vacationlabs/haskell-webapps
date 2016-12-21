{-# LANGUAGE FlexibleInstances #-}
module AppM where

import           AppCore
import           Control.Exception
import qualified Control.Monad.Reader       as R
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Database.PostgreSQL.Simple
import           Control.Monad.Catch
import           Control.Monad.Trans.Class

type AppM = WriterT String (ReaderT (Connection, Maybe Tenant, Maybe User) (ExceptT SomeException IO))

instance CurrentUser AppM where
  getCurrentUser = do
    (_, _, user) <- R.ask
    return user

instance CurrentTenant AppM where
  getCurrentTenant = do
    (_, tenant, _) <- R.ask
    return tenant

instance DbConnection AppM where
  getConnection = do
    (conn, tenant, _) <- R.ask
    return conn

instance MonadThrow AppM where
  throwM e = lift $ lift $ ExceptT (return $ Left $ SomeException e)
