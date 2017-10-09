module AppM where

import UserDefs
import TenantDefs
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.ByteString
import Control.Exception
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Except

type AppM a = WriterT ByteString (ReaderT (Connection, Maybe Tenant, Maybe User) (ExceptT SomeException IO)) a

getCurrentUser :: AppM (Maybe User)
getCurrentUser = do
  (_, _, user) <- R.ask
  return user

getCurrentTenant :: AppM (Maybe Tenant)
getCurrentTenant = do
  (_, tenant, _) <- R.ask
  return tenant

getConnection :: AppM Connection
getConnection = do
  (conn, tenant, _) <- R.ask
  return conn
