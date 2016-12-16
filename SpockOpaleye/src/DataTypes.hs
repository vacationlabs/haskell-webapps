module DataTypes where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.ByteString
import TenantDefs
import UserDefs
import Control.Exception
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Except

type AppM a = WriterT ByteString (ReaderT (Connection, Maybe Tenant, Maybe User) (ExceptT SomeException IO)) a
