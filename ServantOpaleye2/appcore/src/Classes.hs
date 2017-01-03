{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Classes where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader       as R
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Control.Monad.Catch
import           Control.Lens
import           Lenses
import           Data.ByteString
import           Database.PostgreSQL.Simple
import           TenantDefs
import           UserDefs
import           RoleDefs

class (Monad m) => CurrentUser m where
  getCurrentUser :: m (Maybe User)

class (Monad m) => CurrentTenant m where
  getCurrentTenant :: m (Maybe Tenant)

class (Monad m, MonadIO m, MonadThrow m) => DbConnection m where
  getConnection :: m Connection

class UpdatePair src dst where
  merge :: src -> dst -> dst

instance UpdatePair UserIncoming User where
  merge ui u = u & (firstname .~ (ui ^. firstname)) & (lastname .~ (ui ^. lastname))

instance UpdatePair RoleUpdate Role where
  merge ri r = r & (name .~ (ri ^. name)) & (permission .~ (ri ^. permission))
