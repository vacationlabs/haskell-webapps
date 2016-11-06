module Domain.Roles where

import           Database.Persist
import           DBTypes
import           Models

import Operation
import Data.Text
import Control.Monad.Trans

dbCreateRole :: MonadIO m => Role -> OperationT (TransactionT m) (Maybe (Key Role))
dbCreateRole = lift . insertUnique


dbGetRole :: MonadIO m => RoleID -> TransactionT m (Maybe Role)
dbGetRole = get


dbGetRoles :: MonadIO m => TransactionT m [Entity Role]
dbGetRoles = selectList [] []

dbGetRoleByName :: MonadIO m => Text -> TenantID -> TransactionT m (Maybe (Entity Role))
dbGetRoleByName name tid = getBy (UniqueRoleName name tid)
