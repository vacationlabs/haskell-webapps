module Domain.Roles where

import           Database.Persist
import           DBTypes
import           Models

import Operation
import Data.Text
import Control.Monad.Trans

dbCreateRole :: MonadIO m => Role -> OperationT (TransactionT m) (Maybe RoleId)
dbCreateRole = lift . insertUnique


dbGetRole :: MonadIO m => RoleId -> TransactionT m (Maybe Role)
dbGetRole = get


dbGetRoles :: MonadIO m => TenantId -> TransactionT m [Entity Role]
dbGetRoles tid = selectList [RoleTenant ==. tid] []

dbGetRoleByName :: MonadIO m => Text -> TenantId -> TransactionT m (Maybe (Entity Role))
dbGetRoleByName name tid = getBy (UniqueRoleName name tid)
