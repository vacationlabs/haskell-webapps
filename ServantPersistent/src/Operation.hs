{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Operation 
  ( OperationT
  , Permission(..)
  , PermissionError(..)
  , requirePermission
  , runOperation
  , unsafeRunOperation
    ) where

import Prelude hiding (null, filter)
import Data.Set
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Applicative
import Control.Monad.Identity
import qualified Database.Persist as DB
import Control.Lens ((^.))
import Types
import DBTypes
import Models

data Permission = EditUser UserId
                | EditTenant TenantId
                | ViewUser UserId
                | AssignRole UserId
                | EditProduct ProductId
                | CreateProduct TenantId
                    deriving (Eq,Ord,Show)

data PermissionError = Requires (Set Permission)

newtype OperationT m a = Op { unsafeRunOp :: StateT (Set Permission) m a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix, MonadTrans, MonadIO)


deriving instance (MonadReader r m) => MonadReader r (OperationT m)
--deriving instance Monad m => MonadState (Set Permission) (OperationT m)
deriving instance (MonadError e m) => MonadError e (OperationT m)

requirePermission :: Monad m => Permission -> OperationT m a -> OperationT m a
requirePermission p o = (Op $ modify (insert p) ) >> o

modifyM
  :: (MonadState s (t m), MonadTrans t, Monad m) =>
     (s -> m s) -> t m ()
modifyM f = get >>= lift . f >>= put

runOperation :: (MonadIO m)
             => OperationT (TransactionT m) a
             -> Maybe User
             -> TransactionT m (Either PermissionError a)
runOperation o mu =
  flip evalStateT mempty $ runExceptT $ do
    a <- lift $ unsafeRunOp o
    case mu of
      Nothing -> return ()
      Just u@(UserB{_userRole=role}) -> do
        modify $ runIdentity . satisfyPermissions
                (\case (EditUser uid) -> return $ (u ^. userID) == uid
                       (ViewUser uid) -> return $ (u ^. userID) == uid
                       _ -> return False)
        forM_ (_roleCapabilities role) $ \case
            EditUserDetails ->
              modifyM $ satisfyPermissions $
                \case (EditUser uid) -> lift $ hasTenant uid (u ^. tenantID)
                      _ -> return False
            ViewUserDetails ->
              modifyM $ satisfyPermissions $
                \case (ViewUser uid) -> lift $ hasTenant uid (u ^. tenantID)
                      _ -> return False
            EditTenantDetails ->
              modifyM $ satisfyPermissions $
                \case (EditTenant tid) -> return $ tid == (u ^. tenantID)
                      _ -> return False
            EditUserRoles ->
              modifyM $ satisfyPermissions $
                \case (AssignRole uid) -> lift $ hasTenant uid (u ^. tenantID)
                      _ -> return False
            MakeProduct ->
              modifyM $ satisfyPermissions $
                \case (CreateProduct tid) -> return $ tid == (u ^. tenantID)
                      _ -> return False
    s <- get
    if null s
       then return a
       else throwError $ Requires s

satisfyPermissions :: (Applicative f, Ord a) => (a -> f Bool) -> Set a -> f (Set a)
satisfyPermissions f s = fromList <$> filterM (fmap not . f) (toList s)

hasTenant :: (Monad m, MonadIO m) => UserId -> TenantId -> TransactionT m Bool
hasTenant uid tid = do
    mtid <- fmap _dBUserTenantID <$> DB.get uid
    case mtid of
         Nothing -> return False
         Just tid' -> return (tid == tid')

unsafeRunOperation :: Monad m => OperationT m a -> m a
unsafeRunOperation = flip evalStateT mempty . unsafeRunOp
