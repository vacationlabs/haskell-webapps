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
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Writer.Strict
import qualified Database.Persist as DB
import Control.Lens
import Types
import DBTypes
import Models

data Permission = EditUser UserID
                | EditTenant TenantID
                | ViewUser UserID
                    deriving (Eq,Ord,Show)

data PermissionError = Requires (Set Permission)

newtype OperationT m a = Op { unsafeRunOp :: WriterT (Set Permission) m a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Foldable, MonadFix, MonadTrans, MonadIO)

instance (DBMonad m) => DBMonad (OperationT m) where
    getDBPool = lift getDBPool



deriving instance (MonadReader r m) => MonadReader r (OperationT m)
deriving instance (MonadState s m) => MonadState s (OperationT m)
deriving instance (MonadError e m) => MonadError e (OperationT m)

requirePermission :: Monad m => Permission -> OperationT m a -> OperationT m a
requirePermission = (>>) . (Op . tell . singleton)

runOperation :: DBMonad m => OperationT m a -> User -> ExceptT PermissionError m a
runOperation op u@(UserB{_userRole=role}) = do
    (a,s) <- lift $ runWriterT $ unsafeRunOp op
    let go s (EditUserDetails) =
              satisfyPermissions s
                (\case (EditUser uid) -> hasTenant uid (u ^. tenantID)
                       _ -> return False)
        go s (ViewUserDetails) =
              satisfyPermissions s
                (\case (ViewUser uid) -> hasTenant uid (u ^. tenantID)
                       _ -> return False)
        go s (EditTenantDetails) =
              satisfyPermissions s
                (\case (EditTenant tid) -> return $ tid == (u ^. tenantID)
                       _ -> return False)
        go s _ = return s
    let s' = satisfyPermissions s
                (\case (EditUser uid) -> return $ (u ^. userID) == uid
                       (ViewUser uid) -> return $ (u ^. userID) == uid
                       _ -> return False)
    s'' <- foldM go (runIdentity s') (roleCapabilities role)
    if null s''
       then return a
       else throwError $ Requires s''

satisfyPermissions :: (Applicative f, Ord a) => Set a -> (a -> f Bool) -> f (Set a)
satisfyPermissions s f = fromList <$> filterM (fmap not . f) (toList s)

hasTenant :: DBMonad m => UserID -> TenantID -> m Bool
hasTenant uid tid = runDb $ do
    mtid <- fmap _dBUserTenantID <$> DB.get uid
    case mtid of
         Nothing -> return False
         Just tid' -> return (tid == tid')

unsafeRunOperation :: Functor m => OperationT m a -> m a
unsafeRunOperation op = fst <$> (runWriterT $ unsafeRunOp op)
