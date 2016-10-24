{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Operation 
  ( OperationT
  , Permission(..)
  , requirePermission
  , runOperation
    ) where
import Prelude hiding (null, filter)
import Data.Set
import Data.Text (Text)
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import Control.Monad.Writer.Strict
import DBTypes

data Permission = EditUser UserID
                | EditTenant TenantID
                    deriving (Eq,Ord,Show)

data PermissionError = Requires (Set Permission)

newtype OperationT m a = Op { unsafeRunOp :: WriterT (Set Permission) m a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Foldable, MonadFix, MonadTrans, MonadIO)

deriving instance (MonadReader r m) => MonadReader r (OperationT m)
deriving instance (MonadState s m) => MonadState s (OperationT m)
deriving instance (MonadError e m) => MonadError e (OperationT m)

requirePermission :: Monad m => Permission -> OperationT m ()
requirePermission = Op . tell . singleton

runOperation :: Monad m => OperationT m a -> User -> ExceptT PermissionError m a
runOperation op User{userRole=SiteAdmin} =
    lift . fmap fst . runWriterT $ unsafeRunOp op
runOperation op u@(User{userRole=TenantAdmin tid}) = do
    (a,s) <- lift $ runWriterT $ unsafeRunOp op
    let s' = filter (\case (EditUser id) -> id /= userUserID u
                           EditTenant id -> id /= tid
                           )
                    s
    if null s'
       then return a
       else throwError $ Requires s'
runOperation op u@User{userRole=NormalUser} = do
    (a,s) <- lift $ runWriterT $ unsafeRunOp op
    let s' = filter (\case (EditUser id) -> id /= userUserID u
                           EditTenant id -> False
                           )
                    s
    if null s'
       then return a
       else throwError $ Requires s'
