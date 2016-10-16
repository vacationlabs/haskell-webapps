{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module DBApi
    where

import Data.Aeson
import Data.Aeson.TH
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Data.Time.Clock
import Data.Time
import Data.Text
import Types
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Lens
import Data.ByteString
import Data.Proxy
import Models
import Safe

type TenantOutput = TenantBase (Maybe DBUserId)

dbGetTenant :: TenantID -> App (Maybe TenantOutput)
dbGetTenant tid = runDb $ do
    result <- get tid
    return $ fmap (fmap snd . (view $ Control.Lens.from tbdbIso)) result

dbCreateTenant :: Tenant NewT -> App (Maybe (Key DBTenant))
dbCreateTenant t = do
    runDb $ insertUnique $ toDBTenant t

type family Tenant (s :: TenantStatus) =  r | r -> s where
    Tenant NewT = TenantSafe NewT ()
    Tenant ActiveT = TenantSafe ActiveT DBUserId
    Tenant InactiveT = TenantSafe InactiveT (Maybe DBUserId)

type TenantID = Key DBTenant

data TenantEntity s = TenantEntity { tenantKey :: TenantID
                                   , tenantObj :: Tenant s
                                   }

tenant :: Lens' (TenantEntity s) (Tenant s)
tenant = lens tenantObj (\s b -> s { tenantObj = b })

instance (HasTimestamp (Tenant s)) => HasTimestamp (TenantEntity s) where
    createdAt = tenant . createdAt
    updatedAt = tenant . updatedAt

instance (HasTenantIdent (Tenant s)) => HasTenantIdent (TenantEntity s) where
    tenantIdent = tenant . tenantIdent

fromDBTenant :: forall s. Reify TSSingleton s => DBTenant -> Maybe (Tenant s)
fromDBTenant t =
  let owner  = t ^. dBTenantOwnerId
      status = t ^. dBTenantStatus
      tenIn  = t ^. tenantIdent
      creAt  = t ^. createdAt
      updAt  = t ^. updatedAt
      tb     = liftTB . TB tenIn creAt updAt
  in case reify (Proxy @s) of
       TNew ->
         if status == NewT
           then maybe (Just $ tb ()) (const Nothing) owner
           else Nothing
       TActive ->
         if status == ActiveT
           then tb <$> owner
           else Nothing
       TInactive ->
         if status == InactiveT 
           then Just $ tb owner
           else Nothing

toDBTenant :: forall s. ( Reify TSSingleton s
                        , HasTimestamp (Tenant s)
                        , HasTenantIdent (Tenant s))
           => Tenant s -> DBTenant
toDBTenant t = 
  let creAt  = t ^. createdAt
      updAt  = t ^. updatedAt
      dbt status owner = 
          DBTenant { _dBTenantName = t ^. name
                   , _dBTenantBackofficeDomain = t ^. backofficeDomain
                   , _dBTenantOwnerId = owner
                   , _dBTenantStatus = status
                   , _dBTenantCreatedAt = creAt
                   , _dBTenantUpdatedAt = updAt
                   }
  in case reify (Proxy @s) of
       TNew -> dbt NewT Nothing
       TActive -> dbt ActiveT (Just $ t ^. tenantBase . tbOwner)
       TInactive -> dbt InactiveT (t ^. tenantBase . tbOwner)
