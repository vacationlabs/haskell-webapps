{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
module Models
    where

import           Data.Text
import           Data.Time.Clock
import           Database.Persist.Sql
import           Database.Persist.TH
import           Price
import           Types

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
DBTenant json
    name Text
    backofficeDomain Text
    ownerId DBUserId Maybe
    status TenantStatus
    createdAt UTCTime
    updatedAt UTCTime
     deriving Eq Show
    UniqueBackofficeDomain backofficeDomain

DBUser
    firstName Text
    lastName Text
    tenantID DBTenantId
    username Text
    password Text
    status UserStatus
    email Text
    phone Text
    roleId RoleId
    createdAt UTCTime
    updatedAt UTCTime
    UniqueUsername username
    UniqueEmail email

DBTenantActivation
    tenantID DBTenantId
    createdAt UTCTime
    UniqueTenantActivation tenantID

DBUserActivation
    userID DBUserId
    createdAt UTCTime
    UniqueUserActivation userID

DBProduct
    name Text
    description Text
    currency Text
    advertisedPrice Price
    comparisionPrice Price
    costPrice Price Maybe
    productType ProductType
    properties AppJSON
    urlSlug Text
    tenantID DBTenantId
--    variantSkus [Text]
    createdAt UTCTime
    updatedAt UTCTime
    UniqueSlug urlSlug

DBVariant
    name Text
    productID DBProductId
    sku Text
    price Price
    weightInGrams Double Maybe
    weightDisplayUnit Text Maybe
    createdAt UTCTime
    updatedAt UTCTime
    UniqueSku sku

Role
    name Text
    capabilities [Capability]
    tenant DBTenantId
    UniqueRoleName name tenant
|]

deriving instance Eq (Unique DBTenant)

deriving instance Show (Unique DBTenant)

instance HasTimestamp DBTenant where
    createdAt = dBTenantCreatedAt
    updatedAt = dBTenantUpdatedAt
instance HasTimestamp DBUser where
    createdAt = dBUserCreatedAt
    updatedAt = dBUserUpdatedAt

instance HasName DBTenant where
    name = dBTenantName
instance HasBackofficeDomain DBTenant where
    backofficeDomain = dBTenantBackofficeDomain

instance HasHumanName DBUser where
    firstName = dBUserFirstName
    lastName = dBUserLastName
instance HasContactDetails DBUser where
    email = dBUserEmail
    phone = dBUserPhone
instance HasUsername DBUser where
    username = dBUserUsername
instance HasPassword DBUser where
    password = dBUserPassword

type TransactionT = SqlPersistT

runTransaction :: DBMonad m => TransactionT m a -> m a
runTransaction t = do
    pool <- getDBPool
    runSqlPool t pool
