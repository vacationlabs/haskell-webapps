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

import           Control.Monad.IO.Class
import           Data.Text
import           Data.Time.Clock
import           Database.Persist.Sql
import           Database.Persist.TH

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
    createdAt UTCTime
    updatedAt UTCTime
    UniqueUsername username
    UniqueEmail email

DBTenantActivation
    tenantID DBTenantId
    key Text
    createdAt UTCTime
    UniqueTenantKey key
    UniqueTenantActivation tenantID

DBUserActivation
    userID DBUserId
    key Text
    createdAt UTCTime
    UniqueUserKey key
    UniqueUserActivation userID

DBProduct
    name Text
    description Text
    currency Text
    advertisedPrice Rational
    comparisionPrice Rational
    costPrice Rational Maybe
    productType ProductType
    properties AppJSON
    urlSlug Text
    tenantID DBTenantId
    createdAt UTCTime
    updatedAt UTCTime
    UniqueSlug urlSlug

DBVariant
    name Text
    productID DBProductId
    sku Text
    price Rational
    weightInGrams Int
    weightDisplayUnit Text
    createdAt UTCTime
    updatedAt UTCTime
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

runDb :: DBMonad m => SqlPersistT IO b -> m b
runDb query = do
    pool <- getDBPool
    liftIO $ runSqlPool query pool
