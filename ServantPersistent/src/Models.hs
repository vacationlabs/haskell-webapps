{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Models
    where

import Database.Persist.Sql
import Database.Persist.TH
import Data.Time.Clock
import Data.Text
import Control.Monad.IO.Class
import Control.Monad.Reader
import Types

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
