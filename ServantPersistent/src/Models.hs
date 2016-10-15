{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Models
    where

import Data.Aeson
import Data.Aeson.TH
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Data.Time.Clock
import Data.Text
import DatabaseTypes

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Tenant json
    name Text
    backofficeDomain Text
    ownerId UserId Maybe
    tenantStatus TenantStatus
    tenantTime TimeStamp

User json
    firstName Text
    lastName Text
    tenantID TenantId
    username Text
    password Text
    userTime TimeStamp
    userStatus UserStatus
    UniqueUsername username
|]

