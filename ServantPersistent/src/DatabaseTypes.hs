{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Data.Time.Clock
import Data.Text

data TenantStatus = NewT | InactiveT | ActiveT deriving (Read, Show)
derivePersistField "TenantStatus"
data UserStatus = BlockedU | InactiveU | ActiveU deriving (Read, Show)
derivePersistField "UserStatus"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Tenant json
    name Text
    backofficeDomain Text
    ownerId UserId Maybe
    tenantStatus TenantStatus
    tenantTime TimeStamp
    deriving Show

User json
    firstName Text
    lastName Text
    tenantID TenantID
    username Text
    password Text
    userTime TimeStamp
    userStatus UserStatus
    deriving Show
]

