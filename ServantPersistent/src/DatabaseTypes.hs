{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
module DatabaseTypes
    where

import Data.Aeson
import Data.Aeson.TH
import Database.Persist.TH
import GHC.Generics
import Data.Time.Clock
import Data.Text

data TenantStatus = NewT | InactiveT | ActiveT deriving (Read, Show,Generic)
instance ToJSON TenantStatus
instance FromJSON TenantStatus
derivePersistField "TenantStatus"
data UserStatus = BlockedU | InactiveU | ActiveU deriving (Read, Show,Generic)
instance ToJSON UserStatus
instance FromJSON UserStatus
derivePersistField "UserStatus"

data TimeStamp = TS { createdAt :: UTCTime
                    , updatedAt :: UTCTime } deriving (Read, Show, Generic)

instance ToJSON TimeStamp
instance FromJSON TimeStamp
derivePersistFieldJSON "TimeStamp"
