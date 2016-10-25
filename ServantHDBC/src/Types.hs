{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Data.Aeson
import Data.ByteString.UTF8
import Data.Convertible.Base
import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Clock
import GHC.Generics
import Database.HDBC.SqlValue

data Tenant = Tenant 
    { _tid:: Int
    , _tcreated_at        :: UTCTime
    , _tupdated_at        :: UTCTime
    , _tname              :: String
    , _tfirst_name        :: String
    , _tlast_name         :: String
    , _temail             :: String
    , _tphone             :: String
    , _tstatus            :: TenantStatus
    , _towner_id          :: Maybe Int
    , _tbackoffice_domain :: String
    }
    deriving (Generic, Show)
instance FromJSON Tenant


data TenantStatus = TenantNew | TenantInactive | TenantActive
    deriving (Generic, Show)

instance FromJSON TenantStatus

instance Convertible SqlValue TenantStatus where
    safeConvert (SqlString a) = case a of
        "new" -> Right TenantNew
        "active" -> Right TenantActive
        "inactive" -> Right TenantInactive
        _ -> Left $ ConvertError (show a) "SqlValue" "TenantStatus" "Unrecognized value"
    safeConvert (SqlByteString a) = safeConvert $ SqlString $ toString a
    safeConvert a = Left $ ConvertError (show a) "SqlValue" "TenantStatus" "No conversion available"

instance Convertible TenantStatus SqlValue where
    safeConvert TenantNew = Right $ SqlString "new" 
    safeConvert TenantInactive = Right $ SqlString "inactive" 
    safeConvert TenantActive = Right $ SqlString "active" 

data User = User
    { _uid                :: Int
    , _ucreated_at        :: UTCTime
    , _uupdated_at        :: UTCTime
    , _utenant_id         :: Int
    , _uusername          :: String
    , _upassword          :: String
    , _ufirst_name        :: String
    , _ulast_name         :: String
    , _status             :: String
    }
    deriving (Generic, Show)

data UserStatus = UserActive | UserInactive | UserBlocked
    deriving (Generic, Show)
instance FromJSON UserStatus

instance Convertible SqlValue UserStatus where
    safeConvert (SqlString a) = case a of
        "active" -> Right UserActive
        "inactive" -> Right UserInactive
        "blocked" -> Right UserBlocked
        _ -> Left $ ConvertError (show a) "SqlValue" "UserStatus" "Unrecognized value"
    safeConvert (SqlByteString a) = safeConvert $ SqlString $ toString a
    safeConvert a = Left $ ConvertError (show a) "SqlValue" "UserStatus" "No conversion available"

instance Convertible UserStatus SqlValue where
    safeConvert UserActive = Right $ SqlString "active"
    safeConvert UserInactive = Right $ SqlString "inactive"
    safeConvert UserBlocked = Right $ SqlString "blocked"


data Role = Role 
    { _rid :: Int
    , _rtid :: Int
    , _rname :: String
    , _rpermissions :: [Permission]
    , _created_at :: UTCTime
    , _updated_at :: UTCTime
    }
    deriving (Generic, Show)

data Permission = ReadFoo | WriteFoo | ReadBar | WriteBar
    deriving (Eq, Ord, Generic, Read, Show)

instance Convertible SqlValue Permission where
    safeConvert (SqlString a) = case a of
        "ReadFoo" -> Right ReadFoo
        "WriteFoo" -> Right WriteFoo
        "ReadBar" -> Right ReadBar
        "WriteBar" -> Right WriteBar
        _ -> Left $ ConvertError (show a) "SqlValue" "Permission" "Unrecognized value"
    safeConvert (SqlByteString a) = safeConvert $ SqlString $ toString a
    safeConvert a = Left $ ConvertError (show a) "SqlValue" "Permission" "No conversion available"

permissionsToSql :: [Permission] -> SqlValue
permissionsToSql xs = toSql $ "{" ++ (intercalate "," $ map show xs) ++ "}"

permissionsFromSql :: String -> [Permission]
permissionsFromSql str = map read $splitOn "," $ init $ tail str

instance Convertible SqlValue [Permission] where
    safeConvert (SqlString a) = Right $ permissionsFromSql a
    safeConvert (SqlByteString a) = safeConvert $ SqlString $ toString a
    safeConvert a = Left $ ConvertError (show a) "SqlValue" "[Permission]" "No conversion available"

instance Convertible [Permission] SqlValue where
    safeConvert a = Right $ permissionsToSql a
