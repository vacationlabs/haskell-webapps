{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module DB where
import           Prelude hiding (sum)
-- import           Opaleye (Column, Nullable, matchNullable, isNull,
--                          Table(Table), required, queryTable, optional,
--                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
--                          (.===),
--                          (.++), ifThenElse, pgString, aggregate, groupBy,
--                          count, avg, sum, leftJoin, runQuery,
--                          showSqlForPostgres, Unpackspec,
--                          PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool, Nullable)
import Opaleye
-- import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time(UTCTime)
import           Control.Arrow (returnA, (<<<))
import           Data.Text (Text)
import Control.Lens
import GHC.Int
import Database.PostgreSQL.Simple.FromField (FromField, fromField, ResultError(..), returnError)
import Data.String.Conv
import qualified Data.Default as Def

newtype TenantId = TenantId Int64 deriving Show
newtype UserId = UserId Int64 deriving Show
data TenantStatus = TenantActive | TenantInactive | TenantNew deriving Show
data UserStatus = UserActive | UserInactive | UserBlocked deriving Show
newtype BcryptText = BcryptText Text deriving Show -- TODO: Should this be a ByteString?

-- printSql :: Default Unpackspec a a => Query a -> IO ()
-- printSql = putStrLn . showSqlForPostgres

--
-- Setting up ENUM mappings
--
instance FromField TenantStatus where
  -- TODO: How to check OID? HINT: Use `typename`
  fromField field Nothing = returnError UnexpectedNull field ""
  fromField field (Just bytestring) = case (toS bytestring) of
    "active" -> return TenantActive
    "inactive" -> return TenantInactive
    "new" -> return TenantNew
    _ -> returnError ConversionFailed field $ "Unknown value in tenant_status: " ++ (toS bytestring)

instance QueryRunnerColumnDefault PGText TenantStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField UserId where
  fromField field mBytestring = fmap UserId (fromField field mBytestring)

instance QueryRunnerColumnDefault PGInt8 UserId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField TenantId where
  fromField field mBytestring = fmap TenantId (fromField field mBytestring)

instance QueryRunnerColumnDefault PGInt8 TenantId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn


--
-- Tenant
--

-- TODO: Figure out how to get a common 'Active'/'Inactive' without being forced
-- to prefix them
data TenantPoly key createdAt updatedAt name status ownerId backofficeDomain = Tenant {
  tenantKey :: key
  ,tenantCreatedAt :: createdAt
  ,tenantUpdatedAt :: updatedAt
  ,tenantName :: name
  ,tenantStatus :: status
  ,tenantOwnerId :: ownerId
  ,tenantBackofficeDomain :: backofficeDomain
  } deriving Show

-- TODO: Figure out how to map TenantStatus to PG
type TenantPGWrite = TenantPoly
  (Maybe (Column PGInt8)) -- key
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Maybe (Column PGTimestamptz)) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain

type TenantPGRead = TenantPoly
  (Column PGInt8) -- key
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain

type NewTenant = TenantPoly
  () -- key
  () -- createdAt
  () -- updatedAt
  Text -- name
  () -- status
  (Maybe UserId) -- ownerId
  Text -- backofficeDomain

type Tenant = TenantPoly
  TenantId -- key
  UTCTime -- createdAt
  UTCTime -- updatedAt
  Text -- name
  TenantStatus -- status
  (Maybe UserId) -- ownerId
  Text -- backofficeDomain

$(makeAdaptorAndInstance "pTenant" ''TenantPoly)
$(makeLensesWith abbreviatedFields ''TenantPoly)


tenantTable :: Table TenantPGWrite TenantPGRead
tenantTable = Table "tenants" (pTenant Tenant{
                                  tenantKey = optional "id"
                                  ,tenantCreatedAt = optional "created_at"
                                  ,tenantUpdatedAt = optional "updated_at"
                                  ,tenantName = required "name"
                                  ,tenantStatus = required "status"
                                  ,tenantOwnerId = required "owner_id"
                                  ,tenantBackofficeDomain = required "backoffice_domain"
                                  })

tenantQuery :: Query TenantPGRead
tenantQuery = queryTable tenantTable

tenantById :: TenantId -> Query TenantPGRead
tenantById (TenantId tid) = proc () -> do
  tenant <- tenantQuery -< ()
  restrict -< (tenant ^. key) .== (pgInt8 tid)
  returnA -< tenant

pgUserId :: UserId -> Column PGInt8
pgUserId (UserId uid) = pgInt8 uid

maybeToNullableColumn :: (a -> Column b) -> Maybe a -> Column (Nullable b)
maybeToNullableColumn _ Nothing = Opaleye.null
maybeToNullableColumn f (Just m) = toNullable (f m)

newTenantToPg :: NewTenant -> TenantPGWrite
newTenantToPg = pTenant Tenant{
  tenantKey = const Nothing
  ,tenantCreatedAt = const Nothing
  ,tenantUpdatedAt = const Nothing
  ,tenantName = pgStrictText
  ,tenantStatus = const $ pgString "new"
  ,tenantOwnerId = maybeToNullableColumn pgUserId
  ,tenantBackofficeDomain = pgStrictText
  }

tenantToPg :: Tenant -> TenantPGWrite
tenantToPg = pTenant Tenant{
  tenantKey = const Nothing
  ,tenantCreatedAt = const Nothing
  ,tenantUpdatedAt = Just . pgUTCTime
  ,tenantName = pgStrictText
  ,tenantStatus = const $ pgString "new" -- TODO
  ,tenantOwnerId = maybeToNullableColumn pgUserId
  ,tenantBackofficeDomain = pgStrictText
  }


instance Def.Default NewTenant where
  def =  Tenant{
    tenantKey = ()
    ,tenantCreatedAt = ()
    ,tenantUpdatedAt = ()
    ,tenantStatus = ()
    ,tenantOwnerId = Nothing
    }

--
-- User
--

data UserPoly key createdAt updatedAt tenantId username password firstName lastName status = User {
  userKey :: key
  ,userCreatedAt :: createdAt
  ,userUpdatedAt :: updatedAt
  ,userTenantId :: tenantId
  ,userUsername :: username
  ,userPassword :: password
  ,userFirstName :: firstName
  ,userLastName :: lastName
  ,userStatus :: status
  } deriving Show

type User = UserPoly
  UserId -- key
  UTCTime -- createdAt
  UTCTime -- updatedAt
  TenantId -- tenantId
  Text -- username
  BcryptText -- password
  (Maybe Text) -- firstName
  (Maybe Text) -- lastName
  UserStatus -- status

type UserPGWrite = UserPoly
  (Maybe (Column PGInt8)) -- key
  (Maybe (Column UTCTime)) -- createdAt
  (Maybe (Column UTCTime)) -- updatedAt
  (Column PGInt8) -- tenantId
  (Column PGText) -- username
  (Column PGText) -- password
  (Column (Nullable PGText)) -- firstName
  (Column (Nullable PGText)) -- lastName
  (Column PGText) -- status

type UserPGRead = UserPoly
  (Column PGInt8) -- key
  (Column UTCTime) -- createdAt
  (Column UTCTime) -- updatedAt
  (Column PGInt8) -- tenantId
  (Column PGText) -- usernam
  (Column PGText) -- password
  (Column (Nullable PGText)) -- firstName
  (Column (Nullable PGText)) -- lastName
  (Column PGText) -- status


$(makeAdaptorAndInstance "pUser" ''UserPoly)
$(makeLensesWith abbreviatedFields ''UserPoly)

userTable :: Table UserPGWrite UserPGRead
userTable = Table "users"
  (pUser User{
      userKey = optional "id"
      ,userCreatedAt = optional "created_at"
      ,userUpdatedAt = optional "updated_at"
      ,userTenantId = required "tenant_id"
      ,userUsername = required "username"
      ,userPassword = required "password"
      ,userFirstName = required "first_name"
      ,userLastName = required "last_name"
      ,userStatus = required "status"
      })

userQuery :: Query UserPGRead
userQuery = queryTable userTable

