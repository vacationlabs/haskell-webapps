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
import           Opaleye (Column, Nullable, matchNullable, isNull,
                         Table(Table), required, queryTable, optional,
                         Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                         (.===),
                         (.++), ifThenElse, pgString, aggregate, groupBy,
                         count, avg, sum, leftJoin, runQuery,
                         showSqlForPostgres, Unpackspec,
                         PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool, Nullable)
import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar (Day)
import           Data.Time(UTCTime)
import           Control.Arrow (returnA, (<<<))
import           Data.Text (Text)
import qualified Database.PostgreSQL.Simple as PGS
import Control.Lens

--
-- Common newtypes and data types
--

newtype TenantId = TenantId PGInt8
newtype UserId = UserId PGInt8
data TenantStatus = TenantActive | TenantInactive | TenantNew
data UserStatus = UserActive | UserInactive | UserBlocked
newtype BcryptText = BcryptText Text -- TODO: Should this be a ByteString?

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres

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
  }

-- TODO: Fgure out how to map TenantStatus to PG
type TenantPGWrite = TenantPoly
  (Maybe (Column PGInt8)) -- key
  (Maybe (Column UTCTime)) -- createdAt
  (Maybe (Column UTCTime)) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain

type TenantPGRead = TenantPoly
  (Column PGInt8) -- key
  (Column UTCTime) -- createdAt
  (Column UTCTime) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain

$(makeAdaptorAndInstance "pTenant" ''TenantPoly)

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
  }
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

--
-- Lenses
--

$(makeLensesWith abbreviatedFields ''TenantPoly)
$(makeLensesWith abbreviatedFields ''UserPoly)
