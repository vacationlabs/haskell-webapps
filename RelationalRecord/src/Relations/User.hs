{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module  Relations.User where

import  Types.User                          as User
import  qualified Types.Tenant              as Tenant
import  qualified Types.Role                as Role
import  qualified Types.UsersRoles          as UsersRoles
import  Types.DB
import  Relations.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH              (makeRecordPersistableDefault)

import  Data.Int                            (Int32)
import  GHC.Generics                        (Generic)
import  Data.Default



-- SELECTS

allUsers :: Relation () Users
allUsers = User.users

getUser :: Relation PKey Users
getUser = relation' . placeholder $ \usrId -> do
    a       <- query users
    wheres  $ a ! User.id' .=. usrId
    return  a

-- example of a multiway join:
-- * get all users with their roles (inner join, via join-through table)
-- * maybe get tenant whose owner_id is my user id (left outer join)
getUserTenantRoles :: Relation () ((Users, Role.Roles), Maybe Tenant.Tenants)
getUserTenantRoles = relation $ do
    a       <- query users
    ur      <- query UsersRoles.usersRoles
    b       <- query Role.roles
    on      $ a ! User.id' .=. ur ! UsersRoles.userId'
    on      $ ur ! UsersRoles.roleId' .=. b ! Role.id'
    c       <- queryMaybe Tenant.tenants
    on      $ just (a ! User.id') .=. flattenMaybe (c ?! Tenant.ownerId')
    return  $ a >< b >< c


-- INSERTS

-- an insert constrained to the obligatory fields, thus enforcing
-- default values encoded in the DB schema for all other fields
data UserInsert = UserInsert
    { iTenantId     :: PKey
    , iUsername     :: Text
    , iPassword     :: Text
    , iFirstName    :: Maybe Text
    , iLastName     :: Maybe Text
    }
$(makeRecordPersistableDefault ''UserInsert)

piUser :: Pi Users UserInsert
piUser = UserInsert
    |$| User.tenantId'
    |*| User.username'
    |*| User.password'
    |*| User.firstName'
    |*| User.lastName'

insertUser :: Insert UserInsert
insertUser = derivedInsert piUser


-- UPDATES

data UserUpdate = UserUpdate
    { uTenantId     :: VariadicArg PKey
    , uUsername     :: VariadicArg Text
    , uFirstName    :: VariadicArg (Maybe Text)
    , uLastName     :: VariadicArg (Maybe Text)
    , uStatus       :: VariadicArg Int32
    }
    deriving (Generic, Default)

userVariadic :: Users -> Users -> UserUpdate
userVariadic old new = UserUpdate
    (varArg tenantId old new)
    (varArg username old new)
    (varArg firstName old new)
    (varArg lastName old new)
    (varArg status old new)

updateUserVariadic :: UserUpdate -> TimestampedUpdate
updateUserVariadic UserUpdate {..} = derivedUpdate $ \projection -> do
    User.tenantId'  <-#? uTenantId
    User.username'  <-#? uUsername
    User.firstName' <-#? uFirstName
    User.lastName'  <-#? uLastName
    User.status'    <-#? uStatus

    (phTStamp, _)   <- placeholder (\tStamp -> User.updatedAt' <-# tStamp)
    (phUsrId, _)    <- placeholder (\usrId -> wheres $ projection ! User.id' .=. usrId)
    return          $ phTStamp >< phUsrId


-- DELETES
