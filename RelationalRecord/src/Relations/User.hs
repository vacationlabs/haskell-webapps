{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module  Relations.User where

import  Types.User as User
import  Types.DB
import  Relations.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)

import  Data.Int                    (Int32)



-- SELECTS

allUsers :: Relation () Users
allUsers = User.users

getUser :: Relation PKey Users
getUser = relation' . placeholder $ \usrId -> do
    a       <- query users
    wheres  $ a ! User.id' .=. usrId
    return  a


-- INSERTS

data UserInsert = UserInsert
    { iTenantId     :: PKey
    , iUsername     :: String
    , iPassword     :: String
    , iFirstName    :: Maybe String
    , iLastName     :: Maybe String
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
    { uTenantId     :: Maybe PKey
    , uUsername     :: Maybe String
    , uPassword     :: Maybe String
    , uFirstName    :: Maybe (Maybe String)
    , uLastName     :: Maybe (Maybe String)
    , uStatus       :: Maybe Int32
    }

userUpdate :: UserUpdate
userUpdate = UserUpdate
    Nothing Nothing Nothing Nothing Nothing Nothing

updateUserVariadic :: UserUpdate -> TimestampedUpdate
updateUserVariadic UserUpdate {..} = derivedUpdate $ \projection -> do
    User.tenantId'  <-#? uTenantId
    User.username'  <-#? uUsername
    User.password'  <-#? uPassword
    User.firstName' <-#? uFirstName
    User.lastName'  <-#? uLastName
    User.status'    <-#? uStatus

    (phTStamp, _)   <- placeholder (\tStamp -> User.updatedAt' <-# tStamp)
    (phUsrId, _)    <- placeholder (\usrId -> wheres $ projection ! User.id' .=. usrId)
    return          $ phTStamp >< phUsrId

-- DELETES
