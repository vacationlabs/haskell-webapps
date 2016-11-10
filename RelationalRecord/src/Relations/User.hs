{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Relations.User where

import  Types.User as User
import  Types.DB

import  Database.Relational.Query
import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)


allUsers :: Relation () Users
allUsers = User.users

getUser :: Relation PKey Users
getUser = relation' . placeholder $ \usrId -> do
    a       <- query users
    wheres  $ a ! User.id' .=. usrId
    return  a

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

instance HasTableName (Insert UserInsert) where
    getTableName = const User.tableName
