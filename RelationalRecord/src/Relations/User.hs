{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Relations.Role where

import  Types.User as User
import  Types.DB

import  Database.Relational.Query
-- import  Database.HDBC.Query.TH      (makeRecordPersistableDefault)


allUsers :: Relation () Users
allUsers = User.users

getUser :: Relation Int32 Users
getUser = relation' . placeholder $ \usrId -> do
    a       <- query users
    wheres  $ a ! User.id' .=. usrId
    return  a
