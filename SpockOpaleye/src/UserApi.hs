{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module UserApi
  ( create_user
  , read_users
  , read_user_by_id
  , read_users_for_tenant
  , add_role_to_user
  , remove_role_from_user
  , update_user
  , remove_user
  , activate_user
  ) where

import Control.Arrow
import Data.Text
import Database.PostgreSQL.Simple (Connection)
import DataTypes
import GHC.Int
import Opaleye
import OpaleyeDef

import CryptoDef

create_user :: Connection -> User -> IO (Maybe User)
create_user conn user@User {user_id = _
                           ,user_tenantid = tenant_id
                           ,user_username = username
                           ,user_password = password
                           ,user_firstname = first_name
                           ,user_lastname = last_name
                           ,user_status = status} = do
  users <-
    runInsertManyReturning
      conn
      userTable
      (return User {
        user_id = Nothing
       ,user_tenantid = constant tenant_id
       ,user_username = pgStrictText username
       ,user_password = constant password
       ,user_firstname = toNullable . pgStrictText <$> first_name
       ,user_lastname = toNullable . pgStrictText <$> last_name
       ,user_status = constant status
       }) id
  return $
    case users of
      [] -> Nothing
      (x:xs) ->
        Just x

update_user :: Connection -> UserId -> User -> IO GHC.Int.Int64
update_user conn (UserId tid) (User {user_id = _
                                    ,user_tenantid = tenant_id
                                    ,user_username = username
                                    ,user_password = password
                                    ,user_firstname = firstname
                                    ,user_lastname = lastname
                                    ,user_status = status}) =
  runUpdate conn userTable update_func match_func
  where
    update_func User { user_id = id } = User {
       user_id = Just id
     , user_tenantid = constant tenant_id
     , user_username = pgStrictText username
     , user_password = constant password
     , user_firstname = toNullable . pgStrictText <$> firstname
     , user_lastname = toNullable . pgStrictText <$> lastname
     , user_status = constant status
    }
    match_func User { user_id = id } = id .== constant tid

activate_user :: Connection -> User -> IO GHC.Int.Int64
activate_user conn user = set_user_status conn user UserStatusActive

deactivate_user :: Connection -> User -> IO GHC.Int.Int64
deactivate_user conn user = set_user_status conn user UserStatusInActive

set_user_status :: Connection -> User -> UserStatus -> IO GHC.Int.Int64
set_user_status conn user@User {user_id = id
                               ,user_tenantid = tid
                               ,user_username = username
                               ,user_password = password
                               ,user_firstname = firstname
                               ,user_lastname = lastname
                               ,user_status = status} new_status =
  update_user
    conn
    id
    User
    { user_id = id
    , user_tenantid = tid
    , user_username = username
    , user_password = password
    , user_firstname = firstname
    , user_lastname = lastname
    , user_status = new_status
    }

remove_user :: Connection -> User -> IO GHC.Int.Int64
remove_user conn User {user_id = tid} =
  runDelete conn userTable match_function
    where
    match_function User { user_id = id } = id .== constant tid

read_users :: Connection -> IO [User]
read_users conn = do
  runQuery conn $ user_query

read_users_for_tenant :: Connection -> TenantId -> IO [User]
read_users_for_tenant conn tenant_id = do
  r <- runQuery conn $ user_query_by_tenantid tenant_id
  return r

read_user_by_id :: Connection -> UserId -> IO (Maybe User)
read_user_by_id conn id = do
  r <- runQuery conn $ user_query_by_id id
  return $
    case r of
      [] -> Nothing
      (x:xs) -> Just x

add_role_to_user :: Connection -> UserId -> RoleId -> IO GHC.Int.Int64
add_role_to_user conn user_id role_id =
  runInsertMany
    conn
    userRolePivotTable
    (return (constant user_id, constant role_id))

remove_role_from_user :: Connection -> UserId -> RoleId -> IO GHC.Int.Int64
remove_role_from_user conn t_user_id t_role_id =
  runDelete
    conn
    userRolePivotTable
    (\(user_id, role_id) ->
        (user_id .== constant t_user_id) .&& (role_id .== constant t_role_id))

make_user
  :: (UserId, TenantId, Text, BcryptPassword, Maybe Text, Maybe Text, UserStatus)
  -> User
make_user (id, tenant_id, name, password, first_name, last_name, status) =
  User
  { user_id = id
  , user_tenantid = tenant_id
  , user_username = name
  , user_password = password
  , user_firstname = first_name
  , user_lastname = last_name
  , user_status = status
  }

user_query :: Query UserTableR
user_query = queryTable userTable

user_query_by_id :: UserId -> Query UserTableR
user_query_by_id t_id =
  proc () ->
  do row@User{user_id = id} <- user_query -< ()
     restrict -< id .== (constant t_id)
     returnA -< row

user_query_by_tenantid :: TenantId -> Query UserTableR
user_query_by_tenantid t_tenantid =
  proc () ->
  do row@User {user_tenantid = tenant_id} <- user_query -< ()
     restrict -< tenant_id .== (constant t_tenantid)
     returnA -< row
