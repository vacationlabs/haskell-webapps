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

create_user :: Connection -> User -> IO [Int]
create_user conn User {user_id = id
                      ,user_tenantid = tenant_id
                      ,user_username = username
                      ,user_firstname = first_name
                      ,user_lastname = last_name
                      ,user_status = status} =
  runInsertManyReturning
    conn
    userTable
    (return
       ( Nothing
       , constant tenant_id
       , pgStrictText username
       , pgStrictText username
       , toNullable . pgStrictText <$> first_name
       , toNullable . pgStrictText <$> last_name
       , constant status))
    (\(id_, _, _, _, _, _, _) -> id_)

update_user :: Connection -> Int -> User -> IO GHC.Int.Int64
update_user conn tid (User {user_id = id
                           ,user_tenantid = tenant_id
                           ,user_username = username
                           ,user_password = password
                           ,user_firstname = firstname
                           ,user_lastname = lastname
                           ,user_status = status}) =
  runUpdate
    conn
    userTable
    (\(id, _, _, _, _, _, _) ->
        ( Just id
        , constant tenant_id
        , pgStrictText username
        , pgStrictText password
        , toNullable . pgStrictText <$> firstname
        , toNullable . pgStrictText <$> lastname
        , constant status))
    (\(id, _, _, _, _, _, _) -> (id .== constant tid))

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
  runDelete conn userTable (\(id, _, _, _, _, _, _) -> id .== (constant tid))

read_users :: Connection -> IO [User]
read_users conn = do
  r <- runQuery conn $ user_query
  return $ make_user <$> r

read_users_for_tenant :: Connection -> Int -> IO [User]
read_users_for_tenant conn tenant_id = do
  r <- runQuery conn $ user_query_by_tenantid tenant_id
  return $ make_user <$> r

read_user_by_id :: Connection -> Int -> IO (Maybe User)
read_user_by_id conn id = do
  r <- runQuery conn $ user_query_by_id id
  return $
    case r of
      [] -> Nothing
      rows -> Just $ Prelude.head $ make_user <$> rows

add_role_to_user :: Connection -> Int -> Int -> IO GHC.Int.Int64
add_role_to_user conn user_id role_id =
  runInsertMany
    conn
    userRolePivotTable
    (return (constant user_id, constant role_id))

remove_role_from_user :: Connection -> Int -> Int -> IO GHC.Int.Int64
remove_role_from_user conn t_user_id t_role_id =
  runDelete
    conn
    userRolePivotTable
    (\(user_id, role_id) ->
        (user_id .== constant t_user_id) .&& (role_id .== constant t_role_id))

make_user :: (Int, Int, Text, Text, Maybe Text, Maybe Text, UserStatus) -> User
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

user_query_by_id :: Int -> Query UserTableR
user_query_by_id t_id =
  proc () ->
  do row@(id, _, _, _, _, _, _) <- user_query -< ()
     restrict -< id .== (constant t_id)
     returnA -< row

user_query_by_tenantid :: Int -> Query UserTableR
user_query_by_tenantid t_tenantid =
  proc () ->
  do row@(_, tenant_id, _, _, _, _, _) <- user_query -< ()
     restrict -< tenant_id .== (pgInt4 t_tenantid)
     returnA -< row
