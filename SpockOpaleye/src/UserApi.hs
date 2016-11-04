{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

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

import           Control.Arrow
import           Data.Text
import           Database.PostgreSQL.Simple (Connection)
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef

import           CryptoDef

create_user :: Connection -> User -> IO User
create_user conn user = Prelude.head <$> runInsertManyReturning conn userTable [constant user] id

update_user :: Connection -> UserId -> User -> IO User
update_user conn user_id user = do
  runUpdate conn userTable update_func match_func
  return user
  where
    update_func :: UserTableR -> UserTableW
    update_func _ = constant user
    match_func :: UserTableR -> Column PGBool
    match_func User { user_id = id } = id .== constant user_id

activate_user :: Connection -> User -> IO User
activate_user conn user = set_user_status conn user UserStatusActive

deactivate_user :: Connection -> User -> IO User
deactivate_user conn user = set_user_status conn user UserStatusInActive

set_user_status :: Connection -> User -> UserStatus -> IO User
set_user_status conn user new_status = update_user conn (user_id user) user { user_status = new_status }
remove_user :: Connection -> User -> IO GHC.Int.Int64
remove_user conn User {user_id = tid} =
  runDelete conn userTable match_function
    where
    match_function User { user_id = id } = id .== constant tid

read_users :: Connection -> IO [User]
read_users conn = runQuery conn user_query

read_users_for_tenant :: Connection -> TenantId -> IO [User]
read_users_for_tenant conn tenant_id = runQuery conn $ user_query_by_tenantid tenant_id

read_user_by_id :: Connection -> UserId -> IO (Maybe User)
read_user_by_id conn id = do
  r <- runQuery conn $ user_query_by_id id
  return $ case r of
    []     -> Nothing
    (x:xs) -> Just x

add_role_to_user :: Connection -> UserId -> RoleId -> IO GHC.Int.Int64
add_role_to_user conn user_id role_id =
  runInsertMany conn userRolePivotTable (return (constant user_id, constant role_id))

remove_role_from_user :: Connection -> UserId -> RoleId -> IO GHC.Int.Int64
remove_role_from_user conn t_user_id t_role_id = runDelete conn userRolePivotTable
    (\(user_id, role_id) -> (user_id .== constant t_user_id) .&& (role_id .== constant t_role_id))

user_query :: Query UserTableR
user_query = queryTable userTable

user_query_by_id :: UserId -> Query UserTableR
user_query_by_id t_id = proc () -> do
  row@User{user_id = id} <- user_query -< ()
  restrict -< id .== (constant t_id)
  returnA -< row

user_query_by_tenantid :: TenantId -> Query UserTableR
user_query_by_tenantid t_tenantid = proc () -> do
  row@User {user_tenantid = tenant_id} <- user_query -< ()
  restrict -< tenant_id .== (constant t_tenantid)
  returnA -< row
