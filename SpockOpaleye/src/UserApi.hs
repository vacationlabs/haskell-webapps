{-# LANGUAGE Arrows, FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, OverloadedStrings #-}

module UserApi
  (
   create_user,
   read_users,
   read_user_by_id
  )
  where

import Control.Arrow (returnA, (<<<))
import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye
       (Column, restrict, (.==), (.<=), (.&&), (.<),
       (.===), (.++), Nullable,
        Query, PGInt4, runInsertMany, queryTable, constant,
        pgStrictText, runQuery)
import Opaleye.PGTypes
import GHC.Int
import Data.Text

create_user
  :: Connection -> User -> IO GHC.Int.Int64
create_user conn User{user_id = id, user_tenantid = tenant_id, user_username = username, user_firstname = first_name, user_lastname = last_name, user_status = status} = 
  runInsertMany conn userTable $
  (return (constant id
          ,constant tenant_id
          ,pgStrictText username
          ,pgStrictText username
          ,constant $ Just first_name
          ,constant $ Just last_name
          ,constant status
          ))

read_users
  :: Connection
  -> IO (Maybe [User])
read_users conn = do
  r <- runQuery conn $ user_query
  return $ case r of
    [] -> Nothing
    rows -> Just $ fmap make_user rows

read_user_by_id
  :: Connection
  -> Int
  -> IO (Maybe User)
read_user_by_id conn id = do
  r <- runQuery conn $ user_query_by_id id
  return $ case r of
    [] -> Nothing
    rows -> Just $ Prelude.head $ fmap make_user rows

make_user  :: (Int, Int, Text, Text, Maybe Text, Maybe Text, UserStatus) -> User
make_user (id, tenant_id, name, password, first_name, last_name, status) = User {
  user_id = id,
  user_tenantid = tenant_id,
  user_username = name,
  user_password = password,
  user_firstname = first_name,
  user_lastname = last_name,
  user_status = status }

user_query :: Query (Column PGInt4, Column PGInt4, Column PGText, Column PGText, Column (Nullable PGText), Column (Nullable PGText), Column PGText)
user_query = queryTable userTable

user_query_by_id :: Int -> Query (Column PGInt4, Column PGInt4, Column PGText, Column PGText, Column (Nullable PGText), Column (Nullable PGText), Column PGText)
user_query_by_id t_id = proc () -> do
  row@ (id, _, _, _, _, _, _) <- user_query -< ()
  restrict -< id .== (constant t_id)
  returnA -< row
