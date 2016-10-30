{-# LANGUAGE Arrows, FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, OverloadedStrings #-}

module TenantApi
  (create_tenant
  ,read_tenants
  ,read_tenant_by_id
  ,make_tenant
  ,remove_tenant
  )
  where


import           Control.Arrow (returnA, (<<<))
import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye
       (Column, PGText, restrict, (.==), (.<=), (.&&), (.<),
       (.===), (.++), Nullable,
        Query, PGInt4, runInsertMany, runDelete, runInsertManyReturning, queryTable, constant,
        pgStrictText, runQuery)
import qualified Opaleye.PGTypes
import GHC.Int
import Data.Text
import UserApi
import RoleApi

create_tenant
  :: Connection -> Tenant -> IO [Int]
create_tenant conn Tenant{tenant_id = id,tenant_name = name,tenant_firstname = first_name,tenant_lastname = last_name,tenant_email = email,tenant_phone = phone,tenant_status = status,tenant_ownerid = owner_id,tenant_backofficedomain = bo_domain} = 
  runInsertManyReturning conn tenantTable
  (return (Nothing
          ,pgStrictText name
          ,pgStrictText first_name
          ,pgStrictText last_name
          ,pgStrictText email
          ,pgStrictText phone
          ,constant status
          ,Just $ constant owner_id
          ,pgStrictText bo_domain)) (\(id, _, _, _, _, _, _, _, _) -> id)

remove_tenant :: Connection -> Tenant -> IO GHC.Int.Int64
remove_tenant conn Tenant {tenant_id = Just tid } = do
  users_for_tenant <- read_users_for_tenant conn tid
  roles_for_tenant <- read_roles_for_tenant conn tid
  mapM_ (remove_user conn) users_for_tenant
  mapM_ (remove_role conn) roles_for_tenant
  runDelete conn tenantTable (\(id, _, _, _, _, _, _, _, _) -> id .== (constant tid)) 

read_tenants
  :: Connection
  -> IO (Maybe [Tenant])
read_tenants conn = do
  r <- runQuery conn $ tenant_query
  return $ case r of
    [] -> Nothing
    rows -> Just $ fmap make_tenant rows

read_tenant_by_id
  :: Connection
  -> Int
  -> IO (Maybe Tenant)
read_tenant_by_id conn id = do
  r <- runQuery conn $ (tenant_query_by_id id)
  return $ case r of
    [] -> Nothing
    rows -> Just $ Prelude.head $ fmap make_tenant rows

make_tenant :: (Int,Text,Text,Text,Text,Text,TenantStatus,Maybe Int,Text) -> Tenant
make_tenant (id, name, first_name, last_name, email, phone, status, owner_id, bo_domain) = Tenant { 
  tenant_id = Just id,
  tenant_name = name,
  tenant_firstname = first_name,
  tenant_lastname = last_name,
  tenant_email = email,
  tenant_phone = phone,
  tenant_status = status,
  tenant_ownerid = owner_id,
  tenant_backofficedomain = bo_domain }

tenant_query :: Opaleye.Query
 (Column PGInt4, Column PGText, Column PGText, Column PGText,
  Column PGText, Column PGText, Column PGText,
  Column (Nullable PGInt4), Column PGText)
tenant_query = queryTable tenantTable

tenant_query_by_id :: Int -> Opaleye.Query
 (Column PGInt4, Column PGText, Column PGText, Column PGText,
  Column PGText, Column PGText, Column PGText,
  Column (Nullable PGInt4), Column PGText)
tenant_query_by_id t_id = proc () -> do
  row@ (id, _, _, _, _, _, _, _, _ ) <- tenant_query -< ()
  restrict -< id .== (constant t_id)
  returnA -< row
