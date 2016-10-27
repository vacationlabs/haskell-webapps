{-# LANGUAGE FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, OverloadedStrings #-}

module DomainApi
  (create_tenant
  ,read_tenants)
  where

import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye
       (Column, Query, PGInt4, runInsertMany, queryTable, constant,
        pgStrictText, runQuery)
import qualified Opaleye.PGTypes as P
import GHC.Int
import Data.Text

create_tenant
  :: Connection -> Tenant -> IO GHC.Int.Int64
create_tenant conn Tenant{tenant_id = id,tenant_name = name,tenant_firstname = first_name,tenant_lastname = last_name,tenant_email = email,tenant_phone = phone,tenant_status = status,tenant_ownerid = owner_id,tenant_backofficedomain = bo_domain} = 
  runInsertMany conn tenantTable $
  (return (constant id
          ,pgStrictText name
          ,pgStrictText first_name
          ,pgStrictText last_name
          ,pgStrictText email
          ,pgStrictText phone
          ,constant status
          ,Nothing
          ,pgStrictText bo_domain))

read_tenants
  :: Connection
  -> IO (Maybe [Tenant])
read_tenants conn = do
  r <- runQuery conn $ queryTable tenantTable
  return $ case r of
    [] -> Nothing
    rows -> Just $ fmap make_tenant rows
  where
    make_tenant :: (Int,Text,Text,Text,Text,Text,TenantStatus,Maybe Int,Text) -> Tenant
    make_tenant (id, name, first_name, last_name, email, phone, status, owner_id, bo_domain) = Tenant { 
      tenant_id = id,
      tenant_name = name,
      tenant_firstname = first_name,
      tenant_lastname = last_name,
      tenant_email = email,
      tenant_phone = phone,
      tenant_status = status,
      tenant_ownerid = owner_id,
      tenant_backofficedomain = bo_domain }
