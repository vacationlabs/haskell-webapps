{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
  OverloadedStrings #-}

module DomainApi
  (create_tenant, read_tenants)
  where

import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye (Column, Query, PGInt4, runInsertMany, queryTable, constant, pgStrictText, runQuery)
import qualified Opaleye.PGTypes as P
import GHC.Int
import Data.Text


create_tenant
  :: Connection -> Tenant -> IO GHC.Int.Int64
create_tenant conn Tenant{
  tenant_id = id,
  tenant_name = name,
  tenant_firstname = first_name,
  tenant_lastname = last_name,
  tenant_email = email,
  tenant_phone = phone,
  tenant_status = status,
  tenant_ownerid = owner_id,
  tenant_backofficedomain = bo_domain} = 
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

read_tenants :: Connection -> IO [(Int, Text, Text, Text, Text, Text, TenantStatus, Maybe Int, Text)]
read_tenants conn = runQuery conn $ queryTable tenantTable
