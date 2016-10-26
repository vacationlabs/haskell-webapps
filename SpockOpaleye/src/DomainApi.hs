{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DomainApi
    ( 
     create_tenant,
     get_tenant
    ) where


import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye (runInsertMany, constant, pgString)
import GHC.Int

get_tenant = Tenant {
tenant_id=1, tenant_name="name", tenant_firstname = "fn", tenant_lastname = "ln", tenant_email = "t_em",tenant_phone = "", tenant_status = TenantStatusInActive, tenant_ownerid=Nothing, tenant_backofficedomain=""}

create_tenant :: Connection -> Tenant -> IO GHC.Int.Int64
create_tenant conn Tenant {
  tenant_id=id,
  tenant_name = nm,
  tenant_firstname = fn,
  tenant_lastname = ln,
  tenant_email = email,
  tenant_phone = phone,
  tenant_status = status,
  tenant_ownerid = owner_id,
  tenant_backofficedomain = bo_domain 
} = runInsertMany conn tenantTable $ (return (constant id, pgString nm, pgString fn,pgString ln, pgString email, pgString phone, constant status, Nothing,  pgString bo_domain))
