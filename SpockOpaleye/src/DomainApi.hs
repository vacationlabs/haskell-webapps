module DomainApi
  (create_tenant)
  where

import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye (runInsertMany, constant, pgStrictText)
import GHC.Int

create_tenant
  :: Connection -> Tenant -> IO GHC.Int.Int64
create_tenant conn Tenant{tenant_id = id,tenant_name = nm,tenant_firstname = fn,tenant_lastname = ln,tenant_email = email,tenant_phone = phone,tenant_status = status,tenant_ownerid = owner_id,tenant_backofficedomain = bo_domain} = 
  runInsertMany conn tenantTable $
  (return (constant id
          ,pgStrictText nm
          ,pgStrictText fn
          ,pgStrictText ln
          ,pgStrictText email
          ,pgStrictText phone
          ,constant status
          ,Nothing
          ,pgStrictText bo_domain))
