{-# LANGUAGE Arrows, FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, OverloadedStrings #-}

module TenantApi
  (create_tenant
  ,read_tenants
  ,read_tenant_by_id
  ,make_tenant
  )
  where


import           Control.Arrow (returnA, (<<<))
import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye
       (Column, restrict, (.==), (.<=), (.&&), (.<),
       (.===), (.++), Nullable,
        Query, PGInt4, runInsertMany, queryTable, constant,
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
          ,constant $ Just owner_id
          ,pgStrictText bo_domain))

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
  r <- runQuery conn $ (tenant_query_by_id $ constant id)
  return $ case r of
    [] -> Nothing
    rows -> Just $ Prelude.head $ fmap make_tenant rows

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

tenant_query :: Opaleye.Query
 (Column PGInt4, Column P.PGText, Column P.PGText, Column P.PGText,
  Column P.PGText, Column P.PGText, Column P.PGText,
  Column (Nullable PGInt4), Column P.PGText)
tenant_query = queryTable tenantTable

tenant_query_by_id t_id = proc () -> do
  row@ (id, _, _, _, _, _, _, _, _ ) <- tenant_query -< ()
  restrict -< id .== (t_id)
  returnA -< row
