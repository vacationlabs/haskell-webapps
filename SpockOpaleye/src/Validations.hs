{-# LANGUAGE OverloadedStrings #-}

module Validations where

import           Data.Maybe
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple
import           DataTypes
import           TenantApi

validateIncomingTenant :: Connection -> TenantIncoming -> IO ValidationResult
validateIncomingTenant conn tenant = return Valid
--validateIncomingTenant conn tenant@Tenant {tenant_name = name
--                                          ,tenant_firstname = fn
--                                          ,tenant_lastname = ln
--                                          ,tenant_email = em
--                                          ,tenant_phone = phone
--                                          ,tenant_backofficedomain = bo_domain} = do
--  unique_bod <- check_for_unique_bo_domain
--  return $
--    if and [unique_bod, validate_name, validate_contact]
--      then Valid
--      else Invalid
--  where
--    validate_contact = and $ (>= 0) . T.length <$> [fn, ln, em, phone]
--    validate_name = (T.length name) >= 3
--    check_for_unique_bo_domain =
--      isNothing <$> read_tenant_by_backofficedomain conn bo_domain
