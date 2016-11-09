{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}


module Validations where

import Control.Lens
import           DataTypes
import           Data.Maybe
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple
import           TenantApi

validateIncomingTenant :: Connection -> TenantIncoming -> IO ValidationResult
validateIncomingTenant conn tenant = do
  unique_bod <- check_for_unique_bo_domain (tenant ^. backofficedomain)
  return $
    if and [unique_bod, validate_name, validate_contact]
      then Valid
      else Invalid
  where
    validate_contact = and $ (>= 0) . T.length <$> [tenant ^. firstname, tenant ^. lastname, tenant ^. email, tenant ^. phone]
    validate_name = (T.length $ tenant ^. name) >= 3
    check_for_unique_bo_domain domain =
      isNothing <$> read_tenant_by_backofficedomain conn domain
