{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}


module Validations where

import           Control.Lens
import qualified Data.Text                  as T
import           DataTypes
import           TenantApi

validateIncomingTenant :: TenantIncoming -> AppM ValidationResult
validateIncomingTenant tenant = do
  unique_bod <- check_for_unique_bo_domain (tenant ^. backofficedomain)
  let result = do 
                unique_bod
                if validate_contact then Right () 
                  else (Left "Firstname, Lastname, Email, Phone cannot be blank")
                if validate_name then Right () 
                  else (Left "Name cannot be blank")
  return $ case result of
    Right () -> Valid
    Left err -> Invalid err
  where
    validate_contact = and $ (>= 0) . T.length <$> [tenant ^. firstname, tenant ^. lastname, tenant ^. email, tenant ^. phone]
    validate_name = (T.length $ tenant ^. name) >= 3
    check_for_unique_bo_domain :: T.Text -> AppM (Either String ())
    check_for_unique_bo_domain domain = v <$> readTenantByBackofficedomain domain
      where
        v :: Maybe Tenant -> Either String ()
        v (Just _) = Left "Duplicate backoffice domain"
        v _  = Right ()
