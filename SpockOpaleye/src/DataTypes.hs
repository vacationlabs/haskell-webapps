{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module DataTypes 
  (
   Tenant(..),
   TenantStatus(..)
  ) where

import Data.Text

import qualified Data.Profunctor.Product.Default as D
import Opaleye (Constant(Constant), PGText, Column, pgString)

data TenantStatus = TenantStatusActive | TenantStatusInActive | TenantStatusNew

data Tenant = Tenant {
  tenant_id::Int,
  tenant_name::String,
  tenant_firstname::String,
  tenant_lastname::String,
  tenant_email::String,
  tenant_phone::String,
  tenant_status::TenantStatus,
  tenant_ownerid::Maybe Int,
  tenant_backofficedomain :: String
}

instance D.Default Constant TenantStatus (Column PGText) where
  def = Constant def'
    where 
    def' :: TenantStatus -> (Column PGText)
    def' TenantStatusInActive = pgString "inactive"
    def' TenantStatusActive = pgString "active"
    def' TenantStatusNew = pgString "new"
