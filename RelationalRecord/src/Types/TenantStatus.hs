{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}


module  Types.TenantStatus where

import  Helpers.DefineEnum

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineEnum "tenant_status")

deriving instance Generic TenantStatus
instance ToJSON TenantStatus
