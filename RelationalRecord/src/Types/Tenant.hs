{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

{-# LANGUAGE FlexibleContexts #-}

module  Types.Tenant where

import  Types.DefineTable
import  Prelude hiding (id)

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)

$(defineTable "tenants")


instance HasAuditMetadata Tenants where
    getAuditMetadata t = (id t, "tenants")


deriving instance Generic Tenants
instance ToJSON Tenants
