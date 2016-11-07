{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.TenantStatus where

import  Types.DefineTable
import  Prelude hiding (id)


$(defineTable "enum_tenant_status")


-- TODO
-- This Haskell wrapper type is *not* type-safe, conversion might result
-- in a runtime error :(
-- Mapping of Postgres enums to Haskell with Relational Records
-- still needs investigating.

data TenantStatus = TenantInactive | TenantActive | TenantNew deriving Enum

toTenantStatus :: EnumTenantStatus -> TenantStatus
toTenantStatus = toEnum . fromIntegral . subtract 1 . id

fromTenantStatus :: TenantStatus -> Int32
fromTenantStatus = fromIntegral . (+1) .fromEnum
