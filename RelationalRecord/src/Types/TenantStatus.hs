{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Types.TenantStatus where

import  Types.DefineTable
import  Prelude hiding (id)


$(defineTable "enum_tenant_status")


-- TODO
-- This Haskell wrapper type is *not* type-safe, conversion might result
-- in a runtime error :(

-- Mapping of Postgres enums to Haskell with Relational Records
-- still needs investigating.

-- possible solution: build a custom TH action, select * from the
-- above defined table, creating the datatype below

data TenantStatus = Inactive | Active | New deriving Enum

toTenantStatus :: EnumTenantStatus -> TenantStatus
toTenantStatus = toEnum . fromIntegral . subtract 1 . id

fromTenantStatus :: TenantStatus -> Int32
fromTenantStatus = fromIntegral . (+1) .fromEnum
