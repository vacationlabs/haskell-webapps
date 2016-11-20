
module  Types.Enum where

-- TODO
-- This Haskell wrapper type is *not* type-safe, conversion might result
-- in a runtime error :(

-- Mapping of Postgres enums to Haskell with Relational Records
-- still needs investigating.

-- possible solution: build a custom TH action, select * from the
-- above defined table, creating the datatype below

data TestEnum = Inactive | Active | New deriving (Show, Eq)

{-
toTenantStatus :: TestEnum -> TenantStatus
toTenantStatus = toEnum . fromIntegral . subtract 1 . id

fromTenantStatus :: TenantStatus -> Int32
fromTenantStatus = fromIntegral . (+1) .fromEnum
-}
