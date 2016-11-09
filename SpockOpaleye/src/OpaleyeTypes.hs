module OpaleyeTypes where

import Opaleye
import DataTypes

type TenantTableW = TenantPoly
  (Maybe (Column PGInt4))
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Maybe (Column PGTimestamptz)) -- updatedAt
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Maybe (Column PGText))
  (Maybe (Column (Nullable PGInt4)))
  (Column PGText)

type TenantTableR = TenantPoly
  (Column PGInt4)
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column (Nullable PGInt4))
  (Column PGText)

type UserTableW = UserPoly
  (Maybe (Column PGInt4))
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Maybe (Column PGTimestamptz)) -- updatedAt
  (Column PGInt4)
  (Column PGText)
  (Column PGBytea)
  (Maybe (Column (Nullable PGText)))
  (Maybe (Column (Nullable PGText)))
  (Maybe (Column PGText))

type UserTableR = UserPoly
  (Column PGInt4)
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGInt4)
  (Column PGText)
  (Column PGBytea)
  (Column (Nullable PGText))
  (Column (Nullable PGText))
  (Column PGText)

type RoleTableW = RolePoly
  (Maybe (Column PGInt4))
  (Column PGInt4)
  (Column PGText)
  (Column (PGArray PGText))
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Maybe (Column PGTimestamptz)) -- updatedAt

type RoleTableR = RolePoly
  (Column PGInt4)
  (Column PGInt4)
  (Column PGText)
  (Column (PGArray PGText))
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt
