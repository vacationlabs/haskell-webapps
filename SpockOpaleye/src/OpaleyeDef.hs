module OpaleyeDef
  (
    tenantTable
   ,userTable
  ) where

import           Opaleye (Column, Table(Table), Nullable,
                 required, optional, (.==), (.<),
                 arrangeDeleteSql, arrangeInsertManySql,
                 arrangeUpdateSql, arrangeInsertManyReturningSql,
                 PGInt4, PGFloat8)
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Constant as C
import           Data.Profunctor.Product (p6, p7, p8, p9)

import DataTypes

tenantTable :: Table
                  (
                   Column PGInt4,
                   Column P.PGText,
                   Column P.PGText,
                   Column P.PGText,
                   Column P.PGText,
                   Column P.PGText,
                   Column P.PGText,
                   Maybe (Column (Nullable PGInt4)),
                   Column P.PGText
                   )
                  (
                   Column PGInt4,
                   Column P.PGText,
                   Column P.PGText,
                   Column P.PGText,
                   Column P.PGText,
                   Column P.PGText,
                   Column P.PGText,
                   Column (Nullable PGInt4),
                   Column P.PGText
                   )
tenantTable = Table "tenants" (p9 ( 
                              required "id",
                              required "name",
                              required "first_name",
                              required "last_name",
                              required "email",
                              required "phone",
                              required "status",
                              optional "owner_id",
                              required "backoffice_domain"))

userTable :: Table
    (
     Column PGInt4,
     Column PGInt4,
     Column P.PGText,
     Column P.PGText,
     Maybe (Column (Nullable P.PGText)),
     Maybe (Column (Nullable P.PGText)),
     Column P.PGText
    )
    (
     Column PGInt4,
     Column PGInt4,
     Column P.PGText,
     Column P.PGText,
     (Column (Nullable P.PGText)),
     (Column (Nullable P.PGText)),
     Column P.PGText
    )
userTable = Table "users" (p7 (
      required "id",
      required "tenant_id",
      required "username",
      required "password",
      optional "first_name",
      optional "last_name",
      required "status"))
