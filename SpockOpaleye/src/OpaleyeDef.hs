module OpaleyeDef
  (
   tenantTable
  ) where

import           Opaleye (Column, Table(Table),
                 required, optional, (.==), (.<),
                 arrangeDeleteSql, arrangeInsertManySql,
                 arrangeUpdateSql, arrangeInsertManyReturningSql,
                 PGInt4, PGFloat8)
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Constant as C
import           Data.Profunctor.Product (p8, p9)

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
                   Maybe (Column PGInt4),
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
                   (Column PGInt4),
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
