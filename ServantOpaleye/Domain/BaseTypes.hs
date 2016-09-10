{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Domain.BaseTypes where
import Control.Monad.Identity
import DB
import Data.Text
import Data.Time

-- TODO: Figure out the right monad-transformed stack for the domain API. We'll have to do the following:
-- * DB operations
-- * Logging
-- * Redis operations, potentitally
type App = Identity

type Tenant = TenantPoly
  TenantId -- key
  UTCTime -- createdAt
  UTCTime -- updatedAt
  Text -- name
  TenantStatus -- status
  (Maybe UserId) -- ownerId
  Text -- backofficeDomain


-- data TenantWithStatus status = Tenant
