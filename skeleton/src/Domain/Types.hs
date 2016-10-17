{-# LANGUAGE TemplateHaskell #-}
module Types where

import Import

data AppConfig = AppConfig { dbPool :: Text
                           , redisPool :: Text
                           }
type AppM = ReaderT AppConfig

type TenantID = Integer
data Tenant = Tenant{}

type UserID = Integer
data User = User{}

type ProductID = Integer
data Product = Product{}

type VariantID = Integer
data Variant = Variant{}

type PhotoID = Integer
data Photo = Photo{}

type RoleID = Integer
data Role = Role{}

type AuditLogID = Integer
data AuditLog = AuditLog{}

type SessionID = Text

-- LENSES
$(makeClassy ''AppConfig)
$(makeClassy ''Tenant)
$(makeClassy ''User)
$(makeClassy ''Role)
$(makeClassy ''Product)
$(makeClassy ''Variant)
$(makeClassy ''Photo)
$(makeClassy ''AuditLog)
