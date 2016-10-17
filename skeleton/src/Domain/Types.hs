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

type SessionID = Text
